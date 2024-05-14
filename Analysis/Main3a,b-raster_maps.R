#' Produce geographical map of local music proportion and socio-cultural values in Ukraine and Russia.
#' Related to Fig.3 in paper

# load packages and methods
source('utils.R')
require(forcats) # easier factor handling
require(lintr) # code linting
require(sf) # spatial data handling
require(rnaturalearth) # country borders geometries from naturalearth.org


################################################################################
## SETTINGS
################################################################################
type <- 'wvs'        # 'music' for local music proportion change and 'wvs' for socio-cultural values PCA loadings
pc <- 'PC1'          # which PC to use for the plotting. Only relevant when type = 'wvs'
resolution <- 50     # 100 is minimal, 500 and higher is very fine-grained resolution but takes a long time
country <- 'UA'      # UA or RU
nquantile <- 4       # number of quantile to split to reduce noise
global <- TRUE       # if TRUE load global WVS data, if FALSE, use separate PCA results per country

# fixate the coordinate system
crs <-  "+proj=longlat +datum=WGS84"  

# parameters for Kernel smoothing
Dx = 0.1
l = 1


################################################################################
## DATA
################################################################################
# depending on the country, load different variables
switch(country,
       UA = {
         cities <- switch(type, 
                music = {read_rds('Dataset/lyrics/ukranian_lyrics_change_by_city.rds')},
                wvs = {read_rds('Dataset/wvs/UA_wvs_pca_loadings.rds')})
         where <- 'ukraine'
         colour_gradient <- c(UKRAINE_COLOUR, UKRAINE_COLOUR2)
       },
       RU = {
         cities <- switch(type, 
                          music = {read_rds('Dataset/lyrics/russian_lyrics_change_by_city.rds')},
                          wvs = {read_rds('Dataset/wvs/RU_wvs_pca_loadings.rds')})
         where <- 'russia'
         colour_gradient <- c(RUSSIAN_COLOUR2, RUSSIAN_COLOUR)
       }
)

# if using global loadings, override with global PCA results
if(global){cities <- read_rds('Dataset/wvs/global_wvs_pca_loadings.rds')}

# load world map polygon
worldmap <- read_rds('Dataset/census/world_map.rds')

# load cities level meta data
city_meta <- read_rds('Dataset/meta/city_metadata.rds') %>% 
  select(nodeID, country_code, country_name, city_name, fcode, lat, lon)

# join data
joined_data <- cities %>% left_join(city_meta) %>% ungroup()

# make quantiles for the variable of interest
joined_data <- joined_data %>%
  filter(country_code == country) %>%
  {
    if(type == "music") {
      mutate(., abs_percent_change = abs(pre_post_change / sum(pre + post)) * 100) %>%
        mutate(value = ntile(abs_percent_change, nquantile))
    } else if(type == "wvs") {
      mutate(., value = ntile(!!sym(pc), nquantile))
    }
  }


################################################################################
## MAPPING FUNCTIONS
################################################################################
# clean up the points to plot on map
prepare_plotting_data <- function(df, viewpoint, buffer = NULL){
  # load map polygon data
  sf_use_s2(FALSE)
  
  # Define world regions to draw the map
  sf_map <- worldmap %>% st_as_sf()
  
  # transform map to target coordinates
  map <- sf_map %>% 
    st_transform(crs)
  
  map <- switch (viewpoint,
                 ukraine = {map %>% 
                     # include crimean regions
                     filter(iso_a2 == 'UA' | iso_3166_2 %in% c('UA-43', 'UA-40'))}, 
                 russia = {map %>% 
                     # exclude crimean regions
                     filter(iso_a2 == 'RU', !iso_3166_2 %in% c('UA-43', 'UA-40')) %>%
                     st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62)
                 }
  )
  
  # Define the points to plot on the map from data
  point_on_map <- df %>%
    select(city_name, fcode, country_code, change = value, lon, lat) %>%
    as_tibble()
  
  # convert point data into coordinates
  point_dt <- point_on_map %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = crs)
  
  point_dt <- switch (viewpoint,
                      ukraine = {point_dt %>% filter(country == 'UA')},
                      russia = {point_dt %>% 
                          filter(country == 'RU') %>%
                          st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62)
                      }
                      
  )
  
  
  # Take points that are only inside the map
  point_dt <- point_dt[map, ]
  
  return(list(map, point_dt))
}

# make a grid defining the grid in which the KDE estimate will be computed
make_raster_grid <- function(buffered_df, width_pixel = 100){
  # # dx is the width of a grid cell in meters
  dx <- (st_bbox(buffered_df)["xmax"] -
           st_bbox(buffered_df)["xmin"]) / width_pixel
  # dy is the height of a grid cell in meters because we use quadratic grid cells, dx == dy
  dy <- dx
  # calculate the height in pixels of the resulting grid
  height_pixel <- (st_bbox(buffered_df)["ymax"] - 
                     st_bbox(buffered_df)["ymin"]) / dy
  
  grid <- st_make_grid(buffered_df, 
                       cellsize = dx,
                       n = c(width_pixel, height_pixel),
                       what = "centers")
  
  return(grid)
}

# equations for kernel smoothing
smoother <- function(x, D, l){
  exp(-x^l/D^l)
}
normalizer <- function(x){
  x / sum(x)
}

# looper function to iterate over the grid and do kernel smoothing
perform_smoothing <- function(Dx = 0.1, l = 2){
  # make dummy results
  results <- data.frame(value = NA, 
                        lon = st_coordinates(grid_dt)[, 1], 
                        lat = st_coordinates(grid_dt)[, 2])
  
  # coordinate point im working on
  raw_values <- city_data$change
  coord_rest <- city_data %>% select(lon, lat)
  
  # calculate D0 by computing the mean
  D0 <- coord_rest %>% dist() %>% mean()
  
  # iterate over the cell
  for (i in 1:nrow(results)) {
    coord_dist = c()
    for (j in 1:nrow(city_data)) {
      coord1 <- results[i, 2:3]
      coord2 <- coord_rest[j, ]
      coord_dist <- c(coord_dist, dist(rbind(coord1, coord2))[1])
    }
    coord_weight <- coord_dist %>% smoother(D = D0 * Dx, l = l) %>% normalizer()
    weighted_value <- sum(raw_values * coord_weight)
    results$value[i] <- weighted_value
    
    print(sprintf('%s/%s', i, nrow(results)))
  }
  
  return(results)
}

# make raster map
make_raster <- function(smoothed_df,map_df){
  # convert resulting df back to sf object, but do not remove raw geometry cols
  raster <- st_as_sf(
    smoothed_df,
    coords = c("lon", "lat"),
    crs = crs,
    remove = F
  )
  # clip raster 
  output <- raster[map_df, ]
  
  return(output)
}


################################################################################
## DRAWING THE MAP 
################################################################################
# Get location of the cities and map outline
plotDT <- prepare_plotting_data(joined_data, viewpoint = where)
map <- plotDT[[1]]
point_dt <- plotDT[[2]]

# make grid to iterate over the raster
grid_dt <- make_raster_grid(map, width_pixel = resolution)
message(sprintf('The raster grid has %s points to fill', length(grid_dt)))

# perform kernel smoothing by iterating over the points
city_data <- data.frame(
  city_name = point_dt$city_name,
  change = point_dt$change,
  fcode = point_dt$fcode,
  lon = st_coordinates(point_dt)[, 1],
  lat = st_coordinates(point_dt)[, 2]
)

smoothed_grid <- perform_smoothing(Dx, l)

# add geometry and lines for the map
map_geometry <- map$geometry[!st_is_empty(map$geometry), drop=FALSE]
map_outline <- as_Spatial(map_geometry) %>% fortify()

# make raster map to plot
raster_dt <- make_raster(smoothed_grid, map)

# add metadata to define which are capital cities
point_data <- city_data %>% 
  mutate(city_category = ifelse(!is.na(fcode) & fcode == 'PPLC', 'capital', 'non-capital')) %>%
  mutate(city_category = factor(city_category, levels = c('non-capital', 'capital')))

# plot the map
ggplot() +
  geom_raster(data = raster_dt, aes(x = lon, y = lat, fill = value)) +
  geom_path(data = map_outline, aes(x = long, y = lat, group = group),
            colour = "gray20", size = 0.1) +
  labs(title = ifelse(type == 'wvs', pc, ''), fill = '') +
  geom_point(data = point_data, aes(x = lon, y = lat, shape = city_category), 
             colour = ifelse(country == 'UA', 'black', 'white'), alpha = 1, size = 1.5) +
  switch(type,
         music = {
           scale_fill_gradient(low = colour_gradient[1], high = colour_gradient[2])
         },
         wvs = {
           scale_fill_gradient(low = colour_gradient[1], high = colour_gradient[2])
         }) +
  scale_shape_manual(values = c('cross', 'square'), guide = F) +
  theme_map()

# save plot for main figure if PC3
if(type == 'wvs' & pc == 'PC3') {
  plot_save(sprintf('Main/%s_%s_%s_map', country, type, pc), c(180, 120))
} else {
  plot_save(sprintf('SI/%s_%s_%s_map', country, type, pc), c(180, 120))
}

