#' Produce geographical maps of Inglehart-Welzel dimensions in Ukraine and Russia
#' Additive analysis parallel to the PCA-based WVS maps

source('utils.R')
require(forcats)
require(sf)

################################################################################
## SETTINGS
################################################################################
resolution <- 200                # 100 is minimal, 500+ is very fine-grained but slow
nquantile <- 4                   # number of quantiles to split to reduce noise
country <- 'RU'
dimension <- 'trad_secular'      # 'trad_secular' or 'survival_selfexpr'

crs <- "+proj=longlat +datum=WGS84"
Dx = 0.1
l = 1

################################################################################
## MAPPING FUNCTIONS
################################################################################
prepare_plotting_data <- function(df, viewpoint, worldmap) {
  sf_use_s2(FALSE)
  sf_map <- worldmap %>% st_as_sf()

  map <- sf_map %>% st_transform(crs)
  map <- switch(
    viewpoint,
    ukraine = {
      map %>% filter(iso_a2 == 'UA' | iso_3166_2 %in% c('UA-43', 'UA-40'))
    },
    russia = {
      map %>% filter(iso_a2 == 'RU', !iso_3166_2 %in% c('UA-43', 'UA-40')) %>%
        st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62)
    }
  )

  point_on_map <- df %>%
    select(city_name, fcode, country_code, change = value, lon, lat) %>%
    as_tibble()

  point_dt <- point_on_map %>%
    st_as_sf(coords = c("lon", "lat"), crs = crs)

  point_dt <- switch(
    viewpoint,
    ukraine = { point_dt %>% filter(country_code == 'UA') },
    russia = { point_dt %>% filter(country_code == 'RU') %>% st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62) }
  )

  point_dt <- point_dt[map, ]
  list(map, point_dt)
}

make_raster_grid <- function(buffered_df, width_pixel = 100) {
  dx <- (st_bbox(buffered_df)["xmax"] - st_bbox(buffered_df)["xmin"]) / width_pixel
  dy <- dx
  height_pixel <- (st_bbox(buffered_df)["ymax"] - st_bbox(buffered_df)["ymin"]) / dy
  st_make_grid(buffered_df, cellsize = dx, n = c(width_pixel, height_pixel), what = "centers")
}

smoother <- function(x, D, l) {
  exp(-x^l / D^l)
}

normalizer <- function(x) {
  x / sum(x)
}

perform_smoothing <- function(grid_dt, city_data, Dx = 0.1, l = 2) {
  results <- data.frame(value = NA, lon = st_coordinates(grid_dt)[, 1], lat = st_coordinates(grid_dt)[, 2])
  raw_values <- city_data$change
  coord_rest <- city_data %>% select(lon, lat)
  D0 <- coord_rest %>% dist() %>% mean()

  for (i in 1:nrow(results)) {
    coord_dist = c()
    for (j in 1:nrow(city_data)) {
      coord1 <- results[i, 2:3]
      coord2 <- coord_rest[j, ]
      coord_dist <- c(coord_dist, dist(rbind(coord1, coord2))[1])
    }
    coord_weight <- coord_dist %>% smoother(D = D0 * Dx, l = l) %>% normalizer()
    results$value[i] <- sum(raw_values * coord_weight)
  }

  results
}

make_raster <- function(smoothed_df, map_df) {
  raster <- st_as_sf(smoothed_df, coords = c("lon", "lat"), crs = crs, remove = FALSE)
  raster[map_df, ]
}

################################################################################
## DATA
################################################################################
worldmap <- read_rds('Dataset/census/world_map.rds')
city_meta <- read_rds('Dataset/meta/city_metadata.rds') %>%
  select(nodeID, country_code, country_name, city_name, fcode, lat, lon)

################################################################################
## LOOP OVER COUNTRIES AND DIMENSIONS
################################################################################
switch(country,
    UA = {
      cities <- read_rds('Dataset/Inglehart_Welzel/UA_wvs_iw_scores.rds')
      where <- 'ukraine'
      colour_gradient <- c(UKRAINE_COLOUR, UKRAINE_COLOUR2)
    },
    RU = {
      cities <- read_rds('Dataset/Inglehart_Welzel/RU_wvs_iw_scores.rds')
      where <- 'russia'
      colour_gradient <- if(dimension == 'trad_secular') {
          c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
        } else {
          c(RUSSIAN_COLOUR2, RUSSIAN_COLOUR)
        }
    }
)
joined_data <- cities %>% left_join(city_meta) %>% ungroup()

# plot for chosen dimension
message(sprintf('=== %s / %s ===', country, dimension))

plot_data <- joined_data %>%
  filter(country_code == country) %>%
  mutate(value = ntile(!!sym(dimension), nquantile))

plotDT <- prepare_plotting_data(plot_data, viewpoint = where, worldmap = worldmap)
map_sf <- plotDT[[1]]
point_dt <- plotDT[[2]]

grid_dt <- make_raster_grid(map_sf, width_pixel = resolution)
message(sprintf('The raster grid has %s points to fill', length(grid_dt)))

city_data <- data.frame(
  city_name = point_dt$city_name,
  change = point_dt$change,
  fcode = point_dt$fcode,
  lon = st_coordinates(point_dt)[, 1],
  lat = st_coordinates(point_dt)[, 2]
)

smoothed_grid <- perform_smoothing(grid_dt, city_data, Dx, l)
map_geometry <- map_sf$geometry[!st_is_empty(map_sf$geometry), drop = FALSE]
map_outline <- as_Spatial(map_geometry) %>% fortify()
raster_dt <- make_raster(smoothed_grid, map_sf)

point_data <- city_data %>%
  mutate(city_category = ifelse(!is.na(fcode) & fcode == 'PPLC', 'capital', 'non-capital')) %>%
  mutate(city_category = factor(city_category, levels = c('non-capital', 'capital')))

ggplot() +
  geom_raster(data = raster_dt, aes(x = lon, y = lat, fill = value)) +
  geom_path(data = map_outline, aes(x = long, y = lat, group = group), colour = "gray20", linewidth = 0.1) +
  labs(title = dimension, fill = '') +
  geom_point(data = point_data, aes(x = lon, y = lat, shape = city_category),
    colour = ifelse(country == 'UA', 'black', 'white'),
    alpha = 1,
    size = 1.5
  ) +
  scale_fill_gradient(low = colour_gradient[1], high = colour_gradient[2]) +
  scale_shape_manual(values = c('cross', 'square'), guide = 'none') +
  theme_map()

plot_save(sprintf('Main/%s_iw_%s_map', country, dimension), c(180, 120))