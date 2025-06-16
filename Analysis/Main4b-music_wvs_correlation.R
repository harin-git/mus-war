#' Correlations between local music proportion and socio-cultural values
#' Related to Fig. 4 in paper

# load study-wide functions and global variables
source('utils.R')
require(sf) # for spatial mapping
require(psych) # for correlation test


################################################################################
# PREPARATION
################################################################################
COUNTRY_CODE <- 'UA'
GLOBAL <- TRUE # when true, use the global PC results, otherwise do separetely for UA and RU

city_meta <- read_rds('Dataset/meta/city_metadata.rds')

find_closest_point <- function(lon, lat, dataset) {
  d <- geosphere::distVincentySphere(c(lon, lat), cbind(dataset$lon, dataset$lat))
  closest_index <- which.min(d)
  city_name <- dataset[closest_index, ]$city_name
  nodeID <- dataset[closest_index, ]$nodeID
  local_music_change <- dataset[closest_index, ]$pre_post_change
  
  return(tibble(nodeID, city_name, distance = min(d), local_music_change))
}

switch(COUNTRY_CODE,
       UA = {
         LOCAL_MUSIC <- read_rds("Dataset/lyrics/ukranian_lyrics_change_by_city.rds")
         LOCAL_MUSIC <- LOCAL_MUSIC %>% 
           left_join(city_meta) %>%
           filter(country_code == COUNTRY_CODE)
         PCA <- read_rds("Dataset/wvs/UA_wvs_pca_loadings.rds")
         COLOR_GRADIENT <- c(UKRAINE_COLOUR2, UKRAINE_COLOUR)
       },
       RU = {
         # load census data
         CENSUS <- read_rds('Dataset/census/russia_census_2021_clean.rds')
         LOCAL_MUSIC <- read_rds("Dataset/lyrics/russian_lyrics_change_by_city.rds") 
         LOCAL_MUSIC <- LOCAL_MUSIC %>% 
           left_join(city_meta) %>%
           filter(country_code == COUNTRY_CODE)

         PCA <- read_rds("Dataset/wvs/RU_wvs_pca_loadings.rds")
         COLOR_GRADIENT <- c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
       }
)

# if using global loadings, override with global PCA results
if(GLOBAL){PCA <- read_rds('Dataset/wvs/global_wvs_pca_loadings.rds')}


################################################################################
# MATCHING
################################################################################
# get iso_3166_2 for Russian shazam cities
sf_use_s2(FALSE)
map <- read_rds('Dataset/census/world_map.rds')
crs <-  "+proj=longlat +datum=WGS84"
sf_map <- map %>% st_as_sf(CRS = crs)

# project to map and get the closest regions
region_points <- LOCAL_MUSIC %>%
  select(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = crs)
mapping <- st_within(region_points, sf_map) %>% as.character() %>% as.numeric()
mapped_regions <- map[mapping, ] %>% as_tibble() %>% select(iso_3166_2)
mapped_regions$nodeID <- LOCAL_MUSIC$nodeID

# join back to local music
plot_data_agg <- LOCAL_MUSIC %>%
  left_join(mapped_regions) %>%
  left_join(PCA %>% select(iso_3166_2, PC1:PC3)) %>%
  na.omit() %>%
  ungroup()

# make pre and post change %
plot_data_agg <- plot_data_agg %>%
  mutate(abs_percent_change = scale(abs(pre_post_change / sum(pre + post)) * 100))

message(sprintf('There are %s regions in %s', n_distinct(plot_data_agg$iso_3166_2), COUNTRY_CODE))

# add city type information
plot_data_agg <- switch (COUNTRY_CODE,
  UA = plot_data_agg %>% mutate(city_type = case_when(
    city_name %in% c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk', 'Mariupol') ~ 'Occupied',
    TRUE ~ 'Not occupied'
  )),
  RU = plot_data_agg %>%
    left_join(CENSUS)
)


################################################################################
# CORRELATIONS
#######################################s#########################################
# test correlation
cor_result <- corr.test(plot_data_agg %>% select(abs_percent_change, PC1:PC3), adjust = 'bonferroni', method = 'spearman')
print(cor_result, short=FALSE) 
print(cor_result$stars, quote=FALSE, short=FALSE) 

# plot the correlation
message(sprintf('Plotting the correlation between local music and PCA for %s', COUNTRY_CODE))

for (i in 1:3) {
  PC <- paste0('PC', i)
  
  p <- plot_data_agg %>%
    ggplot(aes(pre_post_change * 100, !!sym(PC))) +
    geom_smooth(method = 'glm', colour = 'gray50') +
    geom_point(aes(colour = city_type), size = 1, alpha = 0.5) +
    scale_colour_manual(values = COLOR_GRADIENT, guide = F)
  
  # save plot for main and SI figures
  if((PC == 'PC3')){
    p + labs(x = 'Local music (%)', y = 'Socio-cultural values')
    plot_save(sprintf('Main/%s_%s_%s_correlation', COUNTRY_CODE, 'music', PC), c(40, 50))
  } else {
    p +  
      ggpubr::stat_cor(r.accuracy = 0.01, 
                       p.accuracy = 0.01, 
                       cor.coef.name = 'rho', 
                       method = 'spearman',
                       size = 3) +
      labs(x = 'Local music (%)', y = PC)
    plot_save(sprintf('SI/%s_%s_%s_correlation', COUNTRY_CODE, 'music', PC), c(40, 50))
  }
}

