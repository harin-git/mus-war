#' Produce map of Ukraine and Russia by the census data on language and ethnic proportion
#' Related to Supplementary Information in paper

# load study-wide functions and global variables
source('utils.R')
library(sf) # spatial data handling
library(rnaturalearth) # for world map

################################################################################
# PREPARATION
################################################################################
# determine country of analysis
COUNTRY_CODE <- 'UA'
NQUANTILE <- 4

# load data
city_meta <- read_rds('Dataset/meta/city_metadata.rds')

switch(COUNTRY_CODE,
       UA = {
         WHERE <- 'Ukranian'
         CENSUS_SAVE <- 'SI/ukraine_census_ehtnic_map'
         COR_SAVE <- 'SI/ukraine_music_census'
         # load ethnic data
         CENSUS <- read.csv('Dataset/census/ukraine_census_2001.csv')
         CENSUS$local_prop <- CENSUS$ukranians
         CENSUS$local_lang_prop <- CENSUS$ukranian_lang_prop
         COLOR_GRADIENT <- c(UKRAINE_COLOUR, UKRAINE_COLOUR2)
         MUSIC <- read_rds('Dataset/lyrics/ukranian_lyrics_change_by_city.rds')
       },
       RU = {
         WHERE <- 'Russian'
         CENSUS_SAVE <- 'SI/russia_census_ethnic_map'
         COR_SAVE <- 'SI/russia_music_census'
         CENSUS_RAW <- read.csv('Dataset/census/russia_census_2021.csv') %>% select(-name_ru)
         # filter proportion with russian ethnicity "Русские"
         CENSUS <- CENSUS_RAW %>% filter(nationality == "Русские")
         # calculate russian proportion
         CENSUS <- CENSUS %>% mutate(local_prop = n/nationality_indicated_population*100)
         COLOR_GRADIENT <- c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
         MUSIC <- read_rds('Dataset/lyrics/russian_lyrics_change_by_city.rds')
       }
)
local_songs_city <- MUSIC %>%
  left_join(city_meta %>% select(nodeID, city_name, lat, lon))

# what is the range of ethnic groups?
print(paste('min', min(CENSUS$local_prop) %>% signif(2)))
print(paste('max', max(CENSUS$local_prop) %>% signif(2)))

################################################################################
# PLOT MAP
################################################################################
# define CRS
sf_use_s2(FALSE)
CRS <-  "+proj=longlat +datum=WGS84"

if(COUNTRY_CODE == "UA"){
  # Get Ukraine's administrative regions
  ukraine_regions <- ne_states(country = "Ukraine", returnclass = "sf") %>% select(iso_3166_2, name, geometry)
  russian_regions <- ne_states(country = 'Russia', returnclass = "sf") %>% select(iso_3166_2, name, name_ru, geometry)
  
  # update ukraine regions including crimea
  region <- ukraine_regions %>% bind_rows(russian_regions %>% filter(iso_3166_2 %in% c('UA-43', 'UA-40')))
} else {
  russian_regions <- ne_states(country = 'Russia', returnclass = "sf") %>% select(iso_3166_2, name, name_ru, geometry)

  region <- russian_regions %>% 
    filter(!iso_3166_2 %in% c('UA-43', 'UA-40'))  %>%
    sf::st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62)   # crop map
}

# Transform geometries to the specified CRS
region <- st_transform(region, CRS) 

# join ethnic data 
plot_data <- region %>% left_join(CENSUS, by = 'iso_3166_2')

# plot the census map
plot_data %>%
  mutate(quant = ntile(local_prop, NQUANTILE)) %>%
  ggplot() +
  geom_sf(aes(fill = quant)) +
  theme_minimal() +
  labs(title = "", fill = '') +
  theme_map() +
  scale_fill_gradient(low = COLOR_GRADIENT[1], high = COLOR_GRADIENT[2], guide = F)

plot_save(CENSUS_SAVE, c(120, 100))

# if in Ukraine, plot also the language map
if(COUNTRY_CODE == 'UA'){
  plot_data %>%
    ggplot() +
    geom_sf(aes(fill = ntile(local_lang_prop, NQUANTILE))) +
    theme_minimal() +
    labs(title = "", fill = '') +
    theme_map() +
    scale_fill_gradient(low = COLOR_GRADIENT[1], high = COLOR_GRADIENT[2], guide = F)
  
  plot_save('SI/ukraine_census_language_map', c(120, 100))
}


################################################################################
# CORRELATION WITH MUSIC
################################################################################
city_points <- local_songs_city %>%
  select(city_name, lon, lat) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = CRS)

mapping <- sf::st_within(city_points, region) %>% as.character() %>% as.numeric()
mapped_cities <- region[mapping, ] %>%
  as_tibble() %>%
  select(iso_3166_2) %>%
  mutate(nodeID = local_songs_city$nodeID,
         city_name = local_songs_city$city_name) %>%
  na.omit()

city_music <- mapped_cities %>%
  left_join(MUSIC)

# join with ethnic data
music_ethnic <- city_music %>%
  left_join(plot_data)

music_ethnic <- switch(COUNTRY_CODE,
                       UA = {
                         music_ethnic %>%
                           mutate(
                             city_type = case_when(
                               city_name %in% c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk', 'Mariupol') ~ 'Occupied',
                               TRUE ~ 'Not occupied'
                             )
                           )
                       },
                       RU = {
                         music_ethnic %>%
                           mutate(city_type = ifelse(local_prop > 50, 'non-minority', 'minority')) %>%
                           filter(!is.na(city_type))
                       })  

# test correlation
message('Ethnic results...')
cor_result <- corr.test(music_ethnic %>% select(pre_post_change, local_prop), adjust = 'bonferroni', method = 'spearman')
print(cor_result, short=FALSE) 
print(cor_result$stars, quote=FALSE, short=FALSE)

if(COUNTRY_CODE == 'UA'){
  message('Language results...')
  cor_result <- corr.test(music_ethnic %>% select(pre_post_change, local_lang_prop), adjust = 'bonferroni', method = 'spearman')
  print(cor_result, short=FALSE) 
  print(cor_result$stars, quote=FALSE, short=FALSE)
}


# plot the correlations between music and census
music_ethnic %>%
  ggplot(aes(local_prop, pre_post_change * 100)) +
  geom_smooth(method = 'glm', colour = 'gray50') +
  geom_point(aes(colour = city_type), size = 1, alpha = 0.8) +
  ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001,  cor.coef.name = 'rho', method = 'spearman') +
  switch (COUNTRY_CODE,
          UA = labs(x = 'Ethnic Ukranian (%)', y = 'Local music (%)'),
          RU = labs(x = 'Ethnic Russian (%)', y = 'Local music (%)') 
  ) +
  scale_colour_manual(values = c(rev(COLOR_GRADIENT)), guide = F)

plot_save(COR_SAVE, c(80, 80))

# if Ukraine, plot also the language correlation
if(COUNTRY_CODE == 'UA'){
  music_ethnic %>%
    ggplot(aes(local_lang_prop, pre_post_change * 100)) +
    geom_smooth(method = 'glm', colour = 'gray50') +
    geom_point(aes(colour = city_type), size = 1, alpha = 0.8) +
    ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001,  cor.coef.name = 'rho', method = 'spearman') +
    labs(x = 'Ukrainian speakers (%)', y = 'Local music (%)') +
    scale_colour_manual(values = c(rev(COLOR_GRADIENT)), guide = F)
  
  plot_save('SI/ukraine_music_census_language', c(80, 80))
}


################################################################################
# RUSSIAN CITY EXAMPLES
################################################################################
if(COUNTRY_CODE == 'RU'){
  # get census mapped data
  census_ru_mapped <- read.csv('Dataset/census/russia_census_city_mapped.csv')
  
  # For russia, add statistics about two minority population regions as examples
  minority_groups <- c('RU-DA', 'RU-TA') # Makhachkala and Ufa
  
  minority_data <- filter(CENSUS_RAW, iso_3166_2 %in% minority_groups)
  minority_data <- minority_data %>%
    group_by(iso_3166_2) %>%
    mutate(total_pop = sum(n)) %>%
    group_by(iso_3166_2, nationality) %>%
    summarise(percent = n/total_pop*100) %>%
    group_by(iso_3166_2) %>%
    arrange(iso_3166_2, -percent) %>%
    mutate(rank = 1:n()) %>%
    mutate(nationality = ifelse(rank > 3, 'Other', nationality)) %>%
    group_by(iso_3166_2, nationality) %>%
    summarise(percent = sum(percent),
              rank = sum(rank)) %>%
    arrange(iso_3166_2, rank)
  
  minority_data
  
}
