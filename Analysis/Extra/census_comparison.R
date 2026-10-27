#' Produce map of Ukraine and Russia by the census data on language and ethnic proportion
#' Related to Supplementary Information in paper

# load study-wide functions and global variables
source('utils.R')
require(sf) # spatial data handling
require(rnaturalearth) # for world map

################################################################################
# PREPARATION
################################################################################
# determine country of analysis
COUNTRY_CODE <- Sys.getenv('COUNTRY_CODE', 'UA') # 'UA' or 'RU'
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
         CENSUS_RAW <- read.csv('Dataset/census/russia_census_2021.csv')
         # filter proportion with russian ethnicity "Русские"
         # Arkhangelsk and Tyumen regions are listed with and without their autonomous okrugs ("без АО").
         # The map polygons exclude the okrugs, so keep one row per region.
         CENSUS <- CENSUS_RAW %>% filter(nationality == "Русские") %>%
           group_by(iso_3166_2) %>% filter(n() == 1 | grepl('без АО', name_ru)) %>% ungroup() %>%
           select(-name_ru)
         CENSUS_RAW <- CENSUS_RAW %>% select(-name_ru)
         # calculate russian proportion
         CENSUS <- CENSUS %>% mutate(local_prop = n/nationality_indicated_population*100)
         COLOR_GRADIENT <- c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
         MUSIC <- read_rds('Dataset/lyrics/russian_lyrics_change_by_city.rds')
       }
)
stopifnot(!anyDuplicated(CENSUS$iso_3166_2))
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
  ukraine_regions <- read_rds('Dataset/census/ukraine_regions_map.rds')
  russian_regions <- read_rds('Dataset/census/russian_regions_map.rds')

  # update ukraine regions including crimea
  region_all <- ukraine_regions %>% bind_rows(russian_regions %>% filter(iso_3166_2 %in% c('UA-43', 'UA-40')))
  region <- region_all
} else {
  russian_regions <- ne_states(country = 'Russia', returnclass = "sf") %>% select(iso_3166_2, name, name_ru, geometry)

  region_all <- russian_regions %>% 
    filter(!iso_3166_2 %in% c('UA-43', 'UA-40'))
  # crop for display only; cities east of the crop are still matched to their region
  region <- region_all %>% sf::st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62)
}

# Transform geometries to the specified CRS
region <- st_transform(region, CRS) 
region_all <- st_transform(region_all, CRS)

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

mapping <- sapply(sf::st_within(city_points, region_all), function(i) if (length(i) == 1) i else NA_integer_)
mapped_cities <- region_all[mapping, ] %>%
  as_tibble() %>%
  select(iso_3166_2) %>%
  mutate(nodeID = local_songs_city$nodeID,
         city_name = local_songs_city$city_name) %>%
  na.omit()

city_music <- mapped_cities %>%
  left_join(MUSIC, by = 'nodeID')

# join with ethnic data (one census row per region, so one row per city)
music_ethnic <- city_music %>%
  left_join(CENSUS, by = 'iso_3166_2')

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
stopifnot(!anyDuplicated(music_ethnic$nodeID))

# test correlation: two-tailed Spearman, Fisher-z 95% CI
spearman_stats <- function(x, y) {
  ok <- complete.cases(x, y); n <- sum(ok)
  ct <- cor.test(x[ok], y[ok], method = 'spearman', exact = FALSE)
  ci <- tanh(atanh(unname(ct$estimate)) + c(-1, 1) * qnorm(0.975) / sqrt(n - 3))
  tibble(n = n, df = n - 2, rho = unname(ct$estimate), lower_ci = ci[1], upper_ci = ci[2], p = ct$p.value)
}
message('Ethnic results...')
print(spearman_stats(music_ethnic$pre_post_change, music_ethnic$local_prop), width = Inf)

if(COUNTRY_CODE == 'UA'){
  message('Language results...')
  print(spearman_stats(music_ethnic$pre_post_change, music_ethnic$local_lang_prop), width = Inf)
}


# plot the correlations between music and census
music_ethnic %>%
  ggplot(aes(local_prop, pre_post_change * 100)) +
  geom_smooth(method = 'glm', formula = y ~ x, colour = 'gray50') +
  geom_point(aes(colour = city_type), size = 1, alpha = 0.8) +
  ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001,  cor.coef.name = 'rho', method = 'spearman',
                   size = BASE * PT) +
  switch (COUNTRY_CODE,
          UA = labs(x = 'Ethnic Ukranian (%)', y = 'Local music (%)'),
          RU = labs(x = 'Ethnic Russian (%)', y = 'Local music (%)') 
  ) +
  scale_colour_manual(values = c(rev(COLOR_GRADIENT)), guide = 'none')

census_source <- music_ethnic %>%
  select(nodeID, city_name, iso_3166_2, city_type, census_prop = local_prop, any_of('local_lang_prop'), pre_post_change) %>%
  mutate(local_music_change = pre_post_change * 100)
plot_save(COR_SAVE, c(80, 80), data = census_source %>% select(-any_of('local_lang_prop')),
          source_name = ifelse(COUNTRY_CODE == 'UA', 'Supplementary_Fig_5a_ethnic', 'Supplementary_Fig_5b'))

# if Ukraine, plot also the language correlation
if(COUNTRY_CODE == 'UA'){
  music_ethnic %>%
    ggplot(aes(local_lang_prop, pre_post_change * 100)) +
    geom_smooth(method = 'glm', formula = y ~ x, colour = 'gray50') +
    geom_point(aes(colour = city_type), size = 1, alpha = 0.8) +
    ggpubr::stat_cor(r.accuracy = 0.01, p.accuracy = 0.001,  cor.coef.name = 'rho', method = 'spearman',
                     size = BASE * PT) +
    labs(x = 'Ukrainian speakers (%)', y = 'Local music (%)') +
    scale_colour_manual(values = c(rev(COLOR_GRADIENT)), guide = 'none')
  
  plot_save('SI/ukraine_music_census_language', c(80, 80), data = census_source %>% select(-census_prop),
            source_name = 'Supplementary_Fig_5a_language')
}


################################################################################
# RUSSIAN CITY EXAMPLES
################################################################################
if(COUNTRY_CODE == 'RU'){
  # get census mapped data
  census_ru_mapped <- read.csv('Dataset/census/russia_census_city_mapped.csv')
  
  # For russia, add statistics about two minority population regions as examples
  minority_groups <- c('RU-DA', 'RU-TA', 'RU-BA') # Makhachkala and Ufa
  
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
