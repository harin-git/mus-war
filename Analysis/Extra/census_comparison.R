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
COUNTRY_CODE <- 'RU'
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
       }
)

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
