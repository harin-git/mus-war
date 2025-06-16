#' Understand the increased similarity within Europe by looking at the songs that became hits across Europe after invasion
#' Related Supplementary Information in paper
source('utils.R')


################################################################################
# DATA PREPARATION
################################################################################
# load raw 6 months data
chart_data <- read_rds('Dataset/chart/study_set_chart.rds')

# load city metadata
city_region <- read_rds('Dataset/meta/city_metadata.rds')
city_region <- city_region %>% select(nodeID, worldbank_region)
regions <- city_region$worldbank_region %>% unique() %>% sort()

# join the data
joined_data <- chart_data %>% ungroup() %>% left_join(city_region) %>% as_tibble()

# subset Europe
europe <- joined_data %>% filter(worldbank_region == 'Europe')

################################################################################
# WHICH SONGS BECAME HITS ACROSS EUROPE?
################################################################################
pre_euro_songs <- europe %>% filter(pre_post == 'pre') %>% distinct(trackID) %>% pull(trackID)

post_euro <- europe %>% 
    filter(pre_post == 'post', !trackID %in% pre_euro_songs)

post_euro_agg <- post_euro %>%
    group_by(trackID) %>%
    summarise(n_countries = n_distinct(country_code)) %>%
    arrange(-n_countries)

# add metadata
song_meta <- read.csv('Dataset/meta/song_metadata.csv') %>% distinct(trackID, title, artist, genres)
post_euro_agg <- post_euro_agg %>% left_join(song_meta) %>% na.omit() %>% head(10)
print(post_euro_agg)
