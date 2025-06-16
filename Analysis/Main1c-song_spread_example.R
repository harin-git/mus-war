#' Example of a song that spreads across cities in the study set
#' Related to Fig. 1 in paper

source('utils.R')

# Read the city metadata
city_data <- read_csv('Dataset/meta/city_metadata.csv')

# load songs data
d <- readRDS('Dataset/chart/study_set_chart.rds')

d_sub <- d %>%
    filter(country_code %in% SUB_COUNTRIES) %>%
    as_tibble()

# get metadata
song_meta <- readRDS('Dataset/meta/song_metadata.rds') %>% as_tibble()
ua_songs <- song_meta %>% filter(lyrics_language == 'uk') %>% select(trackID, title, artist)

# get a song from Ukraine that spreads 
d_sub_sub <- d_sub %>%
    filter(trackID %in% ua_songs$trackID)

# get the song that spreads the most
song_spread <- d_sub_sub %>%
    group_by(trackID, city_name, nodeID) %>%
    summarise(appear_date = min(date)) %>%
    group_by(trackID) %>%
    mutate(day_since = as.numeric(appear_date - min(appear_date)))

# add lat and lon
song_spread <- song_spread %>%
    left_join(city_data %>% select(nodeID, lon, lat, worldbank_region))

# sample one song
sample_song <- '603183336' # Stefania (Kalush Orchestra)
song_spread_smp <- song_spread %>%
    filter(trackID == sample_song)

# make three datasets 
song_spread_0 <- song_spread_smp %>%
    filter(day_since == 0) %>% 
    cbind(day_group = 'day_0')

song_spread_10 <- song_spread_smp %>%
    filter(day_since <= 10) %>% 
    cbind(day_group = 'day_10')

song_spread_30 <- song_spread_smp %>%
    filter(day_since <= 30) %>% 
    cbind(day_group = 'day_30')

# combine
song_spread_all <- rbind(song_spread_0, song_spread_10, song_spread_30)

# get city metadata
# Load additional packages for mapping
library(maps)
library(mapdata)

# Get world map data
world_map <- map_data("world")

make_spread_map <- function(dt) {
    ggplot() +
    # Add world map outline (cropped to region)
    geom_polygon(data = world_map, 
                aes(x = long, y = lat, group = group),
                fill = "grey95", 
                color = "grey50", 
                linewidth = 0.3) +
    # Add Post-Soviet city points
    geom_point(data = dt, 
                aes(x = lon, y = lat, color = worldbank_region),
                size = 2, 
                alpha = 0.8,
                colour = 'black',
                shape = 3) +
    theme_map() +
    coord_fixed(1.3, xlim = c(20, 90), ylim = c(40, 70))
}


day_0_map <- make_spread_map(song_spread_0)
day_10_map <- make_spread_map(song_spread_10)
day_30_map <- make_spread_map(song_spread_30)

day_0_map + day_10_map + day_30_map
plot_save('Main/song_spread_example', c(500, 80))
