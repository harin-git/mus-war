#' Example of a song that spreads across cities in the study set
#' Related to Fig. 1 in paper

source('utils.R')

# Read the city metadata
city_data <- read_rds('Dataset/meta/city_metadata.rds')

# load songs data
d <- readRDS('Dataset/chart/study_set_chart.rds')

d_sub <- d %>%
    filter(country_code %in% SUB_COUNTRIES) %>%
    as_tibble()

# example song: Stefania (Kalush Orchestra). Day on which each city first had it in the top 50
sample_song <- '603183336'
song_spread_smp <- d_sub %>%
    filter(trackID == sample_song) %>%
    group_by(nodeID, city_name) %>%
    summarise(appear_date = min(date), .groups = 'drop') %>%
    mutate(day_since = as.numeric(appear_date - min(appear_date))) %>%
    left_join(city_data %>% select(nodeID, lat, lon), by = 'nodeID')

# Load additional packages for mapping
library(maps)
library(mapdata)

# Get world map data
world_map <- map_data("world")

# density surface over the full map extent (same default bandwidth as stat_density_2d)
grid_kde <- function(dt, n = raster_n(FULL_W / 3)) {
    if (nrow(dt) < 3) dt <- bind_rows(dt, dt %>% mutate(lon = lon + 0.5), dt %>% mutate(lat = lat + 0.5))  # day 0 has one city
    h <- c(MASS::bandwidth.nrd(dt$lon), MASS::bandwidth.nrd(dt$lat))
    k <- MASS::kde2d(dt$lon, dt$lat, h = h, n = n, lims = c(20, 90, 40, 70))
    tibble(expand.grid(lon = k$x, lat = k$y), density = as.vector(k$z))
}

make_spread_map <- function(day) {
    dt <- song_spread_smp %>% filter(day_since <= day)
    g <- grid_kde(dt)
    ggplot() +
    # Add world map outline (cropped to region)
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "grey95", color = "grey50", linewidth = LW_MIN) +
    # add density distribution as raster smooth map
    raster_layer(g %>% mutate(density = density / max(density)), 'lon', 'lat', 'density', '#FFA50000', '#FFA50080', limits = c(0, 1)) +
    # cities where the song has appeared
    geom_point(data = dt, aes(x = lon, y = lat), shape = 3, size = 0.8, stroke = 0.3, colour = 'black') +
    coord_fixed(1.3, xlim = c(20, 90), ylim = c(40, 70), expand = FALSE) +
    scale_x_continuous(breaks = seq(30, 90, 20), labels = lon_lab) +
    scale_y_continuous(breaks = seq(40, 70, 10), labels = lat_lab) +
    labs(title = sprintf('Day %s', day)) +
    theme_map_axes() +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(1, 3, 1, 1, 'mm'))
}

# Fig. 1c: day 0, 10 and 30
spread_plot <- make_spread_map(0) + make_spread_map(10) + make_spread_map(30)
plot_save('Main/song_spread_example', c(FULL_W, 38),
          song_spread_smp %>% select(nodeID, city_name, lat, lon, first_date = appear_date, day_since),
          spread_plot)
