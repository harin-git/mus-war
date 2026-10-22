#' Create a world map of the cities in the study set
#' Related to Fig. 1 in paper

source('utils.R')

# Load additional packages for mapping
library(maps)
library(mapdata)
library(ggplot2)

# Read the city metadata
city_data <- read_rds('Dataset/meta/city_metadata.rds')

# Check unique WorldBank regions in the data
unique_regions <- city_data$worldbank_region %>% unique() %>% sort()
print(paste("Unique WorldBank regions:", paste(unique_regions, collapse = ", ")))

# Get world map data
world_map <- map_data("world")
map_base <- geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                         fill = "grey95", color = "grey50", linewidth = LW_MIN)

# Fig. 1a: world map of all cities, coloured by World Bank region (with longitude/latitude labels)
world_city_map <- ggplot() +
  map_base +
  geom_point(data = city_data, aes(x = lon, y = lat, color = worldbank_region), size = 0.5, alpha = 0.7, shape = 16) +
  scale_color_manual(values = REGION_COLS, guide = "none") +
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  scale_x_continuous(breaks = seq(-150, 150, 50), labels = lon_lab) +
  scale_y_continuous(breaks = seq(-40, 80, 40), labels = lat_lab) +
  theme_map_axes()

plot_save('Main/map_world_cities', c(90, 52),
          city_data %>% select(nodeID, city_name, country_code, country_name, worldbank_region, lat, lon),
          world_city_map, source_name = 'Fig_1a')

# Fig. 1b: zoomed-in map of the Post-Soviet region
post_soviet_cities <- city_data %>% filter(worldbank_region == "Post-Soviet")
cat("\nCreating Post-Soviet region map with", nrow(post_soviet_cities), "cities...\n")
post_soviet_cities %>% count(country_code, country_name, sort = TRUE) %>% print()

post_soviet_map <- ggplot() +
  map_base +
  geom_point(data = post_soviet_cities, aes(x = lon, y = lat), colour = REGION_COLS[['Post-Soviet']],
             size = 0.9, alpha = 0.8, shape = 16) +
  coord_fixed(1.3, xlim = c(20, 90), ylim = c(40, 62), expand = FALSE) +
  scale_x_continuous(breaks = seq(30, 90, 20), labels = lon_lab) +
  scale_y_continuous(breaks = seq(40, 70, 10), labels = lat_lab) +
  theme_map_axes()

plot_save('Main/map_post_soviet_region', c(80, 46),
          post_soviet_cities %>% select(nodeID, city_name, country_code, lat, lon),
          post_soviet_map, source_name = 'Fig_1b')
