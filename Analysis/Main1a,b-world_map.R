#' Create a world map of the cities in the study set
#' Related to Fig. 1 in paper

source('utils.R')

# Load additional packages for mapping
library(maps)
library(mapdata)
library(ggplot2)

# Read the city metadata
city_data <- read_csv('Dataset/meta/city_metadata.csv')

# Check unique WorldBank regions in the data
unique_regions <- city_data$worldbank_region %>% unique() %>% sort()
print(paste("Unique WorldBank regions:", paste(unique_regions, collapse = ", ")))

# Get world map data
world_map <- map_data("world")

# Create the plot
world_city_map <- ggplot() +
  # Add world map outline
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "grey95", 
               color = "grey50", 
               linewidth = 0.3) +
  # Add city points colored by WorldBank region
  geom_point(data = city_data, 
             aes(x = lon, y = lat, color = worldbank_region),
             size = 0.8, 
             alpha = 0.7) +
  # Apply manual colors from utils.R
  scale_color_manual(name = "WorldBank Region",
                     values = setNames(WORLDBANK_REGION_COLOUR$region_colour, 
                                      WORLDBANK_REGION_COLOUR$worldbank_region), guide = "none") +
  # Apply map theme
  theme_map() +
  # Adjust coordinate system
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85))

# Display the map
print(world_city_map)

# save the global map
plot_save('Main/map_world_cities', c(183, 120))

# Create zoomed-in map for Post-Soviet region
# Filter for Post-Soviet cities
post_soviet_cities <- city_data %>%
  filter(worldbank_region == "Post-Soviet")

cat("\nCreating Post-Soviet region map with", nrow(post_soviet_cities), "cities...\n")

# Print countries in Post-Soviet region for reference
cat("Countries in Post-Soviet region:\n")
post_soviet_summary <- post_soviet_cities %>%
  count(country_code, country_name, sort = TRUE)
print(post_soviet_summary)

# Create the zoomed Post-Soviet map
post_soviet_map <- ggplot() +
  # Add world map outline (cropped to region)
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "grey95", 
               color = "grey50", 
               linewidth = 0.3) +
  # Add Post-Soviet city points
  geom_point(data = post_soviet_cities, 
             aes(x = lon, y = lat, color = worldbank_region),
             size = 2, 
             alpha = 0.8) +
  # Apply manual colors from utils.R (only Post-Soviet color will show)
  scale_color_manual(name = "WorldBank Region",
                     values = setNames(WORLDBANK_REGION_COLOUR$region_colour, 
                                      WORLDBANK_REGION_COLOUR$worldbank_region), 
                     guide = "none") +
  # Apply map theme
  theme_map() +
  # Zoom to Post-Soviet region coordinates
  # Approximate bounds: Belarus (23-32°E, 51-56°N), Ukraine (22-40°E, 44-52°N), 
  # Kazakhstan (46-87°E, 40-55°N), Western Russia (30-60°E, 45-70°N)
  coord_fixed(1.3, xlim = c(20, 90), ylim = c(40, 70))

# Display the Post-Soviet map
print(post_soviet_map)

# Save the Post-Soviet region map
plot_save('Main/map_post_soviet_region', c(183, 120))

# Print coordinate ranges for Post-Soviet cities
cat("\nPost-Soviet cities coordinate ranges:\n")
cat("Longitude: ", round(min(post_soviet_cities$lon), 2), " to ", round(max(post_soviet_cities$lon), 2), "\n")
cat("Latitude: ", round(min(post_soviet_cities$lat), 2), " to ", round(max(post_soviet_cities$lat), 2), "\n")

