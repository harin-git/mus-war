#' Calculate changes in network connections and density
#' Related to Fig.1 in paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# PREPARATION
################################################################################
SIMULATE <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load the node dictionary
node_dict <- read_rds('Dataset/meta/city_metadata.rds') %>%
  select(nodeID, city_name, country_code, worldbank_region, lat, lon)

all_regions <- node_dict$worldbank_region %>% unique()

## load raw bootstrapped edges
pre_edge <- read_rds('Bootstraps/pre_netinf_boot1000.rds')
post_edge <- read_rds('Bootstraps/post_netinf_boot1000.rds')

# if TRUE, the trimming of dense edges is performed
trim_edge <- TRUE
trim_percent <- 5  # if trim_edge is TRUE, the edges below the percentage threshold is treated as noise and excluded


################################################################################
# BOOTSTRAP WITHIN AND ACROSS CONNECTION CHANGES
################################################################################
n_boot <- 1000

if(SIMULATE){
  # Define all region combination grid 8^2 resulting as 56 rows
  region_mat <- expand.grid(all_regions, all_regions)
  colnames(region_mat) <- c('source_region', 'target_region')
  
  # define post soviet and europe as alphabets
  S <- 'Post-Soviet'
  E <- 'Europe'
  
  # perform bootstraps for within and between region connection
  region_store = list()
  region_density = list()
  soviet_europe = list()
  europe_soviet = list()
  for (b in 1:n_boot) {
    # sample half unique network simulations and add meta data
    smp_pre <- pre_edge %>% 
      filter(boot %in% sample(1:1000, 500)) %>%
      count(Source, Target) %>%
      add_edge_metadata(node_dict)
    
    smp_post <- post_edge %>% 
      filter(boot %in% sample(1:1000, 500)) %>%
      count(Source, Target) %>%
      add_edge_metadata(node_dict)
    
    # trim the noisy edges if TRUE
    if(trim_edge){
      smp_pre <- smp_pre %>% filter(n >= 1000 * 0.5 * (trim_percent/100))
      smp_post <- smp_post %>% filter(n >= 1000 * 0.5 * (trim_percent/100))
    }
    
    # get connection count by region
    pre_region <- smp_pre %>%
      group_by(source_region, target_region) %>%
      summarise(n_pre = n())
    
    post_region <- smp_post %>%
      group_by(source_region, target_region) %>%
      summarise(n_post = n())
    
    region_store[[b]] <- full_join(pre_region, post_region) %>% 
      cbind(boot = b)
    
    # get within region density
    within_region <- tibble(region = pre_region$source_region %>% unique() %>% sort())
    within_region$pre_density <- smp_pre %>% filter(source_region == target_region) %>% 
      group_by(source_region, target_region) %>% group_map(~ get_density(.)) %>% unlist()
    within_region$post_density <- smp_post %>% filter(source_region == target_region) %>% 
      group_by(source_region, target_region) %>% group_map(~ get_density(.)) %>% unlist()
    
    region_density[[b]] <- within_region %>% cbind(boot = b)
    
    # get connection of post soviet cities to and from europe region
    pre_soviet_outgoing <- smp_pre %>%
      filter(source_region == S, target_region == E) %>%
      count(source_country, source_city) %>%
      rename(country_code = source_country, city = source_city, pre_out = n)
    
    post_soviet_outgoing <- smp_post %>%
      filter(source_region == S, target_region == E) %>%
      count(source_country, source_city) %>%
      rename(country_code = source_country, city = source_city, post_out = n)
    
    pre_soviet_incoming <- smp_pre %>%
      filter(source_region == E, target_region == S) %>%
      count(target_country, target_city) %>%
      rename(country_code = target_country, city = target_city, pre_in = n)
    
    post_soviet_incoming <- smp_post %>% 
      filter(source_region == E, target_region == S) %>%
      count(target_country, target_city) %>%
      rename(country_code = target_country, city = target_city, post_in = n)
    
    soviet_europe[[b]] <- pre_soviet_outgoing %>%
      full_join(post_soviet_outgoing) %>%
      full_join(pre_soviet_incoming) %>%
      full_join(post_soviet_incoming) %>%
      cbind(boot = b)
    
    # get connection of europe cities to and from post-soviet region
    pre_europe_outgoing <- smp_pre %>%
      filter(source_region == E, target_region == S) %>%
      count(source_country, source_city, target_country) %>%
      rename(country_code = source_country, soviet_country = target_country, city = source_city, pre_out = n)
    
    post_europe_outgoing <- smp_post %>%
      filter(source_region == E, target_region == S) %>%
      count(source_country, source_city, target_country) %>%
      rename(country_code = source_country, soviet_country = target_country, city = source_city, post_out = n)
    
    pre_europe_incoming <- smp_pre %>%
      filter(source_region == S, target_region == E) %>%
      count(target_country, target_city, source_country) %>%
      rename(country_code = target_country, soviet_country = source_country, city = target_city, pre_in = n)
    
    post_europe_incoming <- smp_post %>% 
      filter(source_region == S, target_region == E) %>%
      count(target_country, target_city, source_country) %>%
      rename(country_code = target_country, soviet_country = source_country, city = target_city, post_in = n)
    
    europe_soviet[[b]] <- pre_europe_outgoing %>%
      full_join(post_europe_outgoing) %>%
      full_join(pre_europe_incoming) %>%
      full_join(post_europe_incoming) %>%
      cbind(boot = b)
    
    message(b)
    
  }
  # bind the list and fill 0 for no NA (i.e., no connection)
  region_boot <- do.call(rbind, region_store)
  region_boot[is.na(region_boot)] <- 0
  region_density_boot <- do.call(rbind, region_density)
  region_density_boot[is.na(region_density_boot)] <- 0
  soviet_europe_boot <- do.call(rbind, soviet_europe)
  soviet_europe_boot[is.na(soviet_europe_boot)] <- 0
  europe_soviet_boot <- do.call(rbind, europe_soviet)
  europe_soviet_boot[is.na(europe_soviet_boot)] <- 0
  
  # bind all as a list and save the bootstrapped outputs
  boot_output <- list(region_connection = region_boot, region_density = region_density_boot, soviet_europe = soviet_europe_boot, europe_soviet = europe_soviet_boot)
  boot_output %>% write_rds(sprintf('Bootstraps/within_between_connections_boot%s.rds', n_boot))
} else {
  boot_load()
  boot_output <- read_rds(sprintf('Bootstraps/within_between_connections_boot%s.rds', n_boot))
}


################################################################################
# WITHIN REGION CONNECTION CHANGE
################################################################################
region_connection <- boot_output$region_connection

# get regional summary
region_connection <- region_connection %>%
  group_by(boot, source_region, target_region) %>%
  summarise(n_pre = sum(n_pre),
            n_post = sum(n_post)) %>%
  ungroup()

# calculate connection change percentage
region_connection <- region_connection %>%
  mutate(connect_change_percent = (n_post - n_pre) / n_pre * 100)

# within region connection change and report
within_connect <- region_connection %>% filter(source_region == target_region)

within_connect_change <- within_connect %>%
  rename(region = source_region) %>%
  group_by(region) %>%
  reframe(get_boot_mean_ci(n_pre, 'pre_connect'),
          get_boot_mean_ci(n_post, 'post_connect'),
          get_boot_mean_ci(connect_change_percent, 'connect_change')
          )

# plot the changes
within_connect_change %>%
  rename(worldbank_region = region) %>%
  left_join(WORLDBANK_REGION_COLOUR) %>%
  ggplot(aes(reorder(worldbank_region, - connect_change_m), connect_change_m, colour = region_colour)) +
  geom_pointrange(aes(ymin = connect_change_lower_ci, ymax = connect_change_upper_ci)) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = 'Percentage change in\nnumber of edges (%)', title = '', fill = 'region') + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_colour_identity()

# save plot for SI
plot_save('SI/connection_percent_change', c(120, 100))


################################################################################
# POST-SOVIET WITH OTHER REGIONS CHANGE
################################################################################
# between connection change compared to post-soviet
post_soviet_between <- region_connection %>% 
  filter(source_region == 'Post-Soviet' | target_region == 'Post-Soviet') %>% 
  filter(source_region != target_region) %>%
  na.omit()

post_soviet_out <- post_soviet_between %>%
  filter(source_region == 'Post-Soviet') %>%
  select(boot, region = target_region, out_pre = n_pre, out_post = n_post)
post_soviet_in <- post_soviet_between %>%
  filter(target_region == 'Post-Soviet') %>%
  select(boot, region = source_region, in_pre = n_pre, in_post = n_post)
post_soviet_inout <- full_join(post_soviet_out, post_soviet_in)

# fill NA with 0
post_soviet_inout[is.na(post_soviet_inout)] <- 0

# make in and out combined edges as all
post_soviet_inout <- post_soviet_inout %>% mutate(all_pre = (in_pre + out_pre),
                                                  all_post = (in_post + out_post))

post_soviet_inout <- post_soviet_inout %>%
  group_by(boot, region) %>%
  mutate(out_change = (out_post - out_pre)/out_pre * 100,
         in_change = (in_post - in_pre)/in_pre * 100,
         all_change = (all_post - all_pre)/all_pre * 100
  )

post_soviet_inout_agg <- post_soviet_inout %>%
  na.omit() %>%
  group_by(region) %>%
  reframe(
    # the raw in and out edge stat
    get_boot_mean_ci(out_pre, 'out_pre'),
    get_boot_mean_ci(out_post, 'out_post'),
    get_boot_mean_ci(in_pre, 'in_pre'),
    get_boot_mean_ci(in_post, 'in_post'),
    get_boot_mean_ci(all_pre, 'all_pre'),
    get_boot_mean_ci(all_post, 'all_post'),

    # percentage change stats
    get_boot_mean_ci(out_change, 'out_change'),
    get_boot_mean_ci(in_change, 'in_change'),
    get_boot_mean_ci(all_change, 'all_change')
  )

# report stats
post_soviet_inout_agg

post_soviet_inout_agg %>%
  filter(!is.infinite(all_change_m)) %>%
  left_join(WORLDBANK_REGION_COLOUR %>% rename(region = worldbank_region)) %>%
  ggplot(aes(reorder(region, all_change_m), all_change_m, ymin = all_change_lower_ci, ymax = all_change_upper_ci, fill = region_colour)) +
  geom_col() +
  geom_errorbar() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  coord_flip() +
  scale_fill_identity() +
  labs(y = 'Percentage change in number of edges (%)', x = '', title = '')

plot_save('SI/post_soviet_w_other_regions_connection_change', c(120, 50))


################################################################################
# UKRAINIAN AND RUSSIAN CITIES WITH EUROPE
################################################################################
soviet_europe_connection <- boot_output$soviet_europe %>% filter(country_code %in% c('UA', 'RU'))
soviet_europe_connection <- soviet_europe_connection %>%
  mutate(outgoing = post_out - pre_out,
         incoming = post_in - pre_in) %>%
  mutate(total = outgoing + incoming)

# aggregate the edges
soviet_europe_agg <- soviet_europe_connection %>%
  group_by(country_code, city) %>%
  reframe(get_boot_mean_ci(outgoing, 'outgoing'),
          get_boot_mean_ci(incoming, 'incoming')
          )

soviet_europe_agg$country_code <- factor(soviet_europe_agg$country_code, levels = c('RU', 'UA'), labels = c('Russia', 'Ukraine'))

# plot the outgoing and incoming
soviet_europe_long <- soviet_europe_agg %>%
  select(country_code, city, outgoing_m, incoming_m) %>%
  pivot_longer(c(outgoing_m, incoming_m))
soviet_europe_long$name <- factor(soviet_europe_long$name, levels = c('outgoing_m', 'incoming_m'), labels = c('Outgoing edges to Europe', 'Incoming edges from Europe'))

soviet_country_mean <- soviet_europe_long %>%
  group_by(country_code, name) %>%
  summarise(m = mean(value),
            sd = sd(value),
            e = CI(value)) %>%
  mutate(lower_ci = m - e,
         upper_ci = m + e)

soviet_europe_long %>%
  ggplot(aes(value, country_code, colour = country_code)) +
  geom_jitter(width = 0.1, alpha = 1, shape=21, size = 1.5) +
  # add mean values
  geom_pointrange(data = soviet_country_mean, aes(m, country_code, xmin = lower_ci, xmax = upper_ci), size = 1) +
  labs(x = 'Number of new edges', y = '') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  scale_colour_manual(values = c(RUSSIAN_COLOUR, UKRAINE_COLOUR), guide = 'none') +
  theme_classic() +
  facet_wrap(~ name, nrow = 2)

# save plot for SI
plot_save('SI/outgoing_incoming_connections_w_europe', c(70, 100))

# report stats at the country level mean
soviet_europe_connection %>%
  group_by(country_code, boot) %>%
  summarise(total = mean(total)) %>%
  group_by(country_code) %>%
  reframe(
    get_boot_mean_ci(total, 'total')
  )

# find which are the top outliers and label in figure
soviet_europe_agg %>%
  group_by(country_code) %>%
  arrange(-outgoing_m) %>%
  slice(1:3)


################################################################################
# CITIES IN EUROPE THAT MADE NEW CONNECTIONS
################################################################################
# which cities in Europe got most of the Post-Soviet influences?
# map the locations
library(maps)
library(eurostat)
library(sf)
library(giscoR)

# Obtain map data for Europe
euromap <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)
EU27 <- eu_countries %>% 
  filter(code != 'UK') %>% 
  select(geo = code, name)

SHP_27 <- euromap %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU27, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

# get lat long for european cities
europe_soviet <- boot_output$europe_soviet %>% filter(soviet_country %in% c('UA', 'RU'))
europe_soviet_agg <- europe_soviet %>%
  group_by(country_code, city, soviet_country) %>%
  summarise(connections = mean(post_in - pre_in)) %>%
  filter(connections > 0)
  
europe_cities <- europe_soviet_agg %>% left_join(node_dict %>% select(city = city_name, lat, lon))
europe_cities$soviet_country <- factor(europe_cities$soviet_country, levels = c('UA', 'RU'))

# nudge the dots by longitude so they don't overlap
europe_cities <- europe_cities %>%
  group_by(city) %>%
  arrange(city, soviet_country) %>%
  mutate(order = 1:n()) %>%
  mutate(lon = lon + (order/2))

# transform to SF
europe_cities_sf <- st_as_sf(europe_cities, coords = c("lon", "lat"), crs = 4326)
europe_cities_transformed <- st_transform(europe_cities_sf, crs = st_crs(SHP_27))

# save for SI
SHP_27 %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = europe_cities_transformed, aes(colour = soviet_country), size = 3) +
  scale_x_continuous(limits = c(-3, 35)) +
  scale_y_continuous(limits = c(45, 65)) +
  theme_void() +
  scale_colour_manual(values = c(UKRAINE_COLOUR, RUSSIAN_COLOUR), guide = F)

plot_save('SI/map_europe_cities_post_soviet_connection', c(183, 183))

