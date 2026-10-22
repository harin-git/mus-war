#' Calculate changes in network connections and density
#' Related to Fig.5 in paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load the node dictionary
node_dict <- read_rds('Dataset/meta/city_metadata.rds') %>%
  select(nodeID, city_name, country_code, worldbank_region, lat, lon)

all_regions <- node_dict$worldbank_region %>% unique()

## load raw bootstrapped edges
# Raw saved edge draws are loaded only inside the optional resampling branch.
# This script never invokes NETINF.

# if TRUE, the trimming of dense edges is performed
trim_edge <- TRUE
trim_percent <- 5  # if trim_edge is TRUE, the edges below the percentage threshold is treated as noise and excluded


################################################################################
# BOOTSTRAP WITHIN AND ACROSS CONNECTION CHANGES
################################################################################
n_boot <- 1000

if(SIMULATION){
  pre_edge <- read_rds('Bootstraps/pre_netinf_boot1000.rds')
  post_edge <- read_rds('Bootstraps/post_netinf_boot1000.rds')
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


# Statistical tables use the same staging root as plots and source data.
statistics_dir <- file.path(Sys.getenv('PLOT_DIR', 'Plots'), 'statistics')
dir.create(statistics_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################
# GLOBAL NETWORK CORRELATION (DESCRIPTIVE ONLY)
################################################################################
global_r <- read_rds('Bootstraps/global_network_cor_boot1000.rds')
# City-pair entries share cities; do not attach a city-pair correlation P value.
write_csv(tibble(B = sum(is.finite(global_r$r)), r = mean(global_r$r),
                       lower_95ci = unname(quantile(global_r$r, .025)),
                       upper_95ci = unname(quantile(global_r$r, .975))), file.path(statistics_dir, 'network_global_r_descriptive.csv'))

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
within_plot <- within_connect_change %>%
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
plot_save('SI/connection_percent_change', c(120, 100), within_connect_change, within_plot, source_name = 'Supplementary_Fig_8')


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

between_plot <- post_soviet_inout_agg %>%
  filter(!is.infinite(all_change_m)) %>%
  left_join(WORLDBANK_REGION_COLOUR %>% rename(region = worldbank_region)) %>%
  ggplot(aes(reorder(region, all_change_m), all_change_m, ymin = all_change_lower_ci, ymax = all_change_upper_ci, fill = region_colour)) +
  geom_col() +
  geom_errorbar() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  coord_flip() +
  scale_fill_identity() +
  labs(y = 'Percentage change in number of edges (%)', x = '', title = '')

plot_save('SI/post_soviet_w_other_regions_connection_change', c(120, 50), post_soviet_inout_agg, between_plot, source_name = 'Unnumbered_between_region_connections')


# Inference uses one change per saved bootstrap, not a t-test across draws.
within_stats <- within_connect %>% group_by(source_region) %>%
  reframe(boot_test(connect_change_percent), pre = mean(n_pre), post = mean(n_post))
write_csv(within_stats, file.path(statistics_dir, 'within_edge_change.csv'))
within_wide <- within_connect %>% select(boot, source_region, connect_change_percent) %>%
  pivot_wider(names_from = source_region, values_from = connect_change_percent)
write_csv(map_dfr(setdiff(names(within_wide), c('boot', 'Post-Soviet')), function(other) {
  boot_test(abs(within_wide$`Post-Soviet`) / abs(within_wide[[other]]), null = 1) %>% mutate(vs = other)
}), file.path(statistics_dir, 'postsoviet_edge_change_ratios.csv'))
write_csv(post_soviet_inout %>% select(boot, region, out_change, in_change, all_change) %>%
  pivot_longer(ends_with('_change'), names_to = 'direction') %>% group_by(region, direction) %>%
  reframe(boot_test(value)), file.path(statistics_dir, 'postsoviet_between_edge_change.csv'))

################################################################################
# UKRAINIAN AND RUSSIAN CITIES WITH EUROPE
################################################################################
se <- boot_output$soviet_europe %>% filter(country_code %in% c('UA','RU')) %>%
  mutate(Outgoing = post_out - pre_out, Incoming = post_in - pre_in)
city <- se %>% group_by(country_code, city) %>% summarise(across(c(Outgoing, Incoming), mean), .groups = 'drop') %>%
  pivot_longer(c(Outgoing, Incoming), names_to = 'direction', values_to = 'new_edges')
ctry <- se %>% group_by(country_code, boot) %>% summarise(across(c(Outgoing, Incoming), mean), .groups = 'drop') %>%
  pivot_longer(c(Outgoing, Incoming), names_to = 'direction') %>% group_by(country_code, direction) %>%
  summarise(mean = mean(value), lower_95ci = quantile(value, .025), upper_95ci = quantile(value, .975), .groups = 'drop')
lab <- function(x) mutate(x, country = factor(country_code, c('RU','UA'), c('Russia','Ukraine')),
                          direction = factor(direction, c('Incoming','Outgoing'), c('Incoming edges from Europe','Outgoing edges to Europe')))
set.seed(1)
pe <- ggplot(lab(city), aes(new_edges, country, colour = country)) +
  geom_vline(xintercept = 0, linetype = 'dashed', linewidth = LW) +
  geom_jitter(height = 0.12, width = 0, shape = 21, size = 1, stroke = 0.3) +
  geom_pointrange(data = lab(ctry), aes(x = mean, xmin = lower_95ci, xmax = upper_95ci), size = 0.7, linewidth = 0.5) +
  facet_wrap(~ direction) +
  scale_colour_manual(values = c(Russia = RUSSIAN_COLOUR, Ukraine = UKRAINE_COLOUR), guide = 'none') +
  labs(x = 'Number of new edges', y = NULL)
plot_save('Fig5/e_new_edges_europe', c(75, 55),
           bind_rows(lab(city) %>% mutate(level = 'city'), lab(ctry) %>% mutate(level = 'country (bootstrap)')) %>%
             select(level, country, city, direction, new_edges, mean, lower_95ci, upper_95ci), pe, source_name = 'Fig_5e')

# Country means and contrasts are paired by bootstrap ID (private s06/s12 method).
country_draws <- se %>% mutate(Total = Outgoing + Incoming) %>% group_by(country_code, boot) %>%
  summarise(across(c(Outgoing, Incoming, Total), mean), .groups = 'drop')
write_csv(country_draws %>% pivot_longer(c(Outgoing, Incoming, Total), names_to = 'direction') %>%
  group_by(country_code, direction) %>% reframe(boot_test(value)), file.path(statistics_dir, 'new_edges_country_boot.csv'))
country_wide <- country_draws %>% pivot_wider(names_from = country_code, values_from = c(Outgoing, Incoming, Total))
write_csv(bind_rows(
  boot_test(country_wide$Total_UA - country_wide$Total_RU) %>% mutate(contrast = 'UA - RU total new edges'),
  boot_test(country_wide$Outgoing_UA - country_wide$Incoming_UA) %>% mutate(contrast = 'UA outgoing - incoming'),
  boot_test(country_wide$Outgoing_RU - country_wide$Incoming_RU) %>% mutate(contrast = 'RU outgoing - incoming')
), file.path(statistics_dir, 'new_edges_contrasts.csv'))

################################################################################
# CITIES IN EUROPE THAT MADE NEW CONNECTIONS
################################################################################
# Base map follows the approved export, using maps::world, not a network download.
nd <- read_rds('Dataset/meta/city_metadata.rds') %>% select(city = city_name, country_code, lat, lon)
es <- boot_output$europe_soviet %>% filter(soviet_country %in% c('UA','RU')) %>%
  group_by(country_code, city, soviet_country) %>% summarise(new_incoming = mean(post_in - pre_in), .groups = 'drop') %>%
  filter(new_incoming > 0) %>% left_join(nd, by = c('city','country_code')) %>%
  group_by(city) %>% arrange(soviet_country != 'UA', .by_group = TRUE) %>% mutate(lon_plot = lon + (row_number() - 1) * 0.5) %>% ungroup() %>%
  mutate(from = factor(soviet_country, c('UA','RU'), c('Ukraine','Russia')))
# same base map as Fig. 1 (maps::map_data('world'), grey95 land, grey50 borders)
world_md <- ggplot2::map_data('world')
pf <- ggplot() +
  geom_polygon(data = world_md, aes(long, lat, group = group), fill = 'grey95', colour = 'grey50', linewidth = LW_MIN) +
  geom_point(data = es, aes(lon_plot, lat, colour = from), size = 1.2, shape = 16) +
  scale_colour_manual(values = c(Ukraine = UKRAINE_COLOUR, Russia = RUSSIAN_COLOUR), name = 'New edges formed with') +
  coord_fixed(1.3, xlim = c(-3, 35), ylim = c(45, 65), expand = FALSE) +   # same aspect as Fig. 1
  scale_x_continuous(breaks = seq(0, 30, 10), labels = lon_lab) + scale_y_continuous(breaks = seq(45, 65, 5), labels = lat_lab) +
  theme_map_axes() + theme(legend.position = 'none', legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(1, 'mm'))
plot_save('Fig5/f_europe_new_edges_map', c(88, 55),
          es %>% select(country_code, city, from, lat, lon, new_incoming), pf, source_name = 'Fig_5f')
