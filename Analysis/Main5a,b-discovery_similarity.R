#' Discovery similarity among countries within each world regions
#' Related to Fig.5 in paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# DATA PREPARATION
################################################################################
SIMULATE <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load raw 6 months data
chart_data <- read_rds('Dataset/chart/study_set_chart.rds')

# load city metadata
city_region <- read_rds('Dataset/meta/city_metadata.rds')
city_region <- city_region %>% select(nodeID, worldbank_region)
regions <- city_region$worldbank_region %>% unique() %>% sort()

# join the data
joined_data <- chart_data %>% ungroup() %>% left_join(city_region)


################################################################################
# BOOTSTRAP DISCOVERY SIMILARITY BY DAY
################################################################################
# number of bootstraps to perform
n_boot <- 1000

if(SIMULATE){
  # for each world region, calculate the similarity of music discovery between countries on each day
  region_store = list()
  for (i in 1:length(regions)) {
    message(sprintf('Beginning region %s', regions[i]))
    
    region <- joined_data %>% filter(worldbank_region == regions[i])
    
    # if region consist of 1 country then skip
    countries <- region$country_code %>% unique()
    print(sprintf('There are %s countries in this region', length(countries)))
    if(length(countries) == 1){message('Skipping because region is formed of single country'); next()}
    
    # get the unique songs for each country
    region_unique <- region %>%
      group_by(date, country_code) %>%
      distinct(worldbank_region, date, country_code, trackID)
    
    # region level stats on number of unique songs, jaccard distance and similarity
    boot_store = list()
    for (b in 1:n_boot) {
      region_boot <- region_unique %>% group_by(date) %>% sample_n(size = n(), replace = T)
      region_stat <- region_boot %>%
        group_by(date, worldbank_region) %>%
        reframe(region_unique = n_distinct(trackID),
                region_jaccard = region_unique / n(),
                region_similarity = 1 - region_jaccard) %>%
        cbind(boot = b)
      boot_store[[b]] <- region_stat
      print(sprintf('boot %s', b))
    }
    region_store[[i]] <- do.call(rbind, boot_store)
    
  }
  # save the bootstrapped results
  region_output <- do.call(rbind, region_store)
  region_output %>% write_rds(sprintf('Bootstraps/region_discovery_similarity_boot%s.rds', n_boot))
} else {
  boot_load()
  region_output <- read_rds(sprintf('Bootstraps/region_discovery_similarity_boot%s.rds', n_boot))
}


################################################################################
# AGGREGATION
################################################################################
# normalize for each region
region_sim_norm <- region_output %>%
  group_by(worldbank_region) %>%
  mutate(norm_sim = scale(region_similarity))

# make a global reference
global_ref <- region_sim_norm %>%
  group_by(boot, date) %>%
  reframe(norm_sim = mean(norm_sim)) %>%
  cbind(worldbank_region = 'Global')

# join the global reference with regional aggregates
boot_normalized <- region_sim_norm %>% rbind(global_ref)

# make a aggregated mean by each date and drop if it's an outlier (+- 2SD)
# these values become the bases for the mean points to plot
agg_boot <- boot_normalized %>%
  group_by(worldbank_region, date) %>%
  reframe(mean_norm_sim = mean(norm_sim)) %>%
  filter(mean_norm_sim < 2, mean_norm_sim > -2) %>%
  left_join(WORLDBANK_REGION_COLOUR) # join for colour palette


################################################################################
# GAM FITTING
################################################################################
# for each bootstrap, fit a GAM
# these become the bases for the gam line and ribbon to fit
n_spline <- 2

gam_boot <- boot_normalized %>%
  mutate(date = as.Date(date)) %>%
  group_by(worldbank_region, boot) %>%
  reframe(gam_fit(date, norm_sim, n_spline)) %>%
  ungroup() %>%
  rename(date = x, norm_sim = y)

gam_boot_agg <- gam_boot %>%
  group_by(worldbank_region, date) %>%
  reframe(get_boot_mean_ci(norm_sim, 'sim')) %>%
  filter(worldbank_region != "Global") %>%
  left_join(WORLDBANK_REGION_COLOUR) # join for colour palette


################################################################################
# TRENDS
################################################################################
# plot temporal trend change of Post-Soviet in comparison to the global average
(
  gam_change_plot <- gam_boot_agg %>%
    ggplot(
      aes(
        as.Date(date),
        sim_m,
        group = worldbank_region,
        colour = region_colour,
        fill = region_colour
      )
    ) +
    geom_hline(yintercept = 0, colour = 'gray50') +
    
    # plot the mean GAM line with ribbon as CI
    geom_line(alpha = 1) +
    geom_ribbon(
      aes(ymin = sim_lower_ci, ymax = sim_upper_ci),
      alpha = 0.3,
      size = 0
    ) +
    
    # add the raw points for post-Soviet
    geom_point(
      data = agg_boot %>% filter(worldbank_region == 'Post-Soviet'),
      aes(as.Date(date), mean_norm_sim),
      size = 1,
      alpha = 0.5
    ) +
    
    # add baseline 0 (no change) as reference and war onset date
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", 
                 limits = c(min(as.Date(agg_boot$date)), max(as.Date(agg_boot$date)) + 7)) +
    geom_vline(xintercept = WAR_START, linetype = 'dashed') +
    labs(y = 'Discovery similarity\nacross countries', x = '', fill = 'World Region') +
    scale_colour_identity() +
    scale_fill_identity() +
    theme(legend.position = 'none')
)

# save plot for main figure
plot_save('Main/within_region_discovery_similarity_over_time', c(130, 80))

# Report stats on the coefficients of pre and post invasion
decline_coef <- boot_normalized %>%
  filter(worldbank_region == 'Post-Soviet') %>%
  mutate(window = case_when(
    date < WAR_START ~ 'pre', # pre war
    date > WAR_START & date < WAR_START + 30 ~ '1 month', # within one month after onset
    TRUE ~ NA
  )) %>%
  na.omit()

# fit gams
decline_gam <- decline_coef %>%
  group_by(window, boot) %>%
  do(coef = gam::gam(region_similarity ~ as.Date(date), data = .) %>% coef() %>% .[2] %>% as.numeric())

# report stats
decline_gam %>%
  group_by(window) %>%
  reframe(get_boot_mean_ci(coef %>% unlist(), 'coef'))


################################################################################
# EFFECT SIZE OF CHANGE
################################################################################
# plot the effect size (Cohen's d) for pre post change relative to global mean
region_effect <- region_sim_norm %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post')) %>%
  select(-date)

# do t-test using the pre and post values for each boot strapped samples
region_change_effect <- region_effect %>%
  pivot_wider(names_from = pre_post, values_from = region_similarity) %>%
  group_by(boot, worldbank_region) %>%
  summarise(get_t_stat(post %>% unlist(), pre %>% unlist())) %>%
  ungroup()
  
# aggregate across bootstraps
region_change_effect_agg <- region_change_effect %>% 
  group_by(worldbank_region) %>%
  reframe(get_boot_mean_ci(cohensD, 'd')) %>%
  left_join(WORLDBANK_REGION_COLOUR) # join for colour palette

region_change_effect_agg <- region_change_effect_agg %>%
  mutate(worldbank_region = str_replace_all(worldbank_region, ' & ', ' &\n'))

# plot effect differences
(region_change_effect_plot <- region_change_effect_agg %>%
    ggplot(aes(
      reorder(worldbank_region,- d_m),
      d_m,
      group = worldbank_region,
      colour = region_colour
    )) +
    geom_hline(yintercept = 0, colour = 'gray50') +
    # geom_bar(stat = 'identity') +
    geom_pointrange(aes(ymin = d_lower_ci, ymax = d_upper_ci)) +
    coord_flip() +
    ylim(-2.6, 0.8) +
    labs(y = "Pre vs. Post change\n(Cohen's d)", x = '') +
    scale_color_identity() +
    theme(legend.position = 'none')
)

# report statistics of effect size in text
region_change_effect_agg %>%
  arrange(d_m)

# save plot for main figure
plot_save('Main/within_region_discovery_similarity_effect_size', c(100, 80))

# make binded plot
gam_change_plot + region_change_effect_plot + plot_layout(widths = c(2.5, 1))
plot_save('Main/within_region_discovery_similarity_bind', c(183, 60))

