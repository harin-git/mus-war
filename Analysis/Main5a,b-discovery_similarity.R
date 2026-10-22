#' Discovery similarity among countries within each world regions
#' Related to Fig.5 in paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# DATA PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

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

if(SIMULATION){
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

# Statistical tables use the same staging root as plots and source data.
statistics_dir <- file.path(Sys.getenv('PLOT_DIR', 'Plots'), 'statistics')
dir.create(statistics_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################
# AGGREGATION AND GAM FITTING
################################################################################
REG_SHORT <- c('East Asia & Pacific' = 'East Asia', 'Europe' = 'Europe', 'Latin America & Caribbean' = 'Latin America',
               'Middle East & North Africa' = 'Middle East', 'North America' = 'North America', 'Post-Soviet' = 'Post-Soviet')
rs <- region_output %>%
  group_by(worldbank_region) %>% mutate(norm_sim = as.numeric(scale(region_similarity))) %>% ungroup()
pts <- rs %>% group_by(worldbank_region, date) %>% summarise(mean = mean(norm_sim), .groups = 'drop') %>%
  filter(mean < 2, mean > -2)  # same single-day outlier exclusion as the original script
gam_fit2 <- function(d) { x <- as.numeric(d$date); m <- gam::gam(norm_sim ~ s(x, 2), data = data.frame(norm_sim = d$norm_sim, x = x))
  tibble(date = d$date, fit = as.numeric(predict(m))) }
message('Fitting GAMs (1000 bootstraps x 6 regions)...')
gb <- rs %>% group_by(worldbank_region, boot) %>% group_modify(~ gam_fit2(.x)) %>% ungroup()
ga <- gb %>% group_by(worldbank_region, date) %>%
  summarise(mean = mean(fit), lower_95ci = quantile(fit, .025), upper_95ci = quantile(fit, .975), .groups = 'drop')
pa <- ggplot(ga, aes(date, mean, colour = worldbank_region, fill = worldbank_region)) +
  geom_hline(yintercept = 0, colour = 'grey50', linewidth = LW) +
  geom_point(data = pts %>% filter(worldbank_region == 'Post-Soviet'), size = 1.2, alpha = 0.5, shape = 16) +
  geom_ribbon(aes(ymin = lower_95ci, ymax = upper_95ci), colour = NA, alpha = 0.25) +
  geom_line(linewidth = 0.4) +
  geom_vline(xintercept = WAR_START, linetype = 'dashed', linewidth = LW) +
  scale_colour_manual(values = REGION_COLS, labels = REG_SHORT, name = NULL) +
  scale_fill_manual(values = REGION_COLS, guide = 'none') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %Y', expand = expansion(mult = 0.01)) +
  labs(x = NULL, y = 'Discovery similarity\nacross countries (z)') + theme(legend.position = 'none')
plot_save('Fig5/a_similarity_over_time', c(100, 55),
           ga %>% rename(region = worldbank_region) %>% left_join(pts %>% rename(region = worldbank_region, daily_mean_postsoviet = mean), by = c('region','date')), pa, source_name = 'Fig_5a')


################################################################################
# RAW-SCALE SLOPES AND POST-MINUS-PRE SLOPE CHANGES
################################################################################
period_data <- region_output %>% ungroup() %>%
  mutate(date = as.Date(date), period = ifelse(date < WAR_START, 'pre', 'post'), x = as.numeric(date))
slopes <- period_data %>% group_by(worldbank_region, period, boot) %>%
  summarise(n_days = n(), slope = unname(coef(lm(region_similarity ~ x))[2]), .groups = 'drop')
write_csv(slopes %>% group_by(worldbank_region, period) %>% reframe(boot_test(slope)), file.path(statistics_dir, 'similarity_slopes.csv'))
slope_change <- slopes %>% select(-n_days) %>% pivot_wider(names_from = period, values_from = slope) %>%
  group_by(worldbank_region) %>% reframe(boot_test(post - pre))
write_csv(slope_change, file.path(statistics_dir, 'similarity_slope_change.csv'))

################################################################################
# EFFECT SIZE OF CHANGE AND BETWEEN-REGION BOOTSTRAP CONTRASTS
################################################################################
# Cohen's d uses the pooled daily SD, post minus pre, within each saved draw.
region_effect <- period_data %>% group_by(worldbank_region, boot) %>%
  summarise(welch(region_similarity[period == 'post'], region_similarity[period == 'pre']), .groups = 'drop')
d <- region_effect %>% group_by(worldbank_region) %>% reframe(boot_test(d)) %>%
  mutate(region = REG_SHORT[worldbank_region])
write_csv(d, file.path(statistics_dir, 'similarity_d.csv'))
effect_wide <- region_effect %>% select(worldbank_region, boot, d) %>% pivot_wider(names_from = worldbank_region, values_from = d)
region_contrasts <- map_dfr(setdiff(names(effect_wide), c('boot', 'Post-Soviet')), function(other) {
  boot_test(effect_wide$`Post-Soviet` - effect_wide[[other]]) %>% mutate(vs = other)
})
write_csv(region_contrasts, file.path(statistics_dir, 'postsoviet_vs_regions_d.csv'))
pb <- ggplot(d, aes(est, reorder(region, -est), colour = worldbank_region)) +
  geom_vline(xintercept = 0, colour = 'grey50', linewidth = LW) +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.5, linewidth = 0.4) +  # default fatten = 4 doubles the previous mean dot
  scale_colour_manual(values = REGION_COLS, guide = 'none') +
  labs(x = "Pre vs. Post change\n(Cohen's d)", y = NULL)
plot_save('Fig5/b_similarity_effect_size', c(55, 55),
           d %>% select(region = worldbank_region, cohens_d = est, lower_95ci = lo, upper_95ci = hi, z, p = p_z), pb, source_name = 'Fig_5b')
