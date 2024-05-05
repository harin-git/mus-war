#' Three validation experiments for the local music trends
#' Related to Supplementary Information in paper

# load study-wide functions and global variables
source('utils.R')

# load bootstrapped trend data
local_trend_boot <- read_rds('Bootstraps/trend_lyrics_lang_boot1000.rds')

SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

################################################################################
# LONGEVITY OF TREND
################################################################################
# Calculate the minimum value reached for Ukranian and Russian lyrics in Ukraine post-invasion
ua_long <- local_trend_boot %>%
  filter(country_code == 'UA', name %in% c('uk', 'ru')) %>%
  group_by(date, name) %>%
  # summarise(get_boot_mean_ci(prop, 'boot')) %>%
  # after the study window
  filter(date > WAR_START + 90) %>%
  group_by(name, boot) %>%
  reframe(min = min(prop), 
          max = max(prop),
          )

# report the min and max in the main text
ua_long <- ua_long %>%
  group_by(name) %>%
  summarise(get_boot_mean_ci(min, 'min'),
            get_boot_mean_ci(max, 'max'))

# Get the first day of returning to pre-invasion level mean for Russian lyrics
pre_mean <- local_trend_boot %>%
  filter(date < WAR_START, name == 'ru') %>%
  group_by(country_code, boot) %>%
  summarise(pre_m_prop = mean(prop))

after_one_month <- local_trend_boot %>%
  filter(date > WAR_START + 30, name == 'ru') %>%
  group_by(country_code, boot, date) %>%
  summarise(m_prop = mean(prop))

# join trends after one month with pre-invasion baseline
back_to_pre <- after_one_month %>%
  left_join(pre_mean)

# for each bootstrap, count the days from invasion reach back to the pre-mean
back_to_pre <- back_to_pre %>%
  group_by(country_code, boot) %>%
  mutate(day_since = 30:(n() + 29),
         is_over_pre = ifelse(m_prop >= pre_m_prop, TRUE, FALSE)) %>%
  filter(is_over_pre) %>%
  slice(1)

# report the days to pre-invasion baselines
back_to_pre %>%
  group_by(country_code) %>%
  reframe(get_boot_mean_ci(day_since, 'day'))

################################################################################
# SEASONAL EFFECTS
################################################################################
# Test for seasonal effects by computing correlations of the same months in 2022 and 2023
seasons <- local_trend_boot %>%
  mutate(window = case_when(
    date >= (WAR_START - 90) & date <= (WAR_START + 90) ~ '2022',
    date >= (WAR_START - 90 + 365) & date <= (WAR_START + 90 + 365) ~ '2023' 
  )) %>%
  na.omit()

# ukraine seasonal effects
ua_seasons <- seasons %>%
  filter(country_code == 'UA', name %in% c('uk', 'ru')) %>%
  group_by(window, name) %>%
  # make fake year for 2023 to align the dates
  mutate(date = ifelse(window == '2022', date, date - 365))

ua_seasons <- ua_seasons %>%
  group_by(country_code, window, date, name) %>%
  reframe(get_boot_mean_ci(prop * 100, 'boot'))

(ua_season_plot <- ua_seasons %>%
  mutate(name = factor(name, levels = c('uk', 'ru'), labels = c('Ukranian', 'Russian')),
         country_code = factor(country_code, labels = c('Ukraine'))) %>%
  ggplot(aes(as.Date(date), boot_m, group = window,linetype = window)) +
  geom_line() +
  geom_ribbon(aes(ymin = boot_lower_ci, ymax = boot_upper_ci), alpha = 0.3, linewidth = 0) +
  scale_x_date(breaks = 'month', date_labels = "%b", date_breaks = "1 month") +
  labs(x = '', y = 'Proportion') +
  facet_wrap(~ name, nrow = 2) +
  geom_vline(xintercept = WAR_START, colour = 'gray') +
  theme(legend.position = 'none'))

# other countries seasonal effects of Russian songs
other_seasons <- seasons %>%
  filter(country_code %in% c('RU', 'BY', 'KZ'), name == 'ru') %>%
  group_by(window, name) %>%
  # make fake year for 2023 to align the dates
  mutate(date = ifelse(window == '2022', date, date - 365))

other_seasons <- other_seasons %>%
  group_by(country_code, window, date, name) %>%
  reframe(get_boot_mean_ci(prop * 100, 'boot'))

(other_season_plot <- other_seasons %>%
    mutate(country_code = factor(country_code, levels = c('RU', 'BY', 'KZ'), labels = c('Russia', 'Belarus', 'Kazakhstan'))) %>%
    ggplot(aes(as.Date(date), boot_m, group = window,linetype = window)) +
    geom_line() +
    geom_ribbon(aes(ymin = boot_lower_ci, ymax = boot_upper_ci), alpha = 0.3, linewidth = 0) +
    scale_x_date(breaks = 'month', date_labels = "%b", date_breaks = "1 month") +
    labs(x = '', y = 'Proportion') +
    geom_vline(xintercept = WAR_START, colour = 'gray') +
    facet_wrap(~ country_code, nrow = 1) +
    theme(legend.position = 'none'))

# bind the two seasonal trends for SI
ua_season_plot + other_season_plot + plot_layout(widths = c(1.5, 3))
plot_save('SI/seasonal_effects_lyrics_trend', c(183, 80))

## Report the correlation stats
ua_season_cor <- ua_seasons %>%
  select(window, name, date, boot_m) %>%
  pivot_wider(names_from = c(name, window), values_from = boot_m)

cor.test(ua_season_cor$uk_2022, ua_season_cor$uk_2023, use = 'complete.obs')
cor.test(ua_season_cor$ru_2022, ua_season_cor$ru_2023, use = 'complete.obs')

# russian cor test in other countries
other_seasons_cor <- other_seasons %>%
  select(window, country_code, date, boot_m) %>%
  pivot_wider(names_from = c(country_code, window), values_from = boot_m)

cor.test(other_seasons_cor$RU_2022, other_seasons_cor$RU_2023, use = 'complete.obs')
cor.test(other_seasons_cor$BY_2022, other_seasons_cor$BY_2023, use = 'complete.obs')
cor.test(other_seasons_cor$KZ_2022, other_seasons_cor$KZ_2023, use = 'complete.obs')

################################################################################
# RANDOM FLUCTUATIONS
################################################################################
# Test for random fluctuations by sampling 6 months randomly and comparing pre and post
# compare this to the observed difference during invasion period using cohen's d
post_invasion_period <- lyrics_long %>%
  filter(date > WAR_START + 90) %>%
  select(date, country_code, language = name, m = mean_boot_value) %>%
  filter(language %in% c('Ukranian', 'Russian'))

# drop language country pair with too little
post_invasion_period <- post_invasion_period %>%
  group_by(country_code, language) %>%
  filter(n() > 400)

# take a random 6 months, split by two and compare the difference with Cohen's d
n_boot <- 1000

if(SIMULATION){
  fluctutation_store = list()
  for (i in 1:n_boot) {
    sample_dates <- post_invasion_period$date %>% unique()
    sample_dates <- sample_dates[1:(length(sample_dates) - 181)]
    
    random_date <- as.Date(sample(sample_dates, 1))
    pre_post <- post_invasion_period %>%
      filter(date >= random_date, date <= (random_date + 180)) %>%
      mutate(pre_post = ifelse(date < (random_date+90), 'pre', 'post')) %>%
      ungroup() %>%
      select(-date) %>%
      group_by(language) %>%
      pivot_wider(names_from = pre_post, values_from = m)
    
    pre_post_effect <- pre_post %>%
      group_by(country_code, language) %>%
      reframe(get_t_stat(post %>% unlist, pre %>% unlist())) %>%
      cbind(boot = i)
    
    fluctutation_store[[i]] <- pre_post_effect
    
    print(i)
    
  }
  fluctuation_output <- do.call(rbind, fluctutation_store)
  fluctuation_output %>% write_rds(sprintf('Bootstraps/random_fluctuation_boot%s.rds', n_boot))
} else {
  boot_load()
  fluctuation_output <- read_rds(sprintf('Bootstraps/random_fluctuation_boot%s.rds', n_boot))
}

# use effect size of the main study as reference
lyrics_lang <- read_rds(sprintf('Bootstraps/trend_lyrics_lang_main_boot%s.rds', n_boot))
lyrics_change_effect <- lyrics_lang %>%
  ungroup() %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post')) %>%
  select(country_code, name, mean_boot_value, pre_post) %>%
  pivot_wider(names_from = pre_post, values_from = mean_boot_value) %>%
  group_by(country_code, name) %>%
  filter(length(pre %>% unlist()) > 80)

lyrics_change_effect <- lyrics_change_effect %>%
  group_by(country_code, name) %>%
  reframe(get_t_stat(post %>% unlist(), pre %>% unlist()))


# make plots
make_comparison_plot <- function(country, lang){
  # get observed reference from the study window
  ref <- lyrics_change_effect %>%
    rename(language = name) %>%
    filter(country_code %in% country, language %in% lang)
  
  # aggregate the random window bootstraps
  random_boot <- fluctuation_output %>%
    filter(country_code %in% country, language %in% lang)
  random_boot_agg <- random_boot %>%
    group_by(country_code, language) %>%
    reframe(get_boot_mean_ci(cohensD, 'cohen'))
  
  # Create the base plot
  p <- ggplot(random_boot, aes(cohensD, language, group = language)) +
    # plot the density of all bootstraps
    ggridges::geom_density_ridges2(alpha = 0.8, linewidth = 0) +
    geom_pointrange(data = random_boot_agg, aes(cohen_m, xmin = cohen_lower_ci, xmax = cohen_upper_ci), colour = 'gray20') +
    geom_point(data = ref, shape = 15, size = 3, colour = 'red', alpha = 0.7) +
    # coord_flip() +
    labs(x = "Cohen's d", y = '') +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  # Conditionally add facets
  if (country[1] == 'Ukraine') {
    p <- p + facet_wrap(~ language, nrow = 2)
  } else {
    p <- p + facet_wrap(~ country_code, nrow = 1)
  }
}

(ukraine_random_flux <- make_comparison_plot('Ukraine', c('Ukranian', 'Russian')))
(other_random_flux <- make_comparison_plot(c('Russia', 'Belarus', 'Kazakhstan'), c('Russian')))

# bind the random flux plots and save as SI figure
ukraine_random_flux + other_random_flux + plot_layout(widths = c(1.5, 3))
plot_save('SI/random_fluctuations_lyrcs_trend', c(183, 80))


