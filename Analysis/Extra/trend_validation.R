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
  reframe(min = min(prop * 100), 
          max = max(prop * 100),
          )

# report the min and max for Ukraine trend
ua_long <- ua_long %>%
  group_by(name) %>%
  summarise(get_boot_mean_ci(min, 'min'),
            get_boot_mean_ci(max, 'max'))
ua_long

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
  arrange(country_code, boot, date) %>%
  group_by(country_code, boot) %>%
  summarise(day_since = {
    first_return <- which(m_prop >= pre_m_prop)[1]
    if (is.na(first_return)) NA_real_ else as.numeric(as.Date(date[first_return]) - WAR_START)
  }, .groups = 'drop')

# report the days to pre-invasion baselines
baseline_return_stats <- back_to_pre %>%
  group_by(country_code) %>%
  summarise(day_m = if (all(is.na(day_since))) NA_real_ else mean(day_since, na.rm = TRUE),
            day_lower_ci = unname(quantile(day_since, .025, na.rm = TRUE)),
            day_upper_ci = unname(quantile(day_since, .975, na.rm = TRUE)),
            n_returned = sum(!is.na(day_since)), n_never = sum(is.na(day_since)),
            .groups = 'drop')
print(baseline_return_stats, width = Inf)

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
  mutate(date = if_else(window == '2022', as.Date(date), as.Date(date) - 365))

ua_seasons <- ua_seasons %>%
  group_by(country_code, window, date, name) %>%
  reframe(get_boot_mean_ci(prop * 100, 'boot'))

(ua_season_plot <- ua_seasons %>%
  mutate(country_code = factor(country_code, labels = c('Ukraine'))) %>%
  filter(name == 'uk') %>%
  ggplot(aes(as.Date(date), boot_m, group = window,linetype = window)) +
  geom_line() +
  geom_ribbon(aes(ymin = boot_lower_ci, ymax = boot_upper_ci), alpha = 0.3, linewidth = 0) +
  scale_x_date(breaks = 'month', date_labels = "%b", date_breaks = "2 month") +
  labs(x = '', y = 'Proportion') +
  facet_wrap(~ country_code, nrow = 2) +
  geom_vline(xintercept = WAR_START, colour = 'gray') +
  theme(legend.position = 'none'))

# other countries seasonal effects of Russian songs
other_seasons <- seasons %>%
  filter(name == 'ru') %>%
  group_by(window, name) %>%
  # make fake year for 2023 to align the dates
  mutate(date = if_else(window == '2022', as.Date(date), as.Date(date) - 365))

other_seasons <- other_seasons %>%
  group_by(country_code, window, date, name) %>%
  reframe(get_boot_mean_ci(prop * 100, 'boot'))

(other_season_plot <- other_seasons %>%
    mutate(country_code = factor(country_code, levels = c('UA', 'RU', 'BY', 'KZ'), labels = c('Ukraine', 'Russia', 'Belarus', 'Kazakhstan'))) %>%
    ggplot(aes(as.Date(date), boot_m, group = window,linetype = window)) +
    geom_line() +
    geom_ribbon(aes(ymin = boot_lower_ci, ymax = boot_upper_ci), alpha = 0.3, linewidth = 0) +
    scale_x_date(breaks = 'month', date_labels = "%b", date_breaks = "2 month") +
    labs(x = '', y = 'Proportion') +
    geom_vline(xintercept = WAR_START, colour = 'gray') +
    facet_wrap(~ country_code, nrow = 1) +
    theme(legend.position = 'none'))

# bind the two seasonal trends for SI
seasonal_plot <- ua_season_plot + other_season_plot + plot_layout(widths = c(1, 4))
seasonal_source <- bind_rows(ua_seasons %>% filter(name == 'uk') %>% mutate(panel = 'Ukrainian'),
                             other_seasons %>% mutate(panel = 'Russian'))
plot_save('SI/seasonal_effects_lyrics_trend', c(180, 50),
          data = seasonal_source, plot = seasonal_plot)

# Five reported same-month correlations, not all pairwise correlations (or date).
# Bonferroni adjustment is across these five tests. Daily values are autocorrelated.
seasonal_stats <- seasonal_source %>%
  select(country_code, name, window, date, boot_m) %>%
  pivot_wider(names_from = window, values_from = boot_m) %>%
  filter(!is.na(`2022`), !is.na(`2023`)) %>%
  group_by(country_code, name) %>%
  group_modify(~ {
    ct <- cor.test(.x$`2022`, .x$`2023`)
    tibble(n = nrow(.x), r = unname(ct$estimate), df = unname(ct$parameter),
           p = ct$p.value, lower_ci = ct$conf.int[1], upper_ci = ct$conf.int[2])
  }) %>%
  ungroup() %>% mutate(p_bonf = p.adjust(p, 'bonferroni'))
print(seasonal_stats, width = Inf)

################################################################################
# RANDOM FLUCTUATIONS
################################################################################
# Test for random fluctuations by sampling 6 months randomly and comparing pre and post
# compare this to the observed difference during invasion period using cohen's d
if (SIMULATION) {
  # The chart file contains chart appearances, not the daily language summaries.
  # Use the same saved longitudinal trend estimates as the seasonal analysis.
  post_invasion_period <- local_trend_boot %>%
    filter(date > WAR_START + 90, name %in% c('uk', 'ru')) %>%
    group_by(date, country_code, name) %>%
    summarise(m = mean(as.numeric(prop)), .groups = 'drop') %>%
    mutate(language = recode(name, uk = 'Ukranian', ru = 'Russian'),
           country_code = recode(country_code, UA = 'Ukraine', RU = 'Russia',
                                 BY = 'Belarus', KZ = 'Kazakhstan')) %>%
    select(-name) %>%
    group_by(country_code, language) %>%
    filter(n() > 400)
}

# take a random 6 months, split by two and compare the difference with Cohen's d
n_boot <- 1000

if(SIMULATION){
  fluctutation_store = list()
  for (i in 1:n_boot) {
    sample_dates <- post_invasion_period$date %>% unique() %>% sort()
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
  pivot_wider(names_from = pre_post, values_from = mean_boot_value, values_fn = list) %>%
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
          axis.ticks.y = element_blank()) +
    facet_wrap(~ country_code, nrow = 1)
  p
}

(ukraine_random_flux <- make_comparison_plot('Ukraine', c('Ukranian')))
(other_random_flux <- make_comparison_plot(c('Ukraine', 'Russia', 'Belarus', 'Kazakhstan'), c('Russian')))

# bind the random flux plots and save as SI figure
random_plot <- ukraine_random_flux + other_random_flux + plot_layout(widths = c(1, 4))
random_reference <- lyrics_change_effect %>% rename(language = name) %>%
  semi_join(fluctuation_output %>% distinct(country_code, language),
            by = c('country_code', 'language'))
random_source <- bind_rows(
  fluctuation_output %>% mutate(series = 'random window'),
  fluctuation_output %>% group_by(country_code, language) %>%
    reframe(get_boot_mean_ci(cohensD, 'cohen')) %>% mutate(series = 'random summary'),
  random_reference %>% mutate(series = 'observed'))
plot_save('SI/random_fluctuations_lyrcs_trend', c(180, 50),
          data = random_source, plot = random_plot)

# Compare observed d with saved random-window draws, not a Gaussian z test.
# Report raw empirical tail proportions and finite-simulation corrected P values;
# zero exceedances in 1,000 draws are not evidence for P = 0.
random_window_stats <- fluctuation_output %>%
  inner_join(lyrics_change_effect %>% select(country_code, language = name, observed_d = cohensD),
             by = c('country_code', 'language')) %>%
  group_by(country_code, language) %>%
  summarise(observed_d = first(observed_d), n_null = n(),
            null_mean = mean(cohensD), null_lower_ci = quantile(cohensD, .025),
            null_upper_ci = quantile(cohensD, .975),
            n_lower = sum(cohensD <= first(observed_d)),
            n_upper = sum(cohensD >= first(observed_d)), .groups = 'drop') %>%
  mutate(p_two_empirical = pmin(1, 2 * pmin(n_lower, n_upper) / n_null),
         p_two = pmin(1, 2 * (1 + pmin(n_lower, n_upper)) / (1 + n_null)),
         p_one = (1 + if_else(observed_d < 0, n_lower, n_upper)) / (1 + n_null))
print(random_window_stats, width = Inf)
