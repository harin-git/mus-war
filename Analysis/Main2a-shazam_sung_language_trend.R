#' Temporal trends in sung language
#' Related to Fig.2 in paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load longitudinal chart and meta data
chart_data <- read_rds('Dataset/chart/longitudinal_postsoviet_chart.rds')
city_meta <- read_rds('Dataset/meta/city_metadata.rds')

sub_languages <- c('uk', 'ru', 'kk', 'en') # post-soviet lyrics languages. There are no lyrics detected in Belarusian

# report basic stats about how many songs have lyrics, language detected, etc
study_data <- chart_data %>% filter(study_window)

study_data %>%
  distinct(trackID, lyrics_lang, lyrics_lang_prob) %>%
  reframe(lyrics_nas = sum(is.na(lyrics_lang))/n(),
          over_threshold = sum(lyrics_lang_prob >= 0.7, na.rm = T)/sum(!is.na(lyrics_lang)))

study_data %>%
  distinct(country_code, trackID, lyrics_lang, lyrics_lang_prob) %>%
  group_by(country_code) %>%
  reframe(nas = sum(is.na(lyrics_lang))/n()*100)

# how many songs were excluded for the sub languages by confidence threshold?
study_data %>%
  distinct(trackID, lyrics_lang, lyrics_lang_prob) %>%
  filter(lyrics_lang %in% sub_languages) %>%
  mutate(over_threshold = ifelse(lyrics_lang_prob >= 0.7, TRUE, FALSE)) %>%
  group_by(lyrics_lang) %>%
  summarise(exclusion = 1 - (sum(over_threshold)/n()))

################################################################################
# LONGITUDINAL TRENDS: LYRICS LANGUAGE
################################################################################
# clean the data by using a language detection threshold
long_clean <- chart_data %>%
  ungroup() %>%
  mutate(lyrics_lang = ifelse(lyrics_lang_prob >= 0.7, lyrics_lang, NA)) %>%
  mutate(lyrics_lang = case_when(
    is.na(lyrics_lang) ~ NA,
    lyrics_lang %in% sub_languages ~ lyrics_lang,
    !lyrics_lang %in% sub_languages ~ 'other'
  ))

country_trend_dt <- long_clean %>% select(date, study_window, country_code, lyrics_lang)

# perform bootstrap
n_boot <- 1000

if(SIMULATION){
  long_boot = list()
  for (b in 1:n_boot) {
    smp <- country_trend_dt %>% 
      group_by(country_code, date) %>% 
      sample_n(size = n(), replace = T)
    
    lang <- smp %>%
      filter(!is.na(lyrics_lang)) %>%
      group_by(date, country_code) %>%
      reframe(get_proportion(lyrics_lang))
    
    long_boot[[b]] <- lang %>% cbind(boot = b)
    print(sprintf('%s out of %s boots', b, n_boot))
  }
  long_boot_output <- do.call(rbind, long_boot) %>% mutate(date = as.Date(date))
  long_boot_output %>% write_rds(sprintf('Bootstraps/trend_lyrics_lang_boot%s.rds', n_boot))
  
} else {
  boot_load()
  long_boot_output <- read_rds(sprintf('Bootstraps/trend_lyrics_lang_boot%s.rds', n_boot))
}

# a language absent from a day's bootstrap sample has proportion 0 on that day
# (this matters for rare series such as Ukrainian songs in Russia and Belarus)
lang_labels <- c(uk = 'Ukrainian', ru = 'Russian', kk = 'Kazakh', en = 'English', other = 'Other')
cells <- long_boot_output %>% distinct(boot, country_code, date)
long_boot_fill <- map_dfr(names(lang_labels), function(l)
  cells %>%
    left_join(long_boot_output %>% filter(name == l) %>% select(boot, country_code, date, prop),
              by = c('boot', 'country_code', 'date')) %>%
    mutate(name = l, prop = replace_na(as.numeric(prop), 0)))
# keep only language-country series that occur at all (e.g. no Kazakh in Ukraine)
long_boot_fill <- long_boot_fill %>% semi_join(long_boot_output %>% distinct(country_code, name), by = c('country_code', 'name'))

# Plotting
# if study_window is TRUE, 3 months before and after invasion is plotted (Fig. 2a), else all data (Supplementary Fig. 1)
long_trend_plot_stat <- function(dt, save_path = c('Main', 'SI'), study_window = TRUE, legend = TRUE){
  # filter the right data according to the criteria
  if(study_window){
    ft_data <- dt %>% filter(date >= WAR_START - 90, date <= WAR_START + 90)
  } else {
    ft_data <- dt %>% filter(date >= WAR_START - 90)
  }

  # aggregate the bootstraps: mean and 95% percentile CI per day
  message('aggregating bootstraps...')
  agg_data <- ft_data %>%
    group_by(date, country_code, name) %>%
    summarise(percent = mean(prop) * 100,
              ci_lower = quantile(prop, .025) * 100,
              ci_upper = quantile(prop, .975) * 100, .groups = 'drop') %>%
    mutate(country = factor(country_code, levels = SUB_COUNTRIES, labels = c('Ukraine', 'Russia', 'Belarus', 'Kazakhstan')),
           language = factor(lang_labels[name], levels = names(LANG_COLS)))

  # plot
  message('plotting trends')
  p <- agg_data %>%
    ggplot(aes(date, percent, colour = language, fill = language)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), colour = NA, alpha = 0.3) +
    geom_line(linewidth = 0.3) +
    geom_vline(xintercept = WAR_START, linetype = 'dashed', linewidth = LW) +
    scale_colour_manual(values = LANG_COLS, name = 'Sung language', drop = FALSE) +
    scale_fill_manual(values = LANG_COLS, guide = 'none', drop = FALSE) +
    labs(x = NULL, y = 'Sung language (%)') +
    strip_box +
    theme(legend.position = ifelse(legend, 'bottom', 'none')) +
    # if longitudinal, add line breaks for the six-month bins
    {if(!study_window) geom_vline(xintercept = c(WAR_START - 90, WAR_START + 90, WAR_START + 272, WAR_START + 455, WAR_START + 630),
                                  colour = 'gray50', alpha = 0.5, linewidth = LW)}

  source_data <- agg_data %>% select(country, language, date, mean = percent, lower_95ci = ci_lower, upper_95ci = ci_upper)

  # save plot
  if(study_window){
    p <- p + date_x + facet_wrap(~ country, nrow = 1) + scale_y_continuous(limits = c(0, 70), breaks = seq(0, 60, 20))
    plot_save(sprintf('%s/lyrics_lang_postsoviet_6month', save_path), c(FULL_W, 50), source_data, p, source_name = 'Fig_2a')
  } else {
    p <- p + scale_x_date(date_breaks = '1 month', date_labels = '%b') + facet_wrap(~ country, nrow = 4)
    plot_save(sprintf('%s/lyrics_lang_postsoviet_2years', save_path), c(FULL_W, 180), source_data, p, source_name = 'Supplementary_Fig_1')
  }

  # return aggregated data
  return(agg_data)
}

# make plots for main and SI
lyrics_study <- long_trend_plot_stat(long_boot_fill, 'Main', study_window = TRUE, legend = FALSE)
lyrics_long <- long_trend_plot_stat(long_boot_fill, 'SI', study_window = FALSE, legend = TRUE)


################################################################################
# TREND STATS
################################################################################
# Pre- vs post-invasion change, bootstrapped over songs within day.
# Effect size: Cohen's d across days in each bootstrap replicate; reported as bootstrap mean, 95% CI, z and P.
lyrics_change <- long_boot_fill %>%
  filter(date >= WAR_START - 90, date <= WAR_START + 90) %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post'))

# number of days per period
lyrics_change %>% filter(boot == 1, name == 'ru') %>% count(country_code, pre_post)

# series reported in the main text
reported <- tribble(~country_code, ~name,
                    'UA', 'uk', 'UA', 'ru',
                    'RU', 'ru', 'RU', 'uk',
                    'BY', 'ru', 'BY', 'uk',
                    'KZ', 'ru', 'KZ', 'kk')

# pre and post mean percentage
lyrics_change_pre_post <- lyrics_change %>%
  semi_join(reported, by = c('country_code', 'name')) %>%
  group_by(boot, country_code, name, pre_post) %>%
  summarise(m_prop = mean(prop) * 100, .groups = 'drop') %>%
  group_by(country_code, name, pre_post) %>%
  reframe(get_boot_mean_ci(m_prop, 'prop'))
print(lyrics_change_pre_post, n = Inf, width = Inf)

# effect sizes
lyrics_change_effect <- lyrics_change %>%
  semi_join(reported, by = c('country_code', 'name')) %>%
  group_by(boot, country_code, name) %>%
  summarise(get_t_stat(prop[pre_post == 'post'], prop[pre_post == 'pre']), .groups = 'drop') %>%
  group_by(country_code, name) %>%
  reframe(get_boot_mean_ci(cohensD, 'd'))
print(lyrics_change_effect, n = Inf, width = Inf)

# Russia: Russian songs in May 2022 (CI of the monthly mean)
lyrics_change %>%
  filter(country_code == 'RU', name == 'ru', date >= as.Date('2022-05-01'), date < as.Date('2022-06-01')) %>%
  group_by(boot) %>% summarise(m = mean(prop) * 100) %>%
  reframe(get_boot_mean_ci(m, 'may_prop'))

# Russia: Ukrainian songs in the first two weeks after the invasion
lyrics_change %>%
  filter(country_code == 'RU', name == 'uk', pre_post == 'post', date < WAR_START + 14) %>%
  group_by(boot) %>% summarise(m = mean(prop) * 100) %>%
  reframe(get_boot_mean_ci(m, 'first14d_prop'))
