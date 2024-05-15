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

# Plotting
# if study_window is TRUE, 3 months before and after invasion is plotted, else all data is plotted
long_trend_plot_stat <- function(dt, 
                                 save_path = c('Main', 'SI'), 
                                 study_window = TRUE, 
                                 legend = TRUE){
  # filter the right data according to the criteria
  if(study_window){
    ft_data <- dt %>% 
      filter(date >= as.Date(WAR_START) - 90, date <= as.Date(WAR_START) + 90)
  } else {
    ft_data <- dt %>%
      filter(date >= as.Date(WAR_START) - 90)
  }
  
  # aggregate the bootstraps
  message('aggregating bootstraps...')
  agg_data <- ft_data %>%
    group_by(date, country_code, name) %>% 
    reframe(get_boot_mean_ci(prop, 'boot'))
  
  # reorder the factors
  agg_data$country_code <- factor(agg_data$country_code, levels = SUB_COUNTRIES, labels = c('Ukraine', 'Russia', 'Belarus', 'Kazakhstan'))
  agg_data <- agg_data %>%
    mutate(name = factor(
      name,
      levels = c(sub_languages, 'other'),
      labels = c('Ukranian', 'Russian', 'Kazakh', 'English', 'Other')
    ))
  
  # set theme colour palette
  colour_pal <- c(UKRAINE_COLOUR, RUSSIAN_COLOUR, KAZAKHSTAN_COLOUR, ENGLISH_COLOUR, 'gray')
  
  # plot
  message('plotting trends')
  agg_data %>%
    # make percentage
    mutate(percent = boot_m * 100,
           ci_lower = boot_lower_ci * 100,
           ci_upper = boot_upper_ci * 100) %>%
    ggplot(aes(as.Date(date), percent, colour = name, fill = name)) +
    geom_line() +
    geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper),  alpha = 0.3, color = NA)  +
    geom_vline(xintercept = WAR_START, linetype = 'dashed') +
    # set theme
    theme_classic() +
    scale_colour_manual(values = colour_pal) +
    scale_fill_manual(values = colour_pal, guide = FALSE) +
    scale_x_date(breaks = 'month', date_labels = "%b", date_breaks = "1 month") +  # Display only month names vertically
    theme(legend.position = ifelse(legend, 'bottom', 'none')) +
    labs(x = '', y = 'Proportion (%)', colour = 'Lyrics Language', 'Register country (ISRC)') +
    facet_wrap(~ country_code, nrow = ifelse(study_window, 1, 4)) +
    # if longitudinal, add line breaks
    {if(!study_window) geom_vline(xintercept = c(WAR_START - 90, WAR_START + 90, WAR_START + 272, WAR_START + 455, WAR_START + 630),
                                  colour = 'gray50', alpha = 0.5)}

  
  # save plot
  if(study_window){
    plot_save(sprintf('%s/lyrics_lang_postsoviet_6month', save_path), c(183, 60))
  } else {
    plot_save(sprintf('%s/lyrics_lang_postsoviet_2years', save_path), c(183, 180))
  }
  
  # return aggregated data for stats
  return(agg_data)
}

# make plots for main and save the data
lyrics_study <- long_trend_plot_stat(long_boot_output, 'Main', study_window = TRUE, FALSE)
lyrics_long <- long_trend_plot_stat(long_boot_output, 'SI', study_window = FALSE, TRUE)


################################################################################
# TREND STATS
################################################################################
# compute the pre vs. post increase/decrease percentage. Use cohen's d for effect size measure
lyrics_change <- long_boot_output %>%
  filter(date >= WAR_START - 90, date <= WAR_START + 90) %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post')) 

# report change in percentage stats in the main text
lyrics_change_pre_post <- lyrics_change %>%
  group_by(boot, country_code, name, pre_post) %>%
  summarise(m_prop = mean(prop) * 100) %>%
  ungroup()

lyrics_change_pre_post <- lyrics_change_pre_post %>%
  group_by(country_code, name, pre_post) %>%
  reframe(get_boot_mean_ci(m_prop, 'prop'))

# report Russian lyrics in May
lyrics_change %>%
  filter(country_code == 'RU', name == 'ru', date >= '2022-05-01', date < '2022-06-01') %>%
  reframe(get_boot_mean_ci(prop * 100, 'm_prop'))

# report effect sizes
lyrics_change_effect <- lyrics_change %>%
  filter((country_code == 'UA' & name == 'uk') |
           (country_code == 'UA' & name == 'ru') |
           (country_code == 'RU' & name == 'ru') |
           (country_code == 'KZ' & name == 'ru') |
           (country_code == 'BY' & name == 'ru')
  )

# make dataframe wide
lyrics_change_effect <- lyrics_change_effect %>%
  select(country_code, name, pre_post, boot, prop) %>%
  pivot_wider(names_from = pre_post, values_from = prop)

lyrics_change_effect <- lyrics_change_effect %>%
  group_by(boot, country_code, name) %>%
  summarise(get_t_stat(post %>% unlist(), pre %>% unlist())) %>%
  ungroup()

# get cohen'D bootstrap
lyrics_change_effect %>%
  group_by(country_code, name) %>%
  reframe(get_boot_mean_ci(cohensD, 'd'))

