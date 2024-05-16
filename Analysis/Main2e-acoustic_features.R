#' Acoustic analyses on genre and music features 
#' Related to Fig. 2 in the paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load data
chart_data <- read_rds('Dataset/chart/study_set_chart.rds')
song_meta <- read_rds('Dataset/meta/song_metadata.rds')
acoustic_data <- song_meta %>% 
  select(trackID, genres, lyrics_language, 
         loudness,
         dynamics = dynamic_complexity,
         richness = spectral_complexity,
         brightness = spectral_centroid, 
         'chord change' = chord_change_rate,
         'zero crossing' = zcr,
         mode = major_minor,
         tempo = bpm) %>%
  group_by(trackID) %>%
  slice(1) %>%
  ungroup() %>%
  # scale each of the features
  mutate(across(loudness:tempo, scale))

# sub ukraine and russia and songs
ua_ru <- chart_data %>%
  filter(country_code %in% c('UA', 'RU'))

# make a dict classifying songs as pre or post invasion
pre_post_songs <- ua_ru %>%
  group_by(trackID) %>%
  arrange(date) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post')) %>%
  select(trackID, pre_post)

# join the chart data with pre and post songs
ua_joined <- ua_ru %>% select(-pre_post) %>% left_join(pre_post_songs)

# join the acoustic features and filter only the local music
acoustic_joined <- ua_joined %>% left_join(acoustic_data)
acoustic_joined <- acoustic_joined %>%
  filter((country_code == 'RU' & lyrics_language == 'ru') | (country_code == 'UA' & lyrics_language == 'uk'))


################################################################################
# CHANGE IN MUSIC FEATURES
################################################################################
# compare the pre and post differences in high-level dimensions of music
acoustic_feat <- acoustic_joined %>%
  select(trackID, country_code, pre_post, loudness:tempo, lyrics_language) %>%
  pivot_longer(loudness:tempo) %>%
  na.omit()

# bootstrap across the songs
n_boot <- 1000

if(SIMULATION){
  acoustic_store = list()
  for (i in 1:n_boot) {
    smp <- acoustic_feat %>%
      group_by(country_code, pre_post) %>%
      sample_n(n(), replace = T)
    
    # calculate effect size of change
    effect <- smp %>%
      select(country_code, pre_post, name, value) %>%
      pivot_wider(names_from = pre_post, values_from = value) %>%
      group_by(country_code, name) %>%
      reframe(get_t_stat(post %>% unlist(), pre %>% unlist()))
    
    effect <- effect %>%
      select(country_code, name, cohensD, change = mean_diff) %>%
      mutate(boot = i)
    
    # store everything
    acoustic_store[[i]] <- smp %>%
      group_by(country_code, pre_post, name) %>%
      reframe(m = mean(value)) %>%
      arrange(country_code) %>%
      mutate(boot = i) %>%
      left_join(effect)
    
    print(i)
  }
  acoustic_store_output <- do.call(rbind, acoustic_store)
  acoustic_store_output %>% write_rds(sprintf('Bootstraps/acoustic_feat_boot%s.rds', n_boot))  
} else {
  boot_load()
  acoustic_store_output <- read_rds(sprintf('Bootstraps/acoustic_feat_boot%s.rds', n_boot))
}

# get acoustic differences
acoustic_diff <- acoustic_store_output %>%
  group_by(country_code, name) %>%
  reframe(get_boot_mean_ci(change, 'change'))

# report stats in the paper
acoustic_diff

# save plots for main figure
acoustic_diff %>%
  mutate(country_code = factor(country_code, levels = c('UA', 'RU'), labels = c('Ukraine', 'Russia'))) %>%
  ggplot(aes(reorder(name, change_m), change_m, 
             ymin = change_lower_ci, ymax = change_upper_ci,
             colour = country_code)) +
  geom_pointrange() +
  facet_wrap(~ country_code) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values = c(UKRAINE_COLOUR, RUSSIAN_COLOUR)) +
  coord_flip() +
  labs(x = '', y = 'Pre vs. Post difference (z-score)') +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5)) +
  theme(legend.position = 'none')

plot_save('Main/acoustic_feature_analyses', c(90, 50))

