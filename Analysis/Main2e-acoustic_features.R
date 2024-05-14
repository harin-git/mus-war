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
# CHANGE IN GENRE
################################################################################
# recode genres into higher level categories
acoustic_joined <- acoustic_joined %>% 
  mutate(genres = str_replace_all(genres, 'Hip-Hop/Rap', 'Hip-Hop'),
         genres = str_replace_all(genres, 'Alternative Rap', 'Hip-Hop'),
         genres = str_replace_all(genres, 'Rap', 'Hip-Hop'),
         genres = str_replace_all(genres, 'Electronic', 'Dance'),
         genres = str_replace_all(genres, 'House', 'Dance'),
         genres = str_replace_all(genres, 'Trance', 'Dance'),
         genres = str_replace_all(genres, 'Techno', 'Dance'),
         genres = str_replace_all(genres, 'Disco', 'Dance'),
         genres = str_replace_all(genres, 'Hard Rock', 'Rock'),
         genres = str_replace_all(genres, 'Pop/Rock', 'Rock'),
         genres = str_replace_all(genres, 'Indie Rock', 'Rock'),
         genres = str_replace_all(genres, 'Folk-Rock', 'Rock'),
         genres = str_replace_all(genres, 'Indie Pop', 'Pop')
         )

# get top 5 genres
top_genres <- acoustic_joined$genres %>% 
  table() %>% sort(decreasing = TRUE) %>% names()
top10_genre <- top_genres[1:10]

# omit NA genres
genre_dt <- acoustic_joined %>% filter(!is.na(genres))

# get proportion pre and post
n_boot <- 1000

if(SIMULATION){
  genre_boot = list()
  for (i in 1:n_boot) {
    smp <- genre_dt %>%
      group_by(country_code, pre_post) %>%
      sample_n(n(), replace = T)
    
    genre_boot[[i]] <- smp %>%
      count(country_code, pre_post, genres) %>%
      group_by(country_code, pre_post) %>%
      mutate(prop = n / sum(n) * 100) %>%
      cbind(boot = i)
    
    print(i)
  }
  genre_boot_output <- do.call(rbind, genre_boot)
  genre_boot_output %>% write_rds(sprintf('Bootstraps/acoustic_genre_boot%s.rds', n_boot))
  
} else {
  boot_load()
  genre_boot_output <- read_rds(sprintf('Bootstraps/acoustic_genre_boot%s.rds', n_boot))
}

# compute raw stats
genre_stat <- genre_boot_output %>%
  group_by(country_code, pre_post, genres) %>%
  reframe(get_boot_mean_ci(prop, 'boot')) %>%
  filter(boot_m > 1) %>%
  arrange(country_code, - boot_m)

genre_stat

# calculate pre vs. post change and report
genre_change <- genre_boot_output %>%
  filter(genres %in% top10_genre) %>%
  select(boot, country_code, genres, pre_post, prop) %>%
  filter(prop > 1) %>%
  pivot_wider(names_from = pre_post, values_from = prop) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(change = post - pre) %>%
  group_by(country_code, genres) %>%
  reframe(get_boot_mean_ci(change, 'change')) %>%
  arrange(country_code, -change_m)

genre_change %>%
  mutate(country_code = factor(country_code, levels = c('UA', 'RU'), labels = c('Ukraine', 'Russia'))) %>%
  ggplot(aes(reorder(genres, change_m), change_m, 
             ymin = change_lower_ci, ymax = change_upper_ci,
             colour = country_code)) +
  geom_pointrange() +
  facet_wrap(~ country_code) +
  geom_hline(yintercept = 0) +
  scale_colour_manual(values = c(UKRAINE_COLOUR, RUSSIAN_COLOUR)) +
  coord_flip() +
  labs(x = '', y = 'Change (%)') +
  theme(legend.position = 'none')

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

# report stats
acoustic_diff

# absolute change across feature sets
acoustic_diff %>%
  mutate(abs_change = abs(change_m)) %>%
  select(country_code, abs_change, name) %>%
  pivot_wider(names_from = country_code, values_from = abs_change) %>%
  reframe(get_t_stat(RU, UA))

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
  labs(x = '', y = 'Pre vs. post difference (Z-score)') +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5)) +
  theme(legend.position = 'none')
plot_save('Main/acoustic_feature_analyses', c(90, 50))


# scale for each country
acoustic_feat_out <- acoustic_store_output %>%
  group_by(country_code, name) %>%
  mutate(m = scale(m))

acoustic_feat_agg <- acoustic_feat_out %>%
  group_by(country_code, pre_post, name) %>%
  reframe(get_boot_mean_ci(m, 'boot'))

# report feature differences
acoustic_feat_agg %>%
  select(country_code, pre_post, name, boot_m, boot_lower_ci, boot_upper_ci) %>%
  pivot_wider(names_from = pre_post, values_from = c(boot_m, boot_lower_ci, boot_upper_ci))

# report the effect size of change
acoustic_store_output %>%
  group_by(country_code, name) %>%
  reframe(get_boot_mean_ci(cohensD, 'd'))
  
# # reorder the variables
# acoustic_feat_agg <- acoustic_feat_agg %>%
#   mutate(name = factor(name, levels = c('loudness', 'richness', 'tempo', 'brightness', 'mode', 'dynamics')),
#          pre_post = factor(pre_post, levels = c('pre', 'post'))) 

# plot separately for loudness and richness and then everything else
(loud_rich <- acoustic_feat_agg %>%
  mutate(pre_post = factor(pre_post, levels = c('pre', 'post'))) %>%
  # filter(name %in% c('tempo', 'loudness')) %>%
  ggplot(aes(pre_post, boot_m, group = country_code, colour = country_code, ymin = boot_lower_ci, ymax = boot_upper_ci)) +
  geom_line(position = position_dodge(0.1)) +
  geom_pointrange(position = position_dodge(0.1)) +
  facet_wrap(~ name, nrow = 1) +
  scale_colour_manual(values = c(RUSSIAN_COLOUR, UKRAINE_COLOUR), guide = FALSE) +
  labs(y = 'Z-score', colour = '', x = ''))

# save for main figure
plot_save('Main/acoustic_feature_analyses', c(90, 50))

(not_loud_rich <- acoustic_feat_agg %>%
  filter(!name %in% c('tempo', 'loudness')) %>%
  ggplot(aes(pre_post, boot_m, group = country_code, colour = country_code, ymin = boot_lower_ci, ymax = boot_upper_ci)) +
  geom_line(position = position_dodge(0.1)) +
  geom_pointrange(position = position_dodge(0.1)) +
  facet_wrap(~ name, nrow = 1) +
  scale_colour_manual(values = c(RUSSIAN_COLOUR, UKRAINE_COLOUR), guide = F) +
  labs(y = 'Z-score', colour = '', x = ''))

# save plot as SI figure
plot_save('SI/acoustic_features', c(183, 80))



  
  