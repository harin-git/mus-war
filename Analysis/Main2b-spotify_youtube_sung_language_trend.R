#' Analyze the trend of lyrics language in the study set on Spotify and Youtube music charts
#' Replication to Shazam trends in paper. 
#' Related to Fig. 2 in paper

source('utils.R')

SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

################################################################################
# DATA PREPARATION
################################################################################
yt_chart <- read_csv('Dataset/private/ua_ru_youtube_chart.csv')
yt_meta <- read_csv('Dataset/private/ua_ru_youtube_lyrics.csv')
yt_chart <- yt_chart %>% left_join(yt_meta, by = c('song_id' = 'song_id'))
yt_chart <- yt_chart %>% 
    select(country_code = iso2, date, lyrics_lang = language, lyrics_lang_prob = confidence) %>%
    mutate(country_code = toupper(country_code))
yt_chart <- yt_chart %>% cbind(platform = 'Youtube')

sp_chart <- read_csv('Dataset/private/ua_ru_spotify_chart.csv')
sp_meta <- read_csv('Dataset/private/ua_ru_spotify_lyrics.csv')
sp_chart <- sp_chart %>% left_join(sp_meta, by = c('song_id' = 'song_id'))
sp_chart <- sp_chart %>% 
    select(country_code = iso2, date, lyrics_lang = language, lyrics_lang_prob = confidence) %>%
    mutate(country_code = toupper(country_code))
sp_chart <- sp_chart %>% cbind(platform = 'Spotify')

# combine all of them
chart <- bind_rows(yt_chart, sp_chart) %>% as_tibble()

# categorise the lyrics lang
chart_clean <- chart %>%
    mutate(lyrics_lang = ifelse(lyrics_lang_prob >= 0.7, lyrics_lang, NA)) %>%
    mutate(lang_category = case_when(
        lyrics_lang == 'uk' ~ 'Ukrainian',
        lyrics_lang == 'ru' ~ 'Russian',
        lyrics_lang == 'en' ~ 'English',
        lyrics_lang == 'kz' ~ 'Kazakh',
        TRUE ~ 'Other'
    )) 

# perform bootstrap across the songs on each day
n_boot <- 1000

if(SIMULATION){
  boot_store = list()
  for (b in 1:n_boot) {
    smp <- chart_clean %>% 
      group_by(platform, country_code, date) %>% 
      sample_n(size = n(), replace = T)
    
    lang <- smp %>%
      filter(!is.na(lyrics_lang)) %>%
      group_by(platform, date, country_code) %>%
      reframe(get_proportion(lang_category))
    
    boot_store[[b]] <- lang %>% cbind(boot = b)
    print(sprintf('%s out of %s boots', b, n_boot))
  }
  boot_output <- do.call(rbind, boot_store) %>% mutate(date = as.Date(date))
  boot_output %>% write_rds(sprintf('Bootstraps/trend_lyrics_lang_replication_boot%s.rds', n_boot))

} else {
  boot_load()
  boot_output <- read_rds(sprintf('Bootstraps/trend_lyrics_lang_replication_boot%s.rds', n_boot))
}

# study window: a language absent from a chart's bootstrap sample has proportion 0 on that date
boot_window <- boot_output %>%
  filter(date >= WAR_START - 90, date <= WAR_START + 90) %>%
  mutate(prop = as.numeric(prop) * 100)
cells <- boot_window %>% distinct(boot, platform, country_code, date)
boot_fill <- map_dfr(names(LANG_COLS), function(l)
  cells %>%
    left_join(boot_window %>% filter(name == l) %>% select(boot, platform, country_code, date, prop),
              by = c('boot', 'platform', 'country_code', 'date')) %>%
    mutate(prop = replace_na(prop, 0), name = l))

################################################################################
# TREND PLOT (Fig. 2b)
################################################################################
chart_plot <- boot_fill %>%
  group_by(platform, country_code, name, date) %>%
  summarise(mean = mean(prop), lower_95ci = quantile(prop, .025), upper_95ci = quantile(prop, .975), .groups = 'drop') %>%
  # keep only language series that occur on a platform and country
  semi_join(boot_window %>% filter(prop > 0) %>% distinct(platform, country_code, name), by = c('platform', 'country_code', 'name')) %>%
  mutate(platform = factor(recode(platform, Youtube = 'YouTube Music'), c('YouTube Music', 'Spotify')),
         country = factor(country_code, c('UA', 'RU'), c('Ukraine', 'Russia')),
         language = factor(name, names(LANG_COLS)))

chart_plot %>%
  ggplot(aes(date, mean, colour = language, fill = language)) +
  geom_ribbon(aes(ymin = lower_95ci, ymax = upper_95ci), colour = NA, alpha = 0.3) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 0.3, shape = 16) +
  geom_vline(xintercept = WAR_START, linetype = 'dashed', linewidth = LW) +
  facet_wrap(~ platform + country, nrow = 1, labeller = labeller(.multi_line = FALSE, .default = function(x) x)) +
  scale_colour_manual(values = LANG_COLS, name = 'Sung language', drop = FALSE) +
  scale_fill_manual(values = LANG_COLS, guide = 'none', drop = FALSE) +
  scale_y_continuous(limits = c(0, 95), breaks = seq(0, 100, 25)) +
  date_x + labs(x = NULL, y = 'Sung language (%)') +
  strip_box +
  theme(legend.position = 'bottom')
plot_save('Main/comparison_w_spotify_youtube', c(FULL_W, 50),
          chart_plot %>% select(platform, country, language, date, mean, lower_95ci, upper_95ci), source_name = 'Fig_2b')


################################################################################
# STATS
################################################################################
# Pre- vs post-invasion change in Ukrainian and Russian songs on each platform and country.
# n = weekly charts per period (Spotify Russia ends in April 2022). Cohen's d across charts in each
# bootstrap replicate, reported as bootstrap mean, 95% CI, z and P.
lyrics_change <- boot_fill %>%
  filter(name %in% c('Ukrainian', 'Russian'), !(country_code == 'RU' & name == 'Ukrainian')) %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post'))

# number of charts per period
lyrics_change %>% filter(boot == 1, name == 'Russian') %>% count(platform, country_code, pre_post)

per_boot <- lyrics_change %>%
  group_by(platform, country_code, name, boot) %>%
  summarise(pre = mean(prop[pre_post == 'pre']), post = mean(prop[pre_post == 'post']),
            get_t_stat(prop[pre_post == 'post'], prop[pre_post == 'pre']), .groups = 'drop')

# pre and post mean percentage
per_boot %>% group_by(platform, country_code, name) %>% reframe(get_boot_mean_ci(pre, 'pre'), get_boot_mean_ci(post, 'post')) %>%
  print(n = Inf, width = Inf)

# effect sizes
per_boot %>% group_by(platform, country_code, name) %>% reframe(get_boot_mean_ci(cohensD, 'd')) %>%
  print(n = Inf, width = Inf)
