#' Analyze the trend of lyrics language in the study set on Spotify and Youtube music charts
#' Replication to Shazam trends in paper. 
#' Related to Fig. 2 in paper

source('utils.R')

SIMULATION <- TRUE

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

# aggregate the outcome
boot_agg <- boot_output %>%
    group_by(platform, date, country_code, name) %>%
    reframe(get_boot_mean_ci(prop, 'boot'))

# organise the outcome
chart_plot <- boot_agg %>%
    mutate(name = factor(name, levels = c('Ukrainian', 'Russian', 'Kazakh', 'English', 'Other')),
           country_code = factor(country_code, levels = c('UA', 'RU'), labels = c('Ukraine', 'Russia')))

################################################################################
# TREND PLOT
################################################################################
# plot the data
plot_comparison <- function(type){
    chart_plot %>% 
    filter(platform == type) %>%
    # make percentage
    mutate(percent = boot_m * 100,
           ci_lower = boot_lower_ci * 100,
           ci_upper = boot_upper_ci * 100) %>%
    ggplot(aes(x = date, y = percent, fill = name, color = name)) +
    geom_line() +
    geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper),  alpha = 0.3, color = NA)  +
    facet_wrap(~ country_code) +
    geom_vline(xintercept = WAR_START, linetype = 'dashed', color = 'black') +
    labs(
        subtitle = '',
        x = '', y = 'Proportion (%)', color = 'Language') +
    scale_color_manual(values = c(
        'Ukrainian' = UKRAINE_COLOUR,
        'Russian' = RUSSIAN_COLOUR,
        'Kazakh' = KAZAKHSTAN_COLOUR,
        'English' = ENGLISH_COLOUR,
        'Other' = 'gray'
    )) +
    scale_fill_manual(values = c(
        'Ukrainian' = UKRAINE_COLOUR,
        'Russian' = RUSSIAN_COLOUR,
        'Kazakh' = KAZAKHSTAN_COLOUR,
        'English' = ENGLISH_COLOUR,
        'Other' = 'gray'
    )) +
    theme(plot.title = element_text(hjust = 0))
}

# make and save the plot
yt_plot <- plot_comparison('Youtube')
sp_plot <- plot_comparison('Spotify')

yt_plot + sp_plot + plot_layout(guides = 'collect') & theme(legend.position = 'none')
plot_save('Main/comparison_w_spotify_youtube', c(183, 50))


################################################################################
# STATS
################################################################################
# compute the pre vs. post increase/decrease percentage. Use cohen's d for effect size measure
lyrics_change <- chart_plot %>%
  filter(date >= WAR_START - 90, date <= WAR_START + 90) %>%
  mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post'))

# Prepare t-stats for Ukraine replication on Spotify and Youtube
lyrics_change_uk <- lyrics_change %>%
    filter(country_code == 'Ukraine', name %in% c('Ukrainian', 'Russian')) %>%
    ungroup()

# get mean and ci
lyrics_change_uk %>%
    group_by(platform, name, pre_post) %>%
    summarise(m = mean(boot_m), ci = CI(boot_m)) %>%
    mutate(ci_lower = round(m - ci, 2), ci_upper = round(m + ci, 2))

# get t-stats
lyrics_change_uk %>%
    select(platform, name, pre_post, boot_m) %>%
    pivot_wider(names_from = pre_post, values_from = boot_m) %>%
    group_by(platform, name) %>%
    summarise(get_t_stat(post %>% unlist(), pre %>% unlist()))

# Prepare t-stats for Russia replication on Spotify and Youtube
lyrics_change_ru <- lyrics_change %>%
    filter(country == 'Russia', lang_category %in% c('Ukrainian', 'Russian')) %>%
    ungroup()

# get mean and ci
lyrics_change_ru %>%
    group_by(platform, lang_category, pre_post) %>%
    summarise(m = mean(prop), ci = CI(prop)) %>%
    mutate(ci_lower = round(m - ci, 2), ci_upper = round(m + ci, 2)) %>%
    view()

# get t-stats
lyrics_change_ru %>%
    filter(lang_category == 'Russian', platform == 'Youtube') %>%
    select(platform, lang_category, pre_post, prop) %>%
    pivot_wider(names_from = pre_post, values_from = prop) %>%
    group_by(platform, lang_category) %>%
    summarise(get_t_stat(post %>% unlist(), pre %>% unlist()))

