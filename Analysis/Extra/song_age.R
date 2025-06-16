#' Analyze the age of the songs in the study set
#' Related to Supplementary Information in paper

source('utils.R')

# load the songs from UA and RU
study_set <- readRDS('Dataset/chart/study_set_chart.rds')

# filter only the songs played in UA and RU
ua_ru <- study_set %>% 
    filter(country_code %in% c("UA", "RU")) %>%
    group_by(country_code, pre_post) %>%
    distinct(trackID) %>%
    as_tibble()

# load song release years
song_release <- read.csv('Dataset/meta/song_release_year.csv')

# load spotify metadata for Ukraine and Russia
ua_ru <- readRDS('Dataset/meta/song_metadata.rds') %>%
    as_tibble() %>%
    distinct() %>%
    filter(country_code %in% c('UA', 'RU'))

# combine
ua_ru_release <- ua_ru %>%
    left_join(song_release) %>%
    select(pre_post, first_appear, country_code, trackID, title, artist, lyrics_language, release_year) %>%
    mutate(lyrics_lang = case_when(
        lyrics_language == 'uk' ~ 'Ukrainian',
        lyrics_language == 'ru' ~ 'Russian'
    )) %>%
    mutate(local = case_when(
        country_code == 'UA' & lyrics_lang == 'Ukrainian' ~ TRUE,
        country_code == 'UA' & lyrics_lang == 'Russian' ~ FALSE,
        country_code == 'RU' & lyrics_lang == 'Russian' ~ TRUE,
        country_code == 'RU' & lyrics_lang == 'Ukrainian' ~ FALSE
    )) %>%
    na.omit() 

# get frequencies by year
ua_ru_freq <- ua_ru_release %>%
    group_by(country_code, lyrics_lang, pre_post, release_year) %>%
    summarise(n = n_distinct(trackID)) %>%
    ungroup() %>%
    mutate(pre_post = factor(pre_post, levels = c('pre', 'post'), labels = c('pre-invasion', 'post-invasion')),
        country_code = factor(country_code, levels = c('UA', 'RU'), labels = c('Ukraine', 'Russia')))

# for ukraine songs in ukrainian, get distirbution of release years
ua_ru_freq %>%
    ggplot(aes(x = release_year, y = n, colour = lyrics_lang)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ country_code + pre_post) +
    scale_colour_manual(values = c('Ukrainian' = UKRAINE_COLOUR, 'Russian' = RUSSIAN_COLOUR)) +
    theme(legend.position = 'right') +
    labs(x = 'Release Year', y = 'Number of Songs Appeared in Charts', colour = 'Language')

plot_save('SI/song_age', c(180, 100))

# get mean release years
ua_ru_freq %>%
    group_by(country_code, lyrics_lang, pre_post) %>%
    summarise(mean_release_year = median(release_year))

# do a paired t-test for the release years
ua_ru_wide <- ua_ru_freq %>%
    select(-n) %>%
    pivot_wider(names_from = pre_post, values_from = release_year)

# drop Russia for Ukranian
ua_ru_wide <- ua_ru_wide[-2, ]

ua_ru_wide %>%
    group_by(country_code, lyrics_lang) %>%
    summarise(t_stat = get_t_stat(pre %>% unlist(), post %>% unlist()))
