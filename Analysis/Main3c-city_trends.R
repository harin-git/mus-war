#' Temporal trends in local proportion of songs per cities in Ukraine and Russia
#' Related to Fig. 3 in paper

# load study-wide functions and global variables
source('utils.R')


################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load data
chart_data <- read_rds('Dataset/chart/longitudinal_postsoviet_chart.rds')
city_meta <- read_rds('Dataset/meta/city_metadata.rds')

# filter chart for study window among Ukraine and Russia
chart_clean <- chart_data %>%
  ungroup() %>%
  filter(study_window) %>%
  filter(country_code %in% c('RU', 'UA')) %>%
  filter(lyrics_lang_prob >= 0.7)


################################################################################
# BOOTSTRAP LOCAL MUSIC CITY-LEVEL TRENDS
################################################################################
# bootstrap songs for each city and each day
n_boot <- 1000

if(SIMULATION){
  city_boot = list()
  for (i in 1:n_boot) {
    smp <- chart_clean %>%
      group_by(country_code, nodeID, date) %>%
      sample_n(n(), replace = T)
    
    city_boot[[i]] <- smp %>%
      count(country_code, nodeID, date, lyrics_lang) %>%
      group_by(country_code, nodeID, date) %>%
      mutate(prop = n / sum(n) * 100) %>%
      cbind(boot = i) %>%
      filter((country_code == 'RU' & lyrics_lang == 'ru') | (country_code == 'UA' & lyrics_lang == 'uk'))
    
    print(i)
  }
  
  # bind and save bootstraps
  city_boot_output <- do.call(rbind, city_boot)
  city_boot_output <- city_boot_output %>% 
    left_join(city_meta %>% select(nodeID, city_name)) %>%
    mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post'))
  
  city_boot_output %>% write_rds(sprintf('Bootstraps/city_level_local_music_boot%s.rds', n_boot))
} else {
  boot_load()
  city_boot_output <- read_rds(sprintf('Bootstraps/city_level_local_music_boot%s.rds', n_boot))
}


################################################################################
# PLOT TRENDS
################################################################################
# aggregate the statstics
city_agg <- city_boot_output %>%
  group_by(country_code, city_name, nodeID, date, lyrics_lang) %>%
  reframe(get_boot_mean_ci(prop, 'boot')) %>%
  ungroup() %>%
  mutate(date = as.Date(date))


plot_city_level <- function(country){
  switch(country,
         'UA' = {
           lang <- 'uk'
           city_categories <- c('Occupied', 'Not occupied')
           sub_cities <- c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk', 'Mariupol')
           colour_pal <- c(UKRAINE_COLOUR2, UKRAINE_COLOUR) 
         },
         'RU'= {
           lang <- 'ru'
           city_categories <- c('Minority', 'Non-minority')
           
           # categorise the region as minority 
           ru_census <- read.csv('Dataset/census/russia_census_city_mapped.csv')
           ru_census <- ru_census %>% mutate(city_type = ifelse(local_prop < 50, city_categories[1], city_categories[2]))
           
           sub_cities <- ru_census[ru_census$city_type == 'Minority', 'city_name']
           colour_pal <- c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
         })
  
  # report the minimum and maximum
  print(paste('min', min(city_agg$boot_m) %>% signif(2)))
  print(paste('max', max(city_agg$boot_m) %>% signif(2)))
  
  plot_cities <- city_agg %>%
    filter(country_code == country, lyrics_lang == lang)
  
  # fill 0 for missing dates
  plot_cities <- plot_cities %>%
    complete(city_name, date, fill = list('boot_m' = 0)) %>%
    mutate(city_type = ifelse(city_name %in% sub_cities, city_categories[1], city_categories[2]))
  
  
  # plot
  plot_cities %>%
    ggplot(aes(as.Date(date), boot_m, colour = city_type, group = city_name)) +
    geom_smooth(aes(linetype = ifelse(city_name == 'Mariupol', 'solid', 'dashed')),
                se = FALSE, span = 1, size = 0.5) +
    geom_vline(xintercept = WAR_START, linetype = 'dashed') +
    labs(x = '', y = 'Local music (%)') +
    scale_colour_manual(values = colour_pal) + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    # make sure it doesn't go below 0
    ylim(0, 60) +
    theme_classic() +
    theme(legend.position = 'None')
}

(ua_cities <- plot_city_level('UA'))
(ru_cities <- plot_city_level('RU'))

# bind the two cities plot and save for main figure
ua_cities + ru_cities
plot_save('Main/city_level_local_music', c(250, 70))


################################################################################
# STATS
################################################################################
## UKRAINE STATS
ua_cities <- city_boot_output %>%
  ungroup() %>%
  filter(lyrics_lang == 'uk', country_code == 'UA')
ua_cities <- ua_cities %>%
  group_by(pre_post) %>%
  complete(city_name, date, boot, fill = list(prop = 0))
ua_cities <- ua_cities %>%
  mutate(occupy = ifelse(city_name %in%  c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk'), 'y', 'n'))

# general homogeneity of change in Ukraine
ua_cities %>%
  group_by(pre_post, boot) %>%
  summarise(sd = sd(prop)) %>%
  reframe(get_boot_mean_ci(sd, 'sd'))

# changes in trends by occupation status
ukraine_change <- ua_cities %>%
  select(occupy, pre_post, prop, boot) %>%
  pivot_wider(names_from = pre_post, values_from = prop) %>%
  group_by(occupy, boot) %>%
  summarise(get_t_stat(post %>% unlist(), pre %>% unlist()))

ukraine_change <- ukraine_change %>%
  group_by(occupy) %>%
  reframe(get_boot_mean_ci(mean_diff, 'diff'),
          get_boot_mean_ci(cohensD, 'd'))

ukraine_change

# marioupol example
mario <- city_boot_output %>%
  filter(nodeID == 1179) %>%
  mutate(date = as.Date(date),
         month = strftime(date, format = '%b')) 
mario <- mario %>%
  group_by(month) %>%
  reframe(get_boot_mean_ci(prop, 'boot'))

mario


## RUSSIAN STATS
# general homogeneity of change in Russia
city_boot_output %>%
  filter(country_code == 'RU') %>%
  group_by(pre_post, boot) %>%
  summarise(sd = sd(prop)) %>%
  reframe(get_boot_mean_ci(sd, 'sd'))

# change by russian cities
ru_cities <- city_boot_output %>%
  ungroup() %>%
  filter(lyrics_lang == 'ru', country_code == 'RU')

# categorise the region as minority 
ru_census <- read.csv('Dataset/census/russia_census_city_mapped.csv')
ru_census <- ru_census %>% mutate(city_type = ifelse(local_prop < 50, 'min', 'notmin'))
sub_cities <- ru_census[ru_census$city_type == 'min', 'city_name']
ru_cities <- ru_cities %>%
  mutate(city_type = ifelse(city_name %in% sub_cities, 'min', 'notmin'))

# aggregate by city
ru_agg <- ru_cities %>%
  group_by(city_type, city_name, pre_post) %>%
  summarise(m = mean(prop)) %>%
  ungroup() %>%
  pivot_wider(names_from = pre_post, values_from = m)

# pre post diff
ru_agg <- ru_agg %>%
  mutate(pre_post_diff = post - pre)

ru_agg %>%
  select(pre_post_diff, city_type) %>%
  pivot_wider(names_from = city_type, values_from = pre_post_diff) %>%
  summarise(get_t_stat(min %>% unlist(), notmin %>% unlist()))

# mean decrease in russia
russia_change_mean <- city_boot_output %>%
  filter(country_code == 'RU') %>%
  group_by(city_name, boot, pre_post) %>%
  summarise(m = mean(prop)) %>%
  select(pre_post, boot, m) %>%
  pivot_wider(names_from = pre_post, values_from = m) %>%
  group_by(boot) %>%
  summarise(get_t_stat(post %>% unlist(), pre %>% unlist()))

russia_change_mean %>%
  reframe(get_boot_mean_ci(cohensD, 'd'))

