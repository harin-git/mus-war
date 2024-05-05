#' Compare the Shazam and Spotify charts by number of overlapping songs
#' Related to Discussion in paper

# load study-wide functions and global variables
source('utils.R')
library(stringdist) # for fuzzy string matching

# get data for 2021 of spotify and shazam
spotify_chart <- read_rds('Dataset/data_comparison/2021_spotify_chart.rds')
shazam_chart <- read_rds('Dataset/data_comparison/2021_shazam_chart.rds')

# code the date into months for comparison in each month
spotify_chart <- spotify_chart %>%
  filter(chart == 'top200') %>%
  rename(country_name = region) %>%
  mutate(date = as.Date(date)) %>%
  filter(date > '2021-06-01') %>%
  mutate(month = strftime(date, format = "%B"))

shazam_chart <- shazam_chart %>%
  mutate(date = as.Date(date)) %>%
  filter(date > '2021-06-01') %>% 
  mutate(month = strftime(date, format = "%B"))

# how many countries overlap?
country_overlap <- intersect(unique(spotify_chart$country_name), unique(shazam_chart$country_name))
country_overlap %>% length() # n = 47

# for each month get all the unique songs of each country
spotify_chart_unique <- spotify_chart %>%
  group_by(country_name, month) %>%
  distinct(title, artist) %>%
  filter(country_name %in% country_overlap) %>%
  ungroup() %>%
  na.omit()

shazam_chart_unique <- shazam_chart %>%
  rename(title = song_title) %>%
  group_by(country_name, month) %>%
  distinct(title, artist) %>%
  filter(country_name %in% country_overlap) %>%
  ungroup() %>%
  na.omit()

# make a song dictionary of the two dataset
spotify_all_songs <- spotify_chart_unique %>%
  ungroup() %>%
  distinct(title, artist) %>%
  mutate(key = paste0('sp', 1:n()),
         id = paste(tolower(title), tolower(artist), sep = '—'))

shazam_all_songs <- shazam_chart_unique %>%
  ungroup() %>%
  distinct(title, artist) %>%
  mutate(key = paste0('sz', 1:n()),
         id = paste(tolower(title), tolower(artist), sep = '—'))

# Fuzzy matching. Using the song title + artist, find matching entries between spotify and shazam
match_threshold <- 0.7
for (i in 1:nrow(spotify_all_songs)) {
  # using shazam songs as the reference (larger number of songs) key, do fuzzy string matching
  sp_song <- spotify_all_songs$id[i]
  match_scores <- stringsim(sp_song, shazam_all_songs$id)
  max_score <- max(match_scores)
  
  # if a match is found above a specified threshold, then recode the spotify key with shazam key
  # this will allow to later compare which one overlaps or not
  if(max_score > match_threshold){
    match_idx <- which(match_scores == max_score)[1]
    spotify_all_songs$key[i] <- shazam_all_songs$key[match_idx]
    print('matched')
  } else {
    print('no match')
  }
  print(sprintf('%s/%s', i, nrow(spotify_all_songs)))
}

# how many matches are found in the entire set? Report stats in methods.
n_overlap <- str_detect(substr(spotify_all_songs$key, 1,2), 'sz') %>% sum()
n_total <- spotify_all_songs$key %>% length()
n_overlap / n_total * 100

## Monthly overlap
# combine the keys to the raw charts
spotify_chart_unique <- spotify_chart_unique %>% left_join(spotify_all_songs)
shazam_chart_unique <- shazam_chart_unique %>% left_join(shazam_all_songs)

# for each country and across the months, compute overlaps
months <- spotify_chart_unique$month %>% unique()
country_store = list()
for (c in 1:length(country_overlap)) {
  month_store = list()
  for (m in 1:length(months)) {
    sp_chart <- spotify_chart_unique %>%
      filter(country_name == country_overlap[c], month == months[m])
    sz_chart <- shazam_chart_unique %>%
      filter(country_name == country_overlap[c], month == months[m])
    n_spotify <- sp_chart %>% nrow()
    n_shazam <- sz_chart %>% nrow()
    n_overlap <- length(intersect(sp_chart$key, sz_chart$key))
    spotify_overlap_percent <- n_overlap/n_spotify* 100
    month_store[[m]] <- tibble(country_name = country_overlap[c], 
                               n_spotify, n_shazam, n_overlap, spotify_overlap_percent)
  }
  country_store[[c]] <- do.call(rbind, month_store)
  print(sprintf('Done with %s', country_overlap[c]))
}

overlap_outcome <- do.call(rbind, country_store)
overlap_outcome <- overlap_outcome %>% na.omit()

# report in text aggregate by country with a global mean and CI
overlap_outcome %>%
  group_by(country_name) %>%
  summarise(country_m = mean(spotify_overlap_percent)) %>%
  ungroup() %>%
  summarise(m = mean(country_m),
            sd = sd(country_m),
            ci = CI(country_m)) %>%
  mutate(lower_ci = m - ci,
         upper_ci = m + ci)


