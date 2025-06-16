#' Compare the Shazam, Spotify, and Youtube charts by number of overlapping songs
#' Related to Methods in paper

# load study-wide functions and global variables
source('utils.R')
require(stringdist) # for fuzzy string matching

# get data for 2021 for the three charts
# The data for these two are not included in OSF as it is excessively large. 
# Please contact the author for the data.
spotify_chart <- read.csv('Dataset/data_comparison/2021_spotify_chart_clean.csv') %>% as_tibble()
shazam_chart <- read.csv('Dataset/data_comparison/2021_shazam_chart_clean.csv') %>% as_tibble()
youtube_chart <- read.csv('Dataset/data_comparison/2021_youtube_chart_clean.csv') %>% as_tibble()

# how many countries overlap?
sp_sz_overlap <- intersect(unique(spotify_chart$country_code), unique(shazam_chart$country_code))
sp_yt_overlap <- intersect(unique(spotify_chart$country_code), unique(youtube_chart$country_code))
sz_yt_overlap <- intersect(unique(shazam_chart$country_code), unique(youtube_chart$country_code))

message(sprintf('Spotify and Shazam overlap in %s countries', length(sp_sz_overlap)))
message(sprintf('Spotify and Youtube overlap in %s countries', length(sp_yt_overlap)))
message(sprintf('Shazam and Youtube overlap in %s countries', length(sz_yt_overlap))) 

# for each month get all the unique songs of each country
sp_month <- spotify_chart %>%
  group_by(country_code, month) %>%
  distinct(title, artist) %>%
  ungroup() %>%
  mutate(key = paste(tolower(title), tolower(artist), sep = '—')) %>%
  na.omit()

sz_month <- shazam_chart %>%
  group_by(country_code, month) %>%
  distinct(title, artist) %>%
  ungroup() %>%
  mutate(key = paste(tolower(title), tolower(artist), sep = '—')) %>%
  na.omit()

yt_month <- youtube_chart %>%
  group_by(country_code, month) %>%
  distinct(title, artist) %>%
  ungroup() %>%
  mutate(key = paste(tolower(title), tolower(artist), sep = '—')) %>%
  na.omit()

# Compute general overlap with progress tracking
compute_overlap <- function(comparison, chart1, chart2, match_threshold = 0.7) {
  set1 <- chart1$key %>% unique()
  set2 <- chart2$key %>% unique()

  n_set1 <- length(set1)
  n_set2 <- length(set2)

  message(sprintf('There are %s songs in set1, %s songs in set2', n_set1, n_set2))

  # Create progress bar
  pb <- txtProgressBar(min = 0, max = n_set1, style = 3)

  for (i in 1:n_set1) {
      # using set1 songs, match to set2 songs
      song <- set1[i]
      match_scores <- stringsim(song, set2)
      max_score <- max(match_scores)
      
      # if a match is found above threshold, recode spotify key with shazam key
      if(max_score > match_threshold){
          match_idx <- which(match_scores == max_score)[1]
          set1[i] <- set2[match_idx]
      }
      
      # Update progress bar every 10 iterations
      if(i %% 10 == 0) setTxtProgressBar(pb, i)
  }
  close(pb)

  n_overlap <- intersect(set1, set2) %>% length()
  set1_overlap <- n_overlap / n_set1 * 100
  set2_overlap <- n_overlap / n_set2 * 100
  total_overlap <- n_overlap / (n_set1 + n_set2 - n_overlap) * 100

  # return overlap percentages
  tibble(comparison, n_set1, n_set2, n_overlap, set1_overlap, set2_overlap, total_overlap)
}

sp_sz_global_overlap <- compute_overlap('sp-sz', sp_month, sz_month)
sp_yt_global_overlap <- compute_overlap('sp-yt', sp_month, yt_month)
sz_yt_global_overlap <- compute_overlap('sz-yt', sz_month, yt_month)

global_overlap <- bind_rows(sp_sz_global_overlap, sp_yt_global_overlap, sz_yt_global_overlap)

# report and save
print(global_overlap)
write_csv(global_overlap, 'Dataset/data_comparison/global_charts_overlap.csv')
