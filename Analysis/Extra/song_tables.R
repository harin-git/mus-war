#' Create Supplementary Information tables 2-4

# load study-wide functions and global variables
source('utils.R')

# load datasets
chart_data <- read_rds('Dataset/chart/longitudinal_postsoviet_chart.rds') # only post-soviet chart
study_data <- chart_data %>% filter(study_window)
song_metadata <- read_csv("Dataset/meta/song_metadata.csv") %>% 
  select(trackID, artist, title) %>% 
  distinct(trackID, .keep_all = T)

# find top 10 songs in post-soviet countries
find_top10songs_psotwar = function(data, country){
  
  select_data = tibble(data) %>%  
    filter(country_code == country) %>% 
    filter(lyrics_lang == "uk") %>% 
    filter(date > "2022-02-23")
  
  top_select_data = select_data %>% 
    group_by(trackID) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    left_join(song_metadata)
  
  return(top_select_data)
}

find_top10songs_psotwar(study_data, "UA")
find_top10songs_psotwar(study_data, "BY")


# find top 10 UA songs in Poland
all_chart_data <- read_rds('Dataset/chart/study_set_chart.rds') # all charts

# select all UA songs in Ukrainian language post-conflict
UA_songs_UA_language = tibble(study_data) %>%  
  filter(country_code == "UA") %>% 
  filter(lyrics_lang == "uk") %>% 
  filter(date > "2022-02-23") %>% 
  distinct(trackID, .keep_all = T)

# select all UA songs in Polish chart post-invasion
UA_songs_UA_language_Poland = all_chart_data %>%  
  filter(country_code == "PL") %>% 
  filter(date > "2022-02-23") %>% 
  group_by(trackID) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  left_join(UA_songs_UA_language) %>% 
  # remove songs that are not in UA
  drop_na() %>% 
  left_join(song_metadata)

UA_songs_UA_language_Poland

