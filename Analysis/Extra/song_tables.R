#' Create Supplementary Information tables

# load study-wide functions and global variables
source('utils.R')

chart_data <- read_rds('Dataset/chart/longitudinal_postsoviet_chart.rds')
study_data <- chart_data %>% filter(study_window)

song_metadata <- read_csv("Dataset/meta/song_metadata.csv") %>% 
  select(trackID, artist, title) %>% 
  distinct(trackID, .keep_all = T)


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
