#' Make networks using co-occurrences of songs between cities
#' Related to Supplementary Information in paper

# load study-wide functions and global variables
source('utils.R')

# make co-occurence
chart_data <- read_rds('Dataset/chart/study_set_chart.rds')

# get only post-soviet countries
chart_data <- chart_data %>% 
  filter(country_code %in% SUB_COUNTRIES) %>%
  select(nodeID, date, pre_post, country_code, trackID)

city_meta <- read_rds('Dataset/meta/city_metadata.rds')
city_meta <- city_meta %>% select(nodeID, country_code, city_name, worldbank_region)
n_places <- city_meta %>%
  count(country_code) %>%
  rename(n_places = n)

# do co-occurence by jaccard distance (i.e. all overlapping unique tracks)
unique_songs <- chart_data %>%
  distinct(nodeID, pre_post, country_code, trackID)

# define function to get co-occurrence weights
get_coco <- function(pre_post = c('pre', 'post', 'global')){
  ft_data <- switch (pre_post,
    pre = unique_songs %>% filter(pre_post == 'pre'),
    post = unique_songs %>% filter(pre_post == 'post'),
    global = unique_songs
  )
  
  # define the weights of each song by the number of times a song has been played everywhere
  song_popularity <- ft_data %>%
    group_by(trackID) %>%
    summarise(n_country = n_distinct(country_code)) %>%
    mutate(song_weight = 1 / (n_country - 1)) %>%
    mutate(song_weight = ifelse(is.infinite(song_weight), 0, song_weight))
  
  # combine the weights and drop songs with song weight = 0 for downsizing data, as there is no cooccurrence.
  ft_data <- ft_data %>% left_join(song_popularity) %>% filter(song_weight != 0)
  
  all_cities <- unique(ft_data$nodeID)
  store = list()
  for (i in 1:length(all_cities)) {
    source_city <- ft_data %>% filter(nodeID == all_cities[i])

    # get overlaps
    source_songs <- source_city$trackID
    
    overlaps <- ft_data %>%
      group_by(nodeID) %>%
      summarise(weight = length(intersect(source_city$trackID, trackID)) / (length(source_city$trackID) + length(trackID)) * 2) %>%
      ungroup() %>%
      rename(Target = nodeID)
    
    # outcome
    store[[i]] <- cbind(Source = all_cities[i], overlaps)
    
    print(i)
    
  }
  do.call(rbind, store) %>% cbind(pre_post)
}

# separately for pre and post invasion
pre_coco <- get_coco('pre')
pre_coco %>% write_rds('Dataset/network/coco/pre_cooccurence.rds')

post_coco <- get_coco('post')
post_coco %>% write_rds('Dataset/network/coco/post_cooccurence.rds')




