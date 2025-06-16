#' Analyze the distribution of unique songs per city in the study set
#' Related to Supplementary Information in paper

source('utils.R')

# load the data
data <- read_rds('Dataset/chart/study_set_chart.rds') %>% as_tibble()

# plot supplementary figure on distribution of unique songs per city
uq_data <- data %>%
    group_by(nodeID) %>%
    summarise(n_unique = n_distinct(trackID))
    
uq_data %>%
    ggplot(aes(x = n_unique)) +
    geom_histogram(binwidth = 10) +
    theme_minimal() +
    labs(x = 'Number of Unique Songs',
         y = 'Count')

plot_save('SI/unique_songs_freq', c(80, 50))

# report statistics
uq_data %>%
    summarise(mean = mean(n_unique),
              sd = sd(n_unique),
              min = min(n_unique),
              max = max(n_unique))
