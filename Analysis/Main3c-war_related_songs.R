#' Proportion of war-related songs in Ukraine and Russia
#' Word embedding vectors extracted from tokenised words of lyrics (see 'Analysis/Extra/extracting_word_embedding.R' script)
#' is used to find the closest neighbour to the seed word "war".
#' Related to Fig. 3 in the paper

# load study-wide functions and global variables
source('utils.R')
library(quanteda) # for text processing


################################################################################
# SETUP
################################################################################
SIMULATION <- TRUE # if FALSE, pre-computed bootstrap is loaded

# if TRUE, test the graph of war related songs for different thresholds of n closest words
# otherwise use 5 as in the main study
control <- TRUE

# n closest words to war words in the embedding
if(control){
  nclosest <- c(1, 3, 7)
} else {
  nclosest <- 5
}

# load data
chart_data <- read_rds('Dataset/chart/study_set_chart.rds')
# Lyrics data cannot be shared due to copyright issues. However, loading bootstrap results can reproduce the figures.
lyrics <- read_rds('Dataset/lyrics/UA_RU_lyrics_acoustics_gpt.rds') %>% rename(trackID = track_id)
word_embedding <- read_rds('Dataset/lyrics/UA_RU_word_embedding_gpt.rds') 

# define war related words
war_words <- c('war')


################################################################################
# SEMANTIC SIMILARITY WITH "WAR"
################################################################################
if(SIMULATION){
  # unpack the word embedding
  word_embedding <- word_embedding %>%
    group_by(word) %>%
    unnest(embedding) %>%
    mutate(index = paste0(1:n())) %>% # make embedding index
    pivot_wider(names_from = index, values_from = embedding) %>%
    ungroup()
  
  # get the similarity of all words to the war words
  war_vec <- word_embedding %>% filter(word == war_words) %>% select(-word)
  other_vec <- word_embedding %>% filter(word != war_words)
  
  sim_storage = list()
  for (i in 1:nrow(other_vec)) {
    word <- other_vec$word[i]
    cos_sim <- lsa::cosine(war_vec %>% unlist(), other_vec[i, 2:length(other_vec)] %>% unlist())
    sim_storage[[i]] <- tibble(word, cos_sim)
    print(i)
  }
  war_embed_sim <- do.call(rbind, sim_storage) %>% arrange(-cos_sim)
  
  # what are the top words?
  if(!control){war_embed_sim[1:5, 1]}
}


################################################################################
# LYRICS TOKENISATION
################################################################################
if(SIMULATION){
  filtered_lyrics <- lyrics %>% filter(!is.na(cleaned_lyrics_gpt), country_code %in% c('RU', 'UA'))
  
  # make a cleaned token table of all lyrics
  tokens <- tokens(filtered_lyrics$cleaned_lyrics_gpt, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE)
  tokens <- tokens_remove(tokens, pattern = stopwords("en"), padding = FALSE)
  tokens_stemmed <- tokens_wordstem(tokens, language = "english")
}


################################################################################
# BOOTSTRAP PROPORTION OF WAR RELATED SONGS
################################################################################
n_boot <- 1000

if(SIMULATION){
  output = list()
  for (n in nclosest) {
    # take the top N war words with the highest cosine similarity
    top_closest <- c(war_words, war_embed_sim$word[1:(n - 1)])
    message(sprintf('The top %s war related words are: %s', n, paste(top_closest, collapse = ', ')))
    
    # identify songs with war related words
    select_war <- tokens_select(tokens_stemmed, pattern = top_closest)
    lengths_tokens <- lengths(select_war)
    
    war_songs <- tibble(trackID = filtered_lyrics$trackID,
                        war_related = ifelse(lengths_tokens > 0, 1, 0)) 
    war_songs <- war_songs %>% group_by(trackID) %>% slice(1)
    
    # prepare all unique songs played in each country for each day
    country_unique <- chart_data %>%
      filter(country_code %in% c('RU', 'UA')) %>%
      distinct(country_code, date, trackID) %>%
      left_join(war_songs) %>%
      na.omit()
    
    # plot the trajectory of war-related song proportion over time
    war_boot = list()
    for (i in 1:n_boot) {
      smp <- country_unique %>%
        group_by(country_code, date) %>%
        sample_n(n(), replace = T)
      
      war_boot[[i]] <- smp %>%
        summarise(
          n_songs = n(),
          war_proportion = sum(war_related) / n() * 100
        ) %>%
        ungroup() %>%
        cbind(boot = i,
              nclosest = n)
      print(i)
    }
    output[[n]] <- do.call(rbind, war_boot)
  }
  
  # save the war related proportion boostrap results
  war_boot_output <- do.call(rbind, output)
  if(!control){war_boot_output %>% write_rds(sprintf('Bootstraps/war_related_songs_boot%s.rds',n_boot))}
  
} else {
  war_boot_output <- read_rds(sprintf('Bootstraps/war_related_songs_boot%s.rds',n_boot))
}


################################################################################
# PLOT & STATS
################################################################################
# plot the trajectories for the two countries
war_traj <- war_boot_output %>% 
  group_by(country_code, date, nclosest) %>%
  reframe(get_boot_mean_ci(war_proportion, 'boot'))

war_traj %>%
  ggplot(aes(as.Date(date), boot_m, colour = country_code, group = country_code)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'glm') +
  scale_colour_manual(values = c(RUSSIAN_COLOUR, UKRAINE_COLOUR), guide = 'none') +
  geom_vline(xintercept = WAR_START, linetype = 'dashed') +
  scale_x_date(breaks = 'month', date_labels = "%b", date_breaks = "1 month") +
  (if(control){facet_wrap(~ nclosest)}) +
  (if(control){ggpubr::stat_cor(p.accuracy = 0.001, cor.coef.name = 'r')}) +
  labs(x = '', y = 'War-related songs (%)')

# report correlation statistics in the main text
war_traj_cor <- war_traj %>%
  select(country_code, date, boot_m) %>%
  pivot_wider(names_from = country_code, values_from = boot_m)

cor_result <- psych::corr.test(war_traj_cor %>% mutate(date = as.numeric(date)), adjust = 'bonferroni', method = 'pearson')
print(cor_result, short=FALSE) 
print(cor_result$stars, quote=FALSE, short=FALSE)

# save plot for main figure
if(!control){
  plot_save('Main/war_related_songs_proportion', c(90, 50))
} else {
  plot_save('SI/different_closest_words_for_war_related_songs', c(183, 80))
}


