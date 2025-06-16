#' Back translating word clouds generated using original language
#' Related to Supplementary Information in paper

source('utils.R')
require(ggwordcloud) # for making word clouds
require(tm) # for stopwords removal in backtranslation version

# make wordcloud of the words
make_wordcloud <- function(zero_rank_df, 
                           pre_post = c('pre', 'post'), 
                           word_colour = c(UKRAINE_COLOUR, RUSSIAN_COLOUR), 
                           translate = FALSE,
                           n_words = 30, 
                           n_word_highlight = 10){

  # take the top n_words in the order of rank
  wordcloud_plot <- zero_rank_df %>%
    filter(groups.use == pre_post) %>%
    arrange(zetarank) %>%
    as.data.frame() %>%
    group_by(groups.use, back_translate) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(idx = 1:n()) %>%
    filter(idx <= n_words)
  if(translate){
    row.names(wordcloud_plot) <- wordcloud_plot$back_translate
    wordcloud_plot$term <- wordcloud_plot$back_translate
  } else {
    row.names(wordcloud_plot) <- wordcloud_plot$term
  }
  
  # plot the wordcloud highlighting the most important words in colours
  wordcloud_plot %>%
    ggplot(aes(label = term, size = plotsize)) +
    ggwordcloud::geom_text_wordcloud(colour = ifelse(wordcloud_plot$idx %in% 1:n_word_highlight, word_colour, 'gray50')) +
    theme_minimal() +
    scale_size_area(max_size = 10)
}

# load original wordcloud words
stop_words <- tm::stopwords('en')

ua_zeta <- read.csv('Dataset/lyrics/UA_zeta_rank_original_backtranslate.csv')
ua_zeta$plotsize <- as.numeric(ua_zeta$plotsize)

ru_zeta <- read.csv('Dataset/lyrics/RU_zeta_rank_original_backtranslate.csv')
ru_zeta <- ru_zeta[-c(7,8,9), ]
ru_zeta$plotsize <- as.numeric(ru_zeta$plotsize)

# filter single letter word
ua_zeta <- ua_zeta %>% filter(str_length(back_translate) > 1,
                              !back_translate %in% stop_words)
ru_zeta <- ru_zeta %>% filter(str_length(back_translate) > 1,
                              !back_translate %in% stop_words)

# plot for Ukraine and Russia
make_wordcloud(ua_zeta, 'pre', UKRAINE_COLOUR, TRUE)
plot_save('SI/UA_lyrics_pre_wordcloud_backtranslate', c(100, 80))
make_wordcloud(ua_zeta, 'pre', UKRAINE_COLOUR, FALSE)
plot_save('SI/UA_lyrics_pre_wordcloud_original', c(100, 80))

make_wordcloud(ua_zeta, 'post', UKRAINE_COLOUR, TRUE)
plot_save('SI/UA_lyrics_post_wordcloud_backtranslate', c(100, 80))
make_wordcloud(ua_zeta, 'post', UKRAINE_COLOUR, FALSE)
plot_save('SI/UA_lyrics_post_wordcloud_original', c(100, 80))

make_wordcloud(ru_zeta, 'pre', RUSSIAN_COLOUR, TRUE)
plot_save('SI/RU_lyrics_pre_wordcloud_backtranslate', c(100, 80))
make_wordcloud(ru_zeta, 'pre', RUSSIAN_COLOUR, FALSE)
plot_save('SI/RU_lyrics_pre_wordcloud_original', c(100, 80))

make_wordcloud(ru_zeta, 'post', RUSSIAN_COLOUR, TRUE)
plot_save('SI/RU_lyrics_post_wordcloud_backtranslate', c(100, 80))
make_wordcloud(ru_zeta, 'post', RUSSIAN_COLOUR, FALSE)
plot_save('SI/RU_lyrics_post_wordcloud_original', c(100, 80))
