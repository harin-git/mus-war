#' Back translating word clouds generated using original language
#' Related to Supplementary Information in paper

source('utils.R')
require(ggwordcloud)

# make wordcloud of the words
make_wordcloud <- function(zero_rank_df, 
                           pre_post = c('pre', 'post'), 
                           word_colour = c(UKRAINE_COLOUR, RUSSIAN_COLOUR), 
                           n_words = 30, 
                           n_word_highlight = 10){

  # take the top n_words in the order of rank
  wordcloud_plot <- zero_rank_df %>%
    filter(zetarank <= n_words, groups.use == pre_post) %>%
    arrange(zetarank) %>%
    as.data.frame() %>%
    group_by(groups.use, back_translate) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(idx = 1:n())
  row.names(wordcloud_plot) <- wordcloud_plot$back_translate
  
  # plot the wordcloud highlighting the most important words in colours
  wordcloud_plot %>%
    ggplot(aes(label = back_translate, size = plotsize)) +
    ggwordcloud::geom_text_wordcloud(colour = ifelse(wordcloud_plot$idx %in% 1:n_word_highlight, word_colour, 'gray50')) +
    theme_minimal() +
    scale_size_area(max_size = 15)
}

# load original wordcloud words
ua_zeta <- read.csv('Dataset/lyrics/UA_zeta_rank_original_backtranslate.csv')
ua_zeta$plotsize <- as.numeric(ua_zeta$plotsize)

ru_zeta <- read.csv('Dataset/lyrics/RU_zeta_rank_original_backtranslate.csv')
ru_zeta <- ru_zeta[-c(7,8,9), ]
ru_zeta$plotsize <- as.numeric(ru_zeta$plotsize)


# plot for Ukraine and Russia
make_wordcloud(ua_zeta, 'pre', UKRAINE_COLOUR)
plot_save('SI/UA_lyrics_pre_wordcloud_backtranslate', c(100, 80))
make_wordcloud(ua_zeta, 'post', UKRAINE_COLOUR)
plot_save('SI/UA_lyrics_post_wordcloud_backtranslate', c(100, 80))

make_wordcloud(ru_zeta, 'pre', RUSSIAN_COLOUR)
plot_save('SI/RU_lyrics_pre_wordcloud_backtranslate', c(100, 80))
make_wordcloud(ru_zeta, 'post', RUSSIAN_COLOUR)
plot_save('SI/RU_lyrics_post_wordcloud_backtranslate', c(100, 80))







