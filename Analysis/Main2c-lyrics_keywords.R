#' Finding keywords in lyrics of Ukraine and Russia.
#' Code is adopted from: https://burtmonroe.github.io/TextAsDataCourse/Tutorials/TADA-FightinWords.nb.html
#' Related to Fig.2 in paper

# load study-wide functions and global variables
source('utils.R')
require(quanteda) # for text analysis

################################################################################
# SETUP
################################################################################
# Lyrics data cannot be shared due to copyright issues, thus the script is for reference of the working only.
lyrics_data <- read_rds('XXX')
country <- 'UA' # either 'UA' (Ukraine) or 'RU' (Russia)
translated <- FALSE # if FALSE, the wordcloud is made using original lyrics_data (not translated)

# custom stop words to remove in the wordclouds
custom_stopwords <- c('ba', 'da', 'eh-', 'di', 
                      'hey', 'e', 'wow', 'dah', 'dee', 'whoa',
                      'll', 'mayb', 's', 'oh', 'la', 'don-digi-don',
                      'got', 'noth', 'can', 'get', 'goe', 'gonna', 'yo',
                      'pah-pah-pah', 'let'
)

################################################################################
# FUNCTIONS
################################################################################
# calculate FW of the lyrics_data
fwgroups <- function(dtm, groups, pair = NULL, weights = rep(1,nrow(dtm)), k.prior = .1) {
  weights[is.na(weights)] <- 0
  
  weights <- weights/mean(weights)
  
  zero.doc <- rowSums(dtm)==0 | weights==0
  zero.term <- colSums(dtm[!zero.doc,])==0
  
  dtm.nz <- apply(dtm[!zero.doc,!zero.term],2,"*", weights[!zero.doc])
  
  g.prior <- tcrossprod(rowSums(dtm.nz),colSums(dtm.nz))/sum(dtm.nz)
  
  g.posterior <- as.matrix(dtm.nz + k.prior*g.prior)
  
  groups <- groups[!zero.doc]
  groups <- droplevels(groups)
  
  g.adtm <- as.matrix(aggregate(x=g.posterior,by=list(groups=groups),FUN=sum)[,-1])
  rownames(g.adtm) <- levels(groups)
  
  g.ladtm <- log(g.adtm)
  
  g.delta <- t(scale( t(scale(g.ladtm, center=T, scale=F)), center=T, scale=F))
  
  g.adtm_w <- -sweep(g.adtm,1,rowSums(g.adtm)) # terms not w spoken by k
  g.adtm_k <- -sweep(g.adtm,2,colSums(g.adtm)) # w spoken by groups other than k
  g.adtm_kw <- sum(g.adtm) - g.adtm_w - g.adtm_k - g.adtm # total terms not w or k 
  
  g.se <- sqrt(1/g.adtm + 1/g.adtm_w + 1/g.adtm_k + 1/g.adtm_kw)
  
  g.zeta <- g.delta/g.se
  
  g.counts <- as.matrix(aggregate(x=dtm.nz, by = list(groups=groups), FUN=sum)[,-1])
  
  if (!is.null(pair)) {
    pr.delta <- t(scale( t(scale(g.ladtm[pair,], center = T, scale =F)), center=T, scale=F))
    pr.adtm_w <- -sweep(g.adtm[pair,],1,rowSums(g.adtm[pair,]))
    pr.adtm_k <- -sweep(g.adtm[pair,],2,colSums(g.adtm[pair,])) # w spoken by groups other than k
    pr.adtm_kw <- sum(g.adtm[pair,]) - pr.adtm_w - pr.adtm_k - g.adtm[pair,] # total terms not w or k
    pr.se <- sqrt(1/g.adtm[pair,] + 1/pr.adtm_w + 1/pr.adtm_k + 1/pr.adtm_kw)
    pr.zeta <- pr.delta/pr.se
    
    return(list(zeta=pr.zeta[1,], delta=pr.delta[1,],se=pr.se[1,], counts = colSums(dtm.nz), acounts = colSums(g.adtm)))
  } else {
    return(list(zeta=g.zeta,delta=g.delta,se=g.se,counts=g.counts,acounts=g.adtm))
  }
}

# compute zeta rank scores
get_zeta_rank <- function(fw.ch, sizescale = 1, groups.use = as.factor(rownames(fw.ch$zeta)),
                          max.words = 50, max.countrank = 400){
  if (is.null(dim(fw.ch$zeta))) {## two-group fw object consists of vectors, not matrices
    zetarankmat <- cbind(rank(-fw.ch$zeta),rank(fw.ch$zeta))
    colnames(zetarankmat) <- groups.use
    countrank <- rank(-(fw.ch$counts))
  } else {
    zetarankmat <- apply(-fw.ch$zeta[groups.use,],1,rank)
    countrank <- rank(-colSums(fw.ch$counts))
  }
  wideplotmat <- as_tibble(cbind(zetarankmat,countrank=countrank))
  wideplotmat$term=names(countrank)
  #rankplot <- gather(wideplotmat, party, zetarank, 1:ncol(zetarankmat))
  rankplot <- gather(wideplotmat, groups.use, zetarank, 1:ncol(zetarankmat))
  rankplot$plotsize <- (50/(rankplot$zetarank))^(sizescale)
  rankplot <- rankplot[rankplot$zetarank < max.words + 1 & rankplot$countrank<max.countrank+1,]
  rankplot$groups.use <- factor(rankplot$groups.use,levels=groups.use)
  
  rankplot
}

# make wordcloud of the words
make_wordcloud <- function(zero_rank_df, pre_post = c('pre', 'post'), n_words = 30, n_word_highlight = 10){
  if(!translated){
    Sys.setlocale(category="LC_CTYPE", locale="ru_RU.KOI8-R")
  }
  
  # take the top n_words in the order of rank
  wordcloud_plot <- zero_rank_df %>%
    filter(zetarank <= n_words, groups.use == pre_post) %>%
    arrange(zetarank) %>%
    as.data.frame() %>%
    mutate(idx = 1:n())
  row.names(wordcloud_plot) <- wordcloud_plot$term
  
  # plot the wordcloud highlighting the most important words in colours
  wordcloud_plot %>%
    ggplot(aes(label = term, size = plotsize)) +
    ggwordcloud::geom_text_wordcloud(colour = ifelse(wordcloud_plot$idx %in% 1:n_word_highlight, word_colour, 'gray50')) +
    theme_minimal() +
    scale_size_area(max_size = 15)
}


################################################################################
# WORDCLOUD
################################################################################
switch (country,
        RU = {
          word_colour <- RUSSIAN_COLOUR
          lang <- 'ru'
        }, 
        UA = {
          word_colour <- UKRAINE_COLOUR
          lang <- 'uk'
        }
)

# filter the country
sel_country <- lyrics_data %>% 
  filter(country_code == country, 
         !is.na(cleaned_lyrics),
         lyrics_language == lang) %>%
  mutate(pre_post = as.factor(pre_post))

# define pre vs. post as meta data
sel_country_meta <- sel_country$pre_post

# convert the text column into a character vector
if(translated){
  text_vector <- as.character(sel_country$cleaned_lyrics)
} else {
  text_vector <- as.character(sel_country$lyrics)
}

# clean the words and tokenize the lyrics_data
tokens <- tokens(text_vector, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE)
tokens <- tokens_remove(tokens, pattern = stopwords("en"), padding = FALSE)
tokens_stemmed <- tokens_wordstem(tokens, language = "english")
tokens_stemmed <- tokens_remove(tokens_stemmed, pattern = custom_stopwords)

dfm_matrix <- dfm(tokens_stemmed, tolower = TRUE)

# calculate FW
lyrics_fw <- fwgroups(dfm_matrix, groups = sel_country_meta)

# get zeta ranking
zeta_rank <- get_zeta_rank(lyrics_fw, max.words = 100, max.countrank = 1000, sizescale = 2/3)

# make wordclouds for pre and post. If in original language, save as SI
if(translated){
  pre_wordcloud <- make_wordcloud(zeta_rank, 'pre')
  plot_save(sprintf('Main/%s_lyrics_pre_wordcloud', country), c(100, 80))
  
  make_wordcloud(zeta_rank, 'post')
  plot_save(sprintf('Main/%s_lyrics_post_wordcloud', country), c(100, 80))
} else {
  make_wordcloud(zeta_rank, 'pre')
  plot_save(sprintf('SI/original_%s_lyrics_pre_wordcloud', country), c(100, 80))
  
  make_wordcloud(zeta_rank, 'post')
  plot_save(sprintf('SI/original_%s_lyrics_post_wordcloud', country), c(100, 80))
  
  # save the zeta rank for the original language
  message('Saving zeta rank for original language')
  zeta_rank %>% write_csv(sprintf('Dataset/lyrics/%s_zeta_rank_original.csv', country))
}


