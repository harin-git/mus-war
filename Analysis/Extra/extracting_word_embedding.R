#' Extracting word embedding vectors
#' Using OpenAI's most advanced version of the word embedding model (text-embedding-ada-002)
#' Related to Fig.2 in paper

# Load all relevant packages
source('utils.R')
require(openai) # wrapper for OpenAI API access
require(quanteda) # text processing

################################################################################
# SETUP
################################################################################
# Your open ai API key should be stored in the ".Renviron" file located normally in the root directory
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

# define the open ai model to use
open_ai_model <- 'text-embedding-ada-002'

# Lyrics data cannot be shared due to copyright issues, thus the script is for reference of the working only.
feature_dt <- read_rds('XXX')

# filter ones with available lyrics and for Russia and Ukraine
filtered_lyrics <- feature_dt %>% filter(!is.na(cleaned_lyrics), country_code %in% c('RU', 'UA'))


################################################################################
# LYRICS EMBEDDING: lyrical similarity
################################################################################
# for each lyric, iterate over to extract embedding vectors
lyrics_embed_storage = list()
for (i in 1:nrow(filtered_lyrics)) {
  tryCatch({
    embed <- openai::create_embedding(
      model = open_ai_model,
      input = filtered_lyrics$cleaned_lyrics[i])
    lyrics_embed_storage[[i]] <- tibble(track_id = filtered_lyrics$track_id[i],
                                        embedding = embed$data$embedding,
                                        country_code == filtered_lyrics$country_code[i])

    print(sprintf('Done with %s', i))
  }, error = function(e){message(sprintf('Error, skipping %s', i))})
}
lyrics_embed_output <- do.call(rbind, lyrics_embed_storage)
lyrics_embed_output %>% write_rds('Dataset/lyrics/UA_RU_lyrics_embedding.rds')


################################################################################
# WORD EMBEDDING: war-related words
################################################################################
ru_uk_lyrics <- filtered_lyrics %>% filter(lyrics_language %in% c('uk', 'ru'))

# make a token table
tokens <- tokens(ru_uk_lyrics$cleaned_lyrics, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE)
tokens <- tokens_remove(tokens, pattern = stopwords("en"), padding = FALSE)
tokens_stemmed <- tokens_wordstem(tokens, language = "english")
tokens_stemmed <- tokens_select(tokens_stemmed, min_nchar = 3, padding = FALSE)

# get the frequency of each word
dfm_stemmed <- dfm(tokens_stemmed)
word_counts <- colSums(dfm_stemmed) %>% sort(decreasing = T)

# how much does the word war appear? (for sanity check)
word_counts["war"]

all_words <- word_counts %>% names()

# extract the word embedding for each of the word
word_embed_storage = list()
for (i in 1:length(word_counts)) {
  tryCatch({
    embed <- openai::create_embedding(
      model = open_ai_model,
      input = all_words[i])
    word_embed_storage[[i]] <- tibble(word = all_words[i], embedding = embed$data$embedding)
    
    print(sprintf('Done with %s', i))
  }, error = function(e){message(sprintf('Error, skipping %s', i))})
}
word_embed_output <- do.call(rbind, word_embed_storage)
word_embed_output %>% write_rds('Dataset/lyrics/UA_RU_word_embedding.rds')


