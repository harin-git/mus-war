#' Lyrics translation, language detection, and word embedding extraction using OpenAI's models
#' Related to Fig. 3 in paper

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
embedding_model <- 'text-embedding-ada-002'
translation_model <- 'gpt-4o'

# Lyrics data cannot be shared due to copyright issues, thus the script is for reference of the working only.
feature_dt <- read_rds('Dataset/lyrics/UA_RU_lyrics_acoustics_gpt.rds') %>% as_tibble()

# filter ones with available lyrics and for Russia and Ukraine
filtered_lyrics <- feature_dt %>% filter(country_code %in% c('RU', 'UA'))

################################################################################
# LANGUAGE DETECTION & TRANSLATION
################################################################################
to_translate <- filtered_lyrics %>% 
  filter(lyrics_language != 'en') %>%
  distinct()

# Create a list to store all the translation results
translation_results <- list()

# Process each lyric that needs translation
for (i in 1:nrow(to_translate)) {
  track_id <- to_translate$track_id[i]
  
  tryCatch({
    response <- openai::create_chat_completion(
      model = translation_model,
      messages = list(
        list(role = 'system', 
          content = '
          You are a helpful assistant that detects the language of the text and translates it to English.
          The output should be a JSON object with the following fields:
          - lyrics_language_gpt: ISO 639-1 Code
          - cleaned_lyrics_gpt: the translation of the lyrics to English
          If the language is already English, the output should be:
          - lyrics_language_gpt: "en"
          - cleaned_lyrics_gpt: the original lyrics
          If the language or lyrics are unintelligible, the output should be null.

          Return nothing else but the JSON object.
          '),
        list(role = 'user', content = to_translate$lyrics[i])
      )
    )
    
    content <- response$choices$message.content
    
    # Remove markdown formatting if present
    content <- gsub("```json\\n|```", "", content)
    
    # Parse the JSON content
    cleaned_content <- tryCatch({
      jsonlite::fromJSON(content) %>% as_tibble()
    }, error = function(e) {
      message(sprintf("Error parsing JSON for track %s: %s", track_id, e$message))
      return(NULL)
    })

    translation_results[[i]] <- cbind(track_id, cleaned_content)
    # Print progress
    print(sprintf('Processed %d/%d: %s', i, nrow(to_translate), track_id))


  }, error = function(e) {
    message(sprintf('Error processing track %s: %s', track_id, e$message))
  })
}


for (i in 1:length(translation_results)) {
  if (is.null(translation_results[[i]])) {
    next
  }
  if (ncol(translation_results[[i]]) != 3) {
    translation_results[[i]] <- NULL
  }
}

# Combine all results into a single dataframe
translations_df <- bind_rows(translation_results) %>% as_tibble()
translations_df <- translations_df %>%
  group_by(track_id) %>%
  slice(1)

# Join with the original data and write to file
final_translations <- to_translate %>%
  left_join(translations_df, by = "track_id") %>%
  distinct()

# Save the results
write_rds(final_translations, 'Dataset/lyrics/UA_RU_lyrics_acoustics_gpt.rds')

# compute statistics on how many of the language lyrics detection between FastText and GPT agree on?
gpt_fasttext <- final_translations %>%
  distinct(track_id, lyrics_language, lyrics_language_gpt) %>%
  mutate(match = ifelse(lyrics_language == lyrics_language_gpt, TRUE, FALSE)) %>%
  na.omit()

# what is the overall match percentage
gpt_fasttext %>%
  summarise(match_percent = sum(match) / n() * 100) %>%
  pull(match_percent)

# how many times when it was Ukrainian and Russian?
gpt_fasttext_ua <- gpt_fasttext %>%
  filter(lyrics_language == 'uk' | lyrics_language_gpt == 'uk')

gpt_fasttext_ua %>%
  summarise(match_percent = sum(match) / n() * 100) %>%
  pull(match_percent)

gpt_fasttext_ru <- gpt_fasttext %>%
  filter(lyrics_language == 'ru' | lyrics_language_gpt == 'ru')

gpt_fasttext_ru %>%
  summarise(match_percent = sum(match) / n() * 100) %>%
  pull(match_percent)


################################################################################
# LYRICS EMBEDDING: lyrical similarity
################################################################################
# for each lyric, iterate over to extract embedding vectors
lyrics_embed_storage = list()
for (i in 1:nrow(final_translations)) {
  tryCatch({
    embed <- openai::create_embedding(
      model = embedding_model,
      input = final_translations$cleaned_lyrics_gpt[i])
    lyrics_embed_storage[[i]] <- tibble(track_id = final_translations$track_id[i],
                                        embedding = embed$data$embedding,
                                        country_code = final_translations$country_code[i])

    print(sprintf('Done with %s', i))
  }, error = function(e){message(sprintf('Error, skipping %s', i))})
}
lyrics_embed_output <- do.call(rbind, lyrics_embed_storage)
lyrics_embed_output %>% write_rds('Dataset/lyrics/UA_RU_lyrics_embedding_gpt.rds')


################################################################################
# WORD EMBEDDING: war-related words
################################################################################
ru_uk_lyrics <- filtered_lyrics %>% filter(lyrics_language %in% c('uk', 'ru'))

# make a token table
tokens <- tokens(ru_uk_lyrics$cleaned_lyrics_gpt, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE)
tokens <- tokens_remove(tokens, pattern = stopwords("en"), padding = FALSE)
tokens_stemmed <- tokens_wordstem(tokens, language = "english")
tokens_stemmed <- tokens_select(tokens_stemmed, min_nchar = 3, padding = FALSE)

# get the document frequency of each word (number of unique songs each term appears in)
dfm_stemmed <- dfm(tokens_stemmed)
word_counts <- docfreq(dfm_stemmed) %>% sort(decreasing = T)

# filter words with at least 5 counts (5 unique songs)
word_counts <- word_counts[word_counts >= 5]

# how much does the word war appear? (for sanity check)
word_counts["war"]

all_words <- word_counts %>% names()

# extract the word embedding for each of the word
word_embed_storage = list()
for (i in 1:length(word_counts)) {
  tryCatch({
    embed <- openai::create_embedding(
      model = embedding_model,
      input = all_words[i])
    word_embed_storage[[i]] <- tibble(word = all_words[i], embedding = embed$data$embedding)
    
    print(sprintf('Done with %s', i))
  }, error = function(e){message(sprintf('Error, skipping %s', i))})
}
word_embed_output <- do.call(rbind, word_embed_storage)
word_embed_output %>% write_rds('Dataset/lyrics/UA_RU_word_embedding_gpt.rds')
