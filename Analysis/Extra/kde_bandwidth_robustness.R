## KDE bandwidth (h) robustness check for Reviewer 5.1
## Tests whether the JSD results are sensitive to the choice of kernel width.
## Computes point-estimate JSD (actual minus shuffled baseline) for three
## values of h, for both lyrics and acoustic UMAP spaces.

source('utils.R')
require(uwot)
require(philentropy)

set.seed(42)

################################################################################
# HELPER FUNCTIONS (from Main3a, adapted for variable h)
################################################################################
get_kde <- function(df, lyrics_lang, type, h, nbin = 512,
                    x_range, y_range) {
  df <- df %>% filter(pre_post == type, lyrics_language == lyrics_lang)
  kde <- MASS::kde2d(df$x, df$y, h = c(h, h), n = nbin,
                     lims = c(x_range, y_range))
  dens <- as.vector(kde$z)
  dens / sum(dens)
}

compute_jsd <- function(umap_df, lyrics_lang, h, x_range, y_range,
                        n_shuffle = 50) {
  pre_dens  <- get_kde(umap_df, lyrics_lang, 'pre',  h, x_range = x_range, y_range = y_range)
  post_dens <- get_kde(umap_df, lyrics_lang, 'post', h, x_range = x_range, y_range = y_range)
  jsd_actual <- philentropy::JSD(rbind(pre_dens, post_dens))

  # shuffled-label baseline (average over n_shuffle permutations for stability)
  subset <- umap_df %>% filter(lyrics_language == lyrics_lang)
  jsd_baselines <- numeric(n_shuffle)
  for (s in seq_len(n_shuffle)) {
    shuffled <- subset %>% mutate(pre_post = sample(pre_post, n(), replace = TRUE))
    pre_s  <- get_kde(shuffled, lyrics_lang, 'pre',  h, x_range = x_range, y_range = y_range)
    post_s <- get_kde(shuffled, lyrics_lang, 'post', h, x_range = x_range, y_range = y_range)
    jsd_baselines[s] <- philentropy::JSD(rbind(pre_s, post_s))
  }

  tibble(
    h = h,
    jsd_actual = jsd_actual,
    jsd_baseline_mean = mean(jsd_baselines),
    jsd_baseline_sd = sd(jsd_baselines),
    estimate = jsd_actual - mean(jsd_baselines)
  )
}

################################################################################
# LYRICS UMAP
################################################################################
song_meta <- read_rds('Dataset/meta/song_metadata.rds')
lyrics_embedding <- read_rds('Dataset/lyrics/UA_RU_lyrics_embedding_gpt.rds') %>%
  rename(trackID = track_id)

lyrics_embedding <- lyrics_embedding %>%
  left_join(song_meta %>% select(trackID, pre_post, lyrics_language), by = 'trackID')

lyrics_embedding <- lyrics_embedding %>%
  group_by(trackID, country_code) %>%
  slice(1) %>%
  unnest(embedding) %>%
  mutate(index = paste0('d', 1:n())) %>%
  pivot_wider(names_from = index, values_from = embedding) %>%
  ungroup()

lyrics_feat <- lyrics_embedding %>% select(-trackID, -pre_post, -lyrics_language, -country_code)
lyrics_meta <- lyrics_embedding %>% select(trackID, pre_post, lyrics_language, country_code)

umap_lyrics <- lyrics_feat %>% umap(n_neighbors = 10, n_components = 2, min_dist = 0.1, metric = 'cosine')
umap_lyrics <- cbind.data.frame(setNames(as.data.frame(umap_lyrics), c('x', 'y')), lyrics_meta)
umap_lyrics$pre_post <- factor(umap_lyrics$pre_post, levels = c('pre', 'post'))

x_range_l <- round(range(umap_lyrics$x)) + c(-1, 1)
y_range_l <- round(range(umap_lyrics$y)) + c(-1, 1)

################################################################################
# ACOUSTIC UMAP
################################################################################
acoustic_embedding <- song_meta %>%
  filter(!is.na(lyrics_language), !is.na(mfcc1), country_code %in% c('RU', 'UA')) %>%
  select(trackID, pre_post, lyrics_language, loudness:mfcc12) %>%
  group_by(trackID) %>% slice(1) %>% ungroup() %>%
  mutate(across(loudness:mfcc12, scale))

acoustic_feat <- acoustic_embedding %>% select(-trackID, -pre_post, -lyrics_language)
acoustic_meta <- acoustic_embedding %>% select(trackID, pre_post, lyrics_language)

umap_acoustic <- acoustic_feat %>% umap(n_neighbors = 10, n_components = 2, min_dist = 0.1, metric = 'cosine')
umap_acoustic <- cbind.data.frame(setNames(as.data.frame(umap_acoustic), c('x', 'y')), acoustic_meta)
umap_acoustic$pre_post <- factor(umap_acoustic$pre_post, levels = c('pre', 'post'))

x_range_a <- round(range(umap_acoustic$x)) + c(-1, 1)
y_range_a <- round(range(umap_acoustic$y)) + c(-1, 1)

################################################################################
# SWEEP OVER h VALUES
################################################################################
h_lyrics   <- c(0.5, 1.0, 2.0)   # main analysis uses h = 1
h_acoustic <- c(0.5, 1.0, 2.0)   # main analysis uses h = 1

cat('=== LYRICS JSD: bandwidth robustness ===\n')
lyrics_results <- list()
for (h in h_lyrics) {
  for (lang in c('uk', 'ru')) {
    country <- ifelse(lang == 'uk', 'UA', 'RU')
    cat(sprintf('  lyrics h=%.1f, %s ... ', h, country))
    res <- compute_jsd(umap_lyrics, lang, h, x_range_l, y_range_l)
    res$country_code <- country
    res$feature <- 'lyrics'
    lyrics_results[[length(lyrics_results) + 1]] <- res
    cat(sprintf('JSD=%.5f, baseline=%.5f, estimate=%.5f\n',
                res$jsd_actual, res$jsd_baseline_mean, res$estimate))
  }
}

cat('\n=== ACOUSTIC JSD: bandwidth robustness ===\n')
acoustic_results <- list()
for (h in h_acoustic) {
  for (lang in c('uk', 'ru')) {
    country <- ifelse(lang == 'uk', 'UA', 'RU')
    cat(sprintf('  acoustic h=%.1f, %s ... ', h, country))
    res <- compute_jsd(umap_acoustic, lang, h, x_range_a, y_range_a)
    res$country_code <- country
    res$feature <- 'acoustic'
    acoustic_results[[length(acoustic_results) + 1]] <- res
    cat(sprintf('JSD=%.5f, baseline=%.5f, estimate=%.5f\n',
                res$jsd_actual, res$jsd_baseline_mean, res$estimate))
  }
}

################################################################################
# SUMMARY TABLE
################################################################################
all_results <- bind_rows(lyrics_results, acoustic_results)

cat('\n========================================\n')
cat('KDE bandwidth robustness summary\n')
cat('========================================\n')
all_results %>%
  mutate(across(where(is.numeric), ~ round(.x, 5))) %>%
  select(feature, country_code, h, jsd_actual, jsd_baseline_mean, estimate) %>%
  arrange(feature, country_code, h) %>%
  print(n = 30)

# flag: does the sign/direction hold across all h values?
cat('\nSign consistency check:\n')
all_results %>%
  group_by(feature, country_code) %>%
  summarise(
    all_positive = all(estimate > 0),
    min_estimate = min(estimate),
    max_estimate = max(estimate),
    .groups = 'drop'
  ) %>%
  print()
