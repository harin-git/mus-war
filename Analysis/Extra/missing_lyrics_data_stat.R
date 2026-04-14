## Missing-lyrics bias checks for Reviewer 5.3
## Quantifies whether songs excluded from the sung-language analysis
## differ systematically, and shows that supplementing missing lyrics
## via Gemini search does not change the main results.

source('utils.R')

################################################################################
# DATA
################################################################################
chart_data <- read_rds('Dataset/chart/longitudinal_postsoviet_chart.rds')
gemini <- read_csv('Dataset/lyrics/gemini_postsoviet_lyrics.csv', show_col_types = FALSE)

sub_languages <- c('uk', 'ru', 'kk', 'en')

study_data <- chart_data %>% filter(study_window)

################################################################################
# 1. ORIGINAL MISSING-LYRICS STATISTICS
################################################################################
original_songs <- study_data %>%
  distinct(trackID, lyrics_lang, lyrics_lang_prob)

n_total <- nrow(original_songs)
n_missing_orig <- sum(is.na(original_songs$lyrics_lang))
n_with_orig <- n_total - n_missing_orig

cat(sprintf(
  'Original data: %d unique songs in study window, %d (%.1f%%) with lyrics, %d (%.1f%%) missing\n',
  n_total, n_with_orig, n_with_orig / n_total * 100,
  n_missing_orig, n_missing_orig / n_total * 100
))

# per-country breakdown
original_by_country <- study_data %>%
  distinct(country_code, trackID, lyrics_lang) %>%
  group_by(country_code) %>%
  summarise(total = n(),
            missing = sum(is.na(lyrics_lang)),
            pct_missing = missing / total * 100,
            .groups = 'drop')
cat('\nOriginal missing rate by country:\n')
print(original_by_country)

################################################################################
# 2. GEMINI SUPPLEMENT RECOVERY
################################################################################
missing_ids <- original_songs %>%
  filter(is.na(lyrics_lang)) %>%
  pull(trackID)

gemini_matched <- gemini %>%
  filter(song_id %in% missing_ids)

n_gemini_searched <- nrow(gemini_matched)
n_gemini_found <- sum(!is.na(gemini_matched$language))
n_gemini_null <- sum(is.na(gemini_matched$language))
n_not_searched <- length(missing_ids) - n_gemini_searched

cat(sprintf(
  '\nGemini supplement: searched %d of %d missing songs\n',
  n_gemini_searched, n_missing_orig
))
cat(sprintf(
  '  Recovered language: %d (%.1f%% of searched)\n',
  n_gemini_found, n_gemini_found / n_gemini_searched * 100
))
cat(sprintf(
  '  Still missing after Gemini: %d (%.1f%% of searched)\n',
  n_gemini_null, n_gemini_null / n_gemini_searched * 100
))
cat(sprintf(
  '  Not searched by Gemini: %d\n', n_not_searched
))

# after supplementing
n_missing_after <- n_missing_orig - n_gemini_found
cat(sprintf(
  '\nAfter supplement: %d of %d songs (%.1f%%) now have lyrics language\n',
  n_total - n_missing_after, n_total,
  (n_total - n_missing_after) / n_total * 100
))

# language breakdown of recovered songs
cat('\nLanguage breakdown of Gemini-recovered songs:\n')
gemini_matched %>%
  filter(!is.na(language)) %>%
  count(language, sort = TRUE) %>%
  print(n = 20)

################################################################################
# 2b. METADATA COMPARISON: MISSING vs NON-MISSING LYRICS SONGS
################################################################################
song_meta <- read_csv('Dataset/meta/song_metadata.csv', show_col_types = FALSE) %>%
  distinct(trackID, .keep_all = TRUE)

joined <- original_songs %>%
  left_join(song_meta, by = 'trackID') %>%
  mutate(has_lyrics = !is.na(lyrics_lang))

# preview audio and genre availability
cat('\n--- Metadata availability by lyrics status ---\n')
meta_avail <- joined %>%
  group_by(has_lyrics) %>%
  summarise(
    n = n(),
    pct_preview = sum(!is.na(preview_audio) & preview_audio != '') / n() * 100,
    pct_genres = sum(!is.na(genres) & genres != '') / n() * 100,
    pct_acoustic = sum(!is.na(loudness)) / n() * 100,
    .groups = 'drop'
  )
print(meta_avail)

cat('\nChi-square: preview audio\n')
print(chisq.test(table(joined$has_lyrics, !is.na(joined$preview_audio) & joined$preview_audio != '')))

cat('\nChi-square: genres\n')
print(chisq.test(table(joined$has_lyrics, !is.na(joined$genres) & joined$genres != '')))

# popularity: chart appearances, city reach, day reach
popularity <- study_data %>%
  group_by(trackID) %>%
  summarise(n_appearances = n(), n_cities = n_distinct(nodeID),
            n_days = n_distinct(date), .groups = 'drop') %>%
  left_join(original_songs %>% select(trackID, lyrics_lang), by = 'trackID') %>%
  mutate(has_lyrics = !is.na(lyrics_lang))

cat('\n--- Popularity by lyrics status ---\n')
popularity %>%
  group_by(has_lyrics) %>%
  summarise(across(c(n_appearances, n_cities, n_days),
                   list(mean = mean, median = median)),
            .groups = 'drop') %>%
  print(width = 200)

cat('\nWilcoxon: chart appearances\n')
print(wilcox.test(n_appearances ~ has_lyrics, data = popularity))
cat('\nWilcoxon: city reach\n')
print(wilcox.test(n_cities ~ has_lyrics, data = popularity))

# country-level missing rate: UA vs RU chi-square
cat('\nChi-square: missing rate UA vs RU\n')
ua_ru <- study_data %>%
  filter(country_code %in% c('UA', 'RU')) %>%
  distinct(country_code, trackID, lyrics_lang) %>%
  mutate(missing = is.na(lyrics_lang))
print(chisq.test(table(ua_ru$country_code, ua_ru$missing)))

################################################################################
# 3. ROBUSTNESS: RE-RUN MAIN SUNG-LANGUAGE TREND WITH SUPPLEMENTED DATA
################################################################################
# Build a lookup from gemini: song_id -> language
# Use the gemini language as lyrics_lang and confidence as lyrics_lang_prob
gemini_lookup <- gemini %>%
  filter(!is.na(language)) %>%
  select(song_id, gemini_lang = language, gemini_conf = confidence)

# Supplement the chart data: for rows where lyrics_lang is NA, fill from gemini
chart_supplemented <- chart_data %>%
  left_join(gemini_lookup, by = c('trackID' = 'song_id')) %>%
  mutate(
    lyrics_lang = coalesce(lyrics_lang, gemini_lang),
    lyrics_lang_prob = coalesce(lyrics_lang_prob, gemini_conf)
  ) %>%
  select(-gemini_lang, -gemini_conf)

# Verify improvement
supp_study <- chart_supplemented %>% filter(study_window)
supp_songs <- supp_study %>% distinct(trackID, lyrics_lang)
cat(sprintf(
  '\nSupplemented study data: %d songs with lyrics_lang (was %d), %d still missing (was %d)\n',
  sum(!is.na(supp_songs$lyrics_lang)), n_with_orig,
  sum(is.na(supp_songs$lyrics_lang)), n_missing_orig
))

# per-country after supplement
supp_by_country <- supp_study %>%
  distinct(country_code, trackID, lyrics_lang) %>%
  group_by(country_code) %>%
  summarise(total = n(),
            missing = sum(is.na(lyrics_lang)),
            pct_missing = missing / total * 100,
            .groups = 'drop')
cat('\nSupplemented missing rate by country:\n')
print(supp_by_country)

################################################################################
# 4. BOOTSTRAP THE SUPPLEMENTED DATA (main 6-month window only)
################################################################################
n_boot_supp <- 1000  # fewer iterations sufficient for robustness comparison

supp_clean <- chart_supplemented %>%
  ungroup() %>%
  mutate(lyrics_lang = ifelse(lyrics_lang_prob >= 0.7, lyrics_lang, NA)) %>%
  mutate(lyrics_lang = case_when(
    is.na(lyrics_lang) ~ NA,
    lyrics_lang %in% sub_languages ~ lyrics_lang,
    !lyrics_lang %in% sub_languages ~ 'other'
  ))

# Pre-filter to study window and convert to data.frame for speed
supp_trend_dt <- supp_clean %>%
  filter(study_window) %>%
  select(date, country_code, lyrics_lang) %>%
  as.data.frame()

cat('\nRunning bootstrap on supplemented data...\n')
supp_boot <- vector('list', n_boot_supp)
for (b in seq_len(n_boot_supp)) {
  smp <- supp_trend_dt %>%
    group_by(country_code, date) %>%
    sample_n(size = n(), replace = TRUE)

  lang <- smp %>%
    filter(!is.na(lyrics_lang)) %>%
    group_by(date, country_code) %>%
    reframe(get_proportion(lyrics_lang))

  supp_boot[[b]] <- lang %>% cbind(boot = b)
  if (b %% 50 == 0) cat(sprintf('  %d / %d\n', b, n_boot_supp))
}
supp_boot_output <- do.call(rbind, supp_boot) %>% mutate(date = as.Date(date))

# aggregate and plot
supp_agg <- filter_study_window(supp_boot_output) %>%
  group_by(date, country_code, name) %>%
  reframe(get_boot_mean_ci(prop, 'boot')) 

supp_agg$country_code <- factor(
  supp_agg$country_code,
  levels = SUB_COUNTRIES,
  labels = c('Ukraine', 'Russia', 'Belarus', 'Kazakhstan')
)

supp_agg <- supp_agg %>%
  mutate(name = factor(
    name,
    levels = c(sub_languages, 'other'),
    labels = c('Ukrainian', 'Russian', 'Kazakh', 'English', 'Other')
  ))

colour_pal <- c(UKRAINE_COLOUR, RUSSIAN_COLOUR, KAZAKHSTAN_COLOUR, ENGLISH_COLOUR, 'gray')

supp_agg %>%
  mutate(percent = boot_m * 100,
         ci_lower = boot_lower_ci * 100,
         ci_upper = boot_upper_ci * 100) %>%
  ggplot(aes(as.Date(date), percent, colour = name, fill = name)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, colour = NA) +
  geom_vline(xintercept = WAR_START) +
  theme_classic() +
  scale_colour_manual(values = colour_pal) +
  scale_fill_manual(values = colour_pal, guide = 'none') +
  scale_x_date(breaks = 'month', date_labels = '%b') +
  theme(legend.position = 'bottom') +
  labs(x = '', y = 'Proportion (%)', colour = 'Lyrics language') +
  facet_wrap(~ country_code, nrow = 1)

plot_save('SI/lyrics_lang_postsoviet_6month_gemini_supplemented', c(183, 70))
