#' Change in individual acoustic features of local songs (Ukrainian songs in Ukraine, Russian songs in Russia)
#' from pre- to post-invasion. Reported as statistics only (Methods and Supplementary Information).
#' Related to Fig. 3 in paper (acoustic analysis)

# load study-wide functions and global variables
source('utils.R')
library(BayesFactor) # Bayes factors for the null results

################################################################################
# PREPARATION
################################################################################
# one row per song; a song's period is the period of its first chart appearance in Ukraine or Russia
chart_data <- read_rds('Dataset/chart/study_set_chart.rds') %>% filter(country_code %in% c('UA', 'RU'))
first_period <- chart_data %>%
  group_by(trackID) %>% arrange(date) %>% slice(1) %>% ungroup() %>%
  transmute(trackID, pre_post = ifelse(date < WAR_START, 'pre', 'post'))

# acoustic features, z-scored across all songs
acoustic_data <- read_rds('Dataset/meta/song_metadata.rds') %>%
  select(trackID, lyrics_language,
         loudness,
         dynamics = dynamic_complexity,
         richness = spectral_complexity,
         brightness = spectral_centroid,
         `chord change` = chord_change_rate,
         `zero crossing` = zcr,
         mode = major_minor,
         tempo = bpm) %>%
  group_by(trackID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(across(loudness:tempo, ~ as.numeric(scale(.x))))

# local songs discovered in each country, each song once
local_songs <- chart_data %>%
  distinct(country_code, trackID) %>%
  left_join(first_period, by = 'trackID') %>%
  left_join(acoustic_data, by = 'trackID') %>%
  filter((country_code == 'UA' & lyrics_language == 'uk') | (country_code == 'RU' & lyrics_language == 'ru')) %>%
  pivot_longer(loudness:tempo, names_to = 'feature') %>%
  filter(!is.na(value))

# number of songs per country and period
local_songs %>% distinct(country_code, pre_post, trackID) %>% count(country_code, pre_post)


################################################################################
# BOOTSTRAP (songs resampled within country, period and feature)
################################################################################
n_boot <- 1000
set.seed(1)
feature_boot <- map_dfr(1:n_boot, function(b)
  local_songs %>%
    group_by(country_code, pre_post, feature) %>%
    summarise(m = mean(sample(value, n(), replace = TRUE)), .groups = 'drop') %>%
    mutate(boot = b))

# change (post - pre) in z-scored units: bootstrap mean, 95% CI, z and P, Bonferroni-corrected over 16 tests
feature_change <- feature_boot %>%
  pivot_wider(names_from = pre_post, values_from = m) %>%
  mutate(change = post - pre) %>%
  group_by(country_code, feature) %>%
  reframe(get_boot_mean_ci(change, 'change')) %>%
  mutate(change_p_bonferroni = p.adjust(change_p, method = 'bonferroni'))

# Bayes factors (BF01, evidence for no change; default JZS prior) for each comparison
feature_bf <- local_songs %>%
  group_by(country_code, feature) %>%
  group_modify(~ tibble(BF01 = 1 / exp(ttestBF(.x$value[.x$pre_post == 'post'], .x$value[.x$pre_post == 'pre'])@bayesFactor$bf))) %>%
  ungroup()

feature_change %>%
  left_join(feature_bf, by = c('country_code', 'feature')) %>%
  arrange(country_code, change_p) %>%
  print(n = Inf, width = Inf)
