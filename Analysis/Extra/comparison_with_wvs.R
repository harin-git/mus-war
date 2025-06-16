#' Compare how the socio-cultural values align with music listening, globally
#' Related to Methods in paper

# load study-wide functions and global variables
source('utils.R')

# load cleaned world-value-survey data
wvs <- read_rds('Dataset/wvs/clean_wvs.rds')
city_meta <- read_rds('Dataset/meta/city_metadata.rds')
music_sim <- read_rds('Dataset/data_comparison/shazam_music_sim.rds')

################################################################################
# WORLD CITIES AND CULTURAL VALUES
################################################################################
# match the WVS location with our data
loc_matched <- wvs %>%
  rowwise() %>%
  mutate(find_closest_point(lon, lat, city_meta), .after = country_code)

# filter for regions with less than 100km
loc_matched_clean <- loc_matched %>% 
  filter(distance < 1e5)
message(sprintf('after matching, there are %s cities remaining from the original 1,423', n_distinct(loc_matched_clean$nodeID)))

# squash with city aggregates
wvs_cities <- loc_matched_clean %>%
  group_by(nodeID, city_name) %>%
  summarise(across(starts_with('Q'), function(x){mean(x, na.rm = T)})) %>%
  ungroup()

# across the matched cities, compute the cosine similarity of wvs questions
matched_wvs_sim <- proxy::simil(wvs_cities %>% select(starts_with('Q')), method = 'cosine', diag = TRUE, upper = TRUE) %>% as.matrix()
matched_wvs_sim[is.na(matched_wvs_sim)] <- 1 # fill diagonols with 1

# make the same using music similarity matrix
matched_music_sim <- music_sim %>% filter(source %in% wvs_cities$nodeID, target %in% wvs_cities$nodeID)
matched_music_sim <- matched_music_sim %>%
  select(source, target, weight) %>%
  pivot_wider(names_from = target, values_from = weight) %>%
  select(-source) %>%
  as.matrix()

# report the correlations in the main text
cor.test(matched_wvs_sim, matched_music_sim)

