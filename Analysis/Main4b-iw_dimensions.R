#' Inglehart-Welzel weighted dimensions using WVS Wave 7
#' Additive analysis parallel to the PCA-based WVS pipeline
#' Related to Reviewer 3.2

source('utils.R')

output_dir <- 'Dataset/Inglehart_Welzel'
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

################################################################################
# DATA
################################################################################
load('Dataset/wvs/WVS_Cross-National_Wave_7_rData_v5_0.rdata')
wvs_raw <- `WVS_Cross-National_Wave_7_v5_0`

################################################################################
# HELPERS
################################################################################
na_if_negative <- function(x) ifelse(x < 0, NA_real_, as.numeric(x))

yes_no_to_binary <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}

materialist_index_4 <- function(first_choice, second_choice) {
  materialist <- c(1, 3)
  postmaterialist <- c(2, 4)
  dplyr::case_when(
    is.na(first_choice) | is.na(second_choice) ~ NA_real_,
    first_choice %in% materialist & second_choice %in% materialist ~ 1,
    first_choice %in% materialist & second_choice %in% postmaterialist ~ 2,
    first_choice %in% postmaterialist & second_choice %in% materialist ~ 3,
    first_choice %in% postmaterialist & second_choice %in% postmaterialist ~ 4,
    TRUE ~ NA_real_
  )
}

weighted_composite <- function(df, vars, weights) {
  scaled_df <- df %>% mutate(across(all_of(vars), ~ as.numeric(scale(.x))))
  score_mat <- as.matrix(scaled_df %>% select(all_of(vars)))
  as.numeric(score_mat %*% weights / sum(weights))
}

################################################################################
# PREPARE UA/RU WVS FROM RAW DATA
################################################################################
ua_ru_qs <- wvs_raw %>%
  as_tibble() %>%
  filter(B_COUNTRY_ALPHA %in% c('RUS', 'UKR')) %>%
  transmute(
    country_code = ifelse(B_COUNTRY_ALPHA == 'RUS', 'RU', 'UA'),
    region_id = N_REGION_ISO,
    lon = O1_LONGITUDE,
    lat = O2_LATITUDE,
    Q8  = na_if_negative(Q8),
    Q14 = na_if_negative(Q14),
    Q15 = na_if_negative(Q15),
    Q17 = na_if_negative(Q17),
    Q45 = na_if_negative(Q45),
    Q46 = na_if_negative(Q46),
    Q57 = na_if_negative(Q57),
    Q154 = na_if_negative(Q154),
    Q155 = na_if_negative(Q155),
    Q164 = na_if_negative(Q164),
    Q182 = na_if_negative(Q182),
    Q184 = na_if_negative(Q184),
    Q209 = na_if_negative(Q209),
    Q254 = na_if_negative(Q254)
  ) %>%
  filter(lon < 360, lat > -90, lat < 90)

ua_ru_qs <- ua_ru_qs %>%
  mutate(
    autonomy_index = rowMeans(
      cbind(yes_no_to_binary(Q8), yes_no_to_binary(Q14),
            1 - yes_no_to_binary(Q15), 1 - yes_no_to_binary(Q17)),
      na.rm = TRUE
    ),
    god_secular            = 11 - Q164,
    abortion_secular       = Q184,
    national_pride_secular = ifelse(Q254 == 5, NA_real_, Q254),
    authority_secular      = Q45,
    postmaterialist_4      = materialist_index_4(Q154, Q155),
    happiness_selfexpr     = 5 - Q46,
    homosexuality_selfexpr = Q182,
    petition_selfexpr      = 4 - Q209,
    trust_selfexpr         = 3 - Q57
  )

################################################################################
# MAP RESPONDENTS TO ISO 3166-2 REGIONS
################################################################################
sf::sf_use_s2(FALSE)
map <- read_rds('Dataset/census/world_map.rds')
crs <- "+proj=longlat +datum=WGS84"
sf_map <- map %>% sf::st_as_sf(CRS = crs)

region_points <- ua_ru_qs %>%
  select(region_id, lon, lat) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = crs)

mapping <- sf::st_within(region_points, sf_map) %>% as.character() %>% as.numeric()
mapping[is.na(mapping)] <- 1
mapped_regions <- map[mapping, ] %>% as_tibble() %>% select(iso_3166_2, city_name = name, name_local, type)

ua_ru_qs <- cbind(mapped_regions, ua_ru_qs) %>%
  filter(iso_3166_2 != map$iso_3166_2[1]) %>%
  mutate(country_code = ifelse(iso_3166_2 %in% c('UA-43', 'UA-40'), 'UA', country_code))

################################################################################
# AGGREGATE TO REGIONAL LEVEL
################################################################################
iw_vars <- c(
  'autonomy_index', 'god_secular', 'abortion_secular',
  'national_pride_secular', 'authority_secular',
  'postmaterialist_4', 'happiness_selfexpr',
  'homosexuality_selfexpr', 'petition_selfexpr', 'trust_selfexpr'
)

region_grouping <- ua_ru_qs %>%
  mutate(lat = signif(lat, 3), lon = signif(lon, 3)) %>%
  group_by(country_code, region_id, iso_3166_2, city_name, lat, lon) %>%
  summarise(across(all_of(iw_vars), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
  group_by(country_code, iso_3166_2) %>%
  mutate(across(all_of(iw_vars), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%
  ungroup()

################################################################################
# COMPUTE WEIGHTED DIMENSIONS
################################################################################
trad_vars <- c(
  'god_secular',
  'autonomy_index',
  'abortion_secular',
  'national_pride_secular',
  'authority_secular'
)
trad_weights <- c(0.70, 0.61, 0.61, 0.60, 0.51)

survival_vars <- c(
  'postmaterialist_4',
  'happiness_selfexpr',
  'homosexuality_selfexpr',
  'petition_selfexpr',
  'trust_selfexpr'
)
survival_weights <- c(0.59, 0.59, 0.58, 0.54, 0.44)

for (cnt in c('UA', 'RU', 'Global')) {
  score_data <- if (cnt == 'Global') region_grouping else region_grouping %>% filter(country_code == cnt)

  score_data$trad_secular <- weighted_composite(score_data, trad_vars, trad_weights)
  score_data$survival_selfexpr <- weighted_composite(score_data, survival_vars, survival_weights)

  score_data_iso <- score_data %>%
    group_by(country_code, iso_3166_2) %>%
    summarise(
      city_name = dplyr::first(city_name),
      lat = mean(lat, na.rm = TRUE),
      lon = mean(lon, na.rm = TRUE),
      trad_secular = mean(trad_secular, na.rm = TRUE),
      survival_selfexpr = mean(survival_selfexpr, na.rm = TRUE),
      .groups = 'drop'
    )

  score_data %>% write_rds(file.path(output_dir, sprintf('%s_wvs_iw_scores.rds', cnt)))
  score_data %>% write_csv(file.path(output_dir, sprintf('%s_wvs_iw_scores.csv', cnt)))
  score_data_iso %>% write_rds(file.path(output_dir, sprintf('%s_wvs_iw_scores_iso.rds', cnt)))
  score_data_iso %>% write_csv(file.path(output_dir, sprintf('%s_wvs_iw_scores_iso.csv', cnt)))
}
