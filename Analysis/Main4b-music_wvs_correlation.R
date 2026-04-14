#' Correlations between local music proportion and socio-cultural values
#' Related to Fig. 4 in paper

source('utils.R')
require(sf)
require(psych)

################################################################################
# SETTINGS
################################################################################
COUNTRY_CODE <- 'RU'   # 'UA' or 'RU'
GLOBAL <- FALSE         # if TRUE use global results, otherwise per-country
TYPE <- 'pca'           # 'iw' for Inglehart-Welzel dimensions (main), 'pca' for PCA loadings (SI)

################################################################################
# PREPARATION
################################################################################
city_meta <- read_rds('Dataset/meta/city_metadata.rds')

find_closest_music_city <- function(lon, lat, dataset) {
  d <- geosphere::distVincentySphere(c(lon, lat), cbind(dataset$lon, dataset$lat))
  closest_index <- which.min(d)
  dataset[closest_index, ] %>%
    transmute(
      nodeID,
      music_city = city_name,
      music_lat = lat,
      music_lon = lon,
      pre,
      post,
      pre_post_change,
      distance = min(d)
    )
}

switch(COUNTRY_CODE,
  UA = {
    LOCAL_MUSIC <- read_rds("Dataset/lyrics/ukranian_lyrics_change_by_city.rds") %>%
      left_join(city_meta) %>%
      filter(country_code == COUNTRY_CODE)
    COLOR_GRADIENT <- c(UKRAINE_COLOUR2, UKRAINE_COLOUR)
  },
  RU = {
    CENSUS <- read_rds('Dataset/census/russia_census_2021_clean.rds') %>%
      filter(!is.na(iso_3166_2)) %>%
      distinct(iso_3166_2, .keep_all = TRUE)
    LOCAL_MUSIC <- read_rds("Dataset/lyrics/russian_lyrics_change_by_city.rds") %>%
      left_join(city_meta) %>%
      filter(country_code == COUNTRY_CODE)
    COLOR_GRADIENT <- c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
  }
)

WVS_DATA <- switch(TYPE,
  pca = {
    d <- switch(COUNTRY_CODE,
      UA = read_rds("Dataset/wvs/UA_wvs_pca_loadings.rds"),
      RU = read_rds("Dataset/wvs/RU_wvs_pca_loadings.rds")
    )
    if (GLOBAL) d <- read_rds('Dataset/wvs/global_wvs_pca_loadings.rds') %>% filter(country_code == COUNTRY_CODE)
    d
  },
  iw = {
    d <- switch(COUNTRY_CODE,
      UA = read_rds("Dataset/Inglehart_Welzel/UA_wvs_iw_scores.rds"),
      RU = read_rds("Dataset/Inglehart_Welzel/RU_wvs_iw_scores.rds")
    )
    if (GLOBAL) d <- read_rds('Dataset/Inglehart_Welzel/Global_wvs_iw_scores.rds') %>% filter(country_code == COUNTRY_CODE)
    d
  }
)

################################################################################
# MATCHING
################################################################################
plot_data_agg <- WVS_DATA %>%
  rowwise() %>%
  mutate(find_closest_music_city(lon, lat, LOCAL_MUSIC), .after = lon) %>%
  ungroup() %>%
  filter(distance < 1e5) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(percent_change = scale(pre_post_change / sum(pre + post) * 100))

message(sprintf('There are %s matched WVS locations in %s (%s)', nrow(plot_data_agg), COUNTRY_CODE, TYPE))

plot_data_agg <- switch(COUNTRY_CODE,
  UA = plot_data_agg %>% mutate(city_type = case_when(
    music_city %in% c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk', 'Mariupol') ~ 'Occupied',
    TRUE ~ 'Not occupied'
  )),
  RU = plot_data_agg %>% left_join(CENSUS, by = 'iso_3166_2')
)

################################################################################
# CORRELATIONS
################################################################################
dims <- switch(TYPE,
  pca = paste0('PC', 1:3),
  iw  = c('trad_secular', 'survival_selfexpr')
)

dimension_labels <- switch(TYPE,
  pca = setNames(paste0('PC', 1:3), paste0('PC', 1:3)),
  iw  = c(trad_secular = 'Traditional vs.\n Secular-rational',
           survival_selfexpr = 'Survival vs.\n Self-expression')
)

cor_result <- corr.test(
  plot_data_agg %>% select(percent_change, all_of(dims)),
  adjust = 'bonferroni',
  method = 'spearman'
)
print(cor_result, short = FALSE)
print(cor_result$stars, quote = FALSE, short = FALSE)

message(sprintf('Plotting the correlation between local music and %s for %s', TYPE, COUNTRY_CODE))

for (dim_name in dims) {
  p <- plot_data_agg %>%
    ggplot(aes(pre_post_change * 100, !!sym(dim_name))) +
    geom_smooth(method = 'glm', colour = 'gray50') +
    geom_point(aes(colour = city_type), size = 1, alpha = 0.5) +
    scale_colour_manual(values = COLOR_GRADIENT, guide = 'none')

  is_main <- (TYPE == 'iw')
  folder <- if (is_main) 'Main' else 'SI'

  if (!is_main) {
    p <- p + ggpubr::stat_cor(
      r.accuracy = 0.01,
      p.accuracy = 0.01,
      cor.coef.name = 'rho',
      method = 'spearman',
      size = 3
    )
  }

  p + labs(x = 'Local music (%)', y = dimension_labels[[dim_name]])

  plot_save(sprintf('%s/%s_music_%s_correlation', folder, COUNTRY_CODE, dim_name), c(40, 50))
}
