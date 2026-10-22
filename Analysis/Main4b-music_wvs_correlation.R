#' Correlations between the change in local music and socio-cultural values (Inglehart-Welzel dimensions)
#' WVS respondent locations are matched to the nearest Shazam city within 100 km.
#' Related to Fig. 4b in paper

source('utils.R')

################################################################################
# SETTINGS
################################################################################
TYPE <- 'iw'           # 'iw' for Inglehart-Welzel dimensions (main), 'pca' for PCA loadings (exploratory)

################################################################################
# PREPARATION
################################################################################
city_meta <- read_rds('Dataset/meta/city_metadata.rds')
russia_census <- read.csv('Dataset/census/russia_census_city_mapped.csv')

find_closest_music_city <- function(lon, lat, dataset) {
  d <- geosphere::distVincentySphere(c(lon, lat), cbind(dataset$lon, dataset$lat))
  closest_index <- which.min(d)
  dataset[closest_index, ] %>%
    transmute(nodeID, music_city = city_name, music_lat = lat, music_lon = lon, pre, post, pre_post_change,
              distance = min(d))
}

local_music <- function(cc) {
  read_rds(sprintf('Dataset/lyrics/%s_lyrics_change_by_city.rds', ifelse(cc == 'UA', 'ukranian', 'russian'))) %>%
    left_join(city_meta %>% select(nodeID, country_code, city_name, lat, lon), by = 'nodeID') %>%
    filter(country_code == cc)
}

wvs_data <- function(cc) {
  switch(TYPE,
         pca = read_rds(sprintf('Dataset/wvs/%s_wvs_pca_loadings.rds', cc)),
         iw  = read_rds(sprintf('Dataset/Inglehart_Welzel/%s_wvs_iw_scores.rds', cc)))
}

dims <- switch(TYPE, pca = paste0('PC', 1:3), iw = c('trad_secular', 'survival_selfexpr'))

################################################################################
# MATCHING
################################################################################
matched <- map(c(UA = 'UA', RU = 'RU'), function(cc) {
  lm_ <- local_music(cc)
  m <- wvs_data(cc) %>%
    rowwise() %>%
    mutate(find_closest_music_city(lon, lat, lm_), .after = lon) %>%
    ungroup() %>%
    filter(distance < 1e5) %>%
    na.omit() %>%
    mutate(percent_change = as.numeric(scale(pre_post_change / sum(pre + post) * 100)),
           local_music_change = pre_post_change * 100,
           group = if (cc == 'UA') {
             ifelse(music_city %in% c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk', 'Mariupol'), 'Occupied', 'Not occupied')
           } else {
             ifelse(music_city %in% russia_census$city_name[russia_census$local_prop < 50], 'Minority region', 'Non-minority region')
           })
  message(sprintf('%s: %s matched WVS locations in %s cities', cc, nrow(m), n_distinct(m$nodeID)))
  m
})

################################################################################
# CORRELATIONS
################################################################################
# two-tailed Spearman rho(df) with 95% CI (Fisher z, SE = 1.03 / sqrt(n - 3)), Bonferroni-corrected across the
# dimensions within each country; n = matched WVS locations (n_cities = the cities they match)
spearman_ci <- function(x, y) {
  n <- length(x)
  ct <- suppressWarnings(cor.test(x, y, method = 'spearman', exact = FALSE))
  rho <- unname(ct$estimate); se <- 1.03 / sqrt(n - 3)
  tibble(n, rho, df = n - 2, lower_ci = tanh(atanh(rho) - 1.96 * se), upper_ci = tanh(atanh(rho) + 1.96 * se), p = ct$p.value)
}

map_dfr(names(matched), function(cc) {
  m <- matched[[cc]]
  map_dfr(dims, ~ spearman_ci(m$percent_change, m[[.x]]) %>% mutate(dimension = .x, .before = 1)) %>%
    mutate(country = cc, n_cities = n_distinct(m$nodeID), .before = 1) %>%
    mutate(p_bonferroni = p.adjust(p, method = 'bonferroni'))
}) %>% print(width = Inf)

################################################################################
# PLOT (Fig. 4b scatter, for the dimension shown in each country's map)
################################################################################
scatter_plot <- function(d, ycol, ylab, cols) {
  ggplot(d, aes(local_music_change, .data[[ycol]])) +
    geom_smooth(method = 'lm', formula = y ~ x, colour = 'grey40', fill = 'grey80', linewidth = 0.35) +
    geom_point(aes(colour = group), size = 0.6, alpha = 0.7, shape = 16) +
    scale_colour_manual(values = cols, name = NULL) +
    labs(x = 'Local music\nchange (%)', y = ylab) +
    theme(legend.position = 'none', plot.margin = margin(1, 2, 1, 1, 'mm'))
}

if (TYPE == 'iw') {
  plot_save('Main/UA_music_survival_selfexpr_correlation', c(32, 40),
            matched$UA %>% select(music_city, lat, lon, local_music_change, survival_selfexpr, group),
            scatter_plot(matched$UA, 'survival_selfexpr', 'Survival vs\nself-expression',
                         c(Occupied = UKRAINE_COLOUR, `Not occupied` = UKRAINE_COLOUR2)), source_name = 'Fig_4b_Ukraine_scatter')
  plot_save('Main/RU_music_trad_secular_correlation', c(32, 40),
            matched$RU %>% select(music_city, lat, lon, local_music_change, trad_secular, group),
            scatter_plot(matched$RU, 'trad_secular', 'Traditional vs\nsecular-rational',
                         c(`Minority region` = RUSSIAN_COLOUR, `Non-minority region` = RUSSIAN_COLOUR2)), source_name = 'Fig_4b_Russia_scatter')
}
