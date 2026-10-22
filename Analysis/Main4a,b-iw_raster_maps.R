#' Geographical maps of the change in local music (Fig. 4a) and of the Inglehart-Welzel dimensions (Fig. 4b)
#' in Ukraine and Russia, smoothed over the city locations. The maps are for visualisation only; all
#' inferential analyses use the city-level values.
#' Related to Fig. 4 in paper

source('utils.R')
require(sf)

################################################################################
## SETTINGS AND MAPPING FUNCTIONS
################################################################################
# Kernel smoothing: each grid cell is the weighted mean of the city values (city quartiles), with weights
# exp(-(d / (D0 * Dx))^l) normalised per cell, where d is the distance to a city and D0 the mean distance
# between cities. The grid resolution gives >= 300 dpi at print size.
sf_use_s2(FALSE)
CRS <- 4326
city_meta <- read_rds('Dataset/meta/city_metadata.rds')
world <- read_rds('Dataset/census/world_map.rds') %>% st_as_sf() %>% st_transform(CRS)
country_poly <- function(cc) {
  if (cc == 'UA') world %>% filter(iso_a2 == 'UA' | iso_3166_2 %in% c('UA-43','UA-40'))
  else world %>% filter(iso_a2 == 'RU', !iso_3166_2 %in% c('UA-43','UA-40')) %>% st_crop(xmin = 20, xmax = 100, ymin = 40, ymax = 62)
}

# Vectorised version of the kernel smoothing in Main4a,b
smooth_map <- function(pts, value, poly, res = raster_n(HALF_W), Dx = 0.1, l = 1) {
  bb <- st_bbox(poly); dx <- (bb['xmax'] - bb['xmin']) / res
  grid <- expand.grid(lon = seq(bb['xmin'], bb['xmax'], by = dx), lat = seq(bb['ymin'], bb['ymax'], by = dx))
  inside <- lengths(st_intersects(st_as_sf(grid, coords = c('lon','lat'), crs = CRS), st_union(poly))) > 0
  grid <- grid[inside, ]
  D0 <- mean(dist(pts[, c('lon','lat')]))
  grid$value <- NA_real_
  for (ix in split(seq_len(nrow(grid)), ceiling(seq_len(nrow(grid)) / 50000))) {   # chunked to limit memory
    d <- sqrt(outer(grid$lon[ix], pts$lon, '-')^2 + outer(grid$lat[ix], pts$lat, '-')^2)
    w <- exp(-(d / (D0 * Dx))^l); w <- w / rowSums(w)
    grid$value[ix] <- as.vector(w %*% value)
  }
  grid$cell <- unname(dx); grid
}
map_panel <- function(cc, pts, value, cols, legend_labels, title, quartile_before_crop = FALSE,
                      marker_scale = 1, fill_limits = c(1, 4)) {
  poly <- country_poly(cc)
  if (quartile_before_crop) pts <- pts %>% mutate(.map_quartile = ntile(.data[[value]], 4))
  pts <- pts %>% st_as_sf(coords = c('lon','lat'), crs = CRS, remove = FALSE) %>% { .[poly, ] } %>% st_drop_geometry()
  if (!quartile_before_crop) pts <- pts %>% mutate(.map_quartile = ntile(.data[[value]], 4))
  g <- smooth_map(pts, pts$.map_quartile, poly)
  # region (oblast) outlines, as in the original Main4a,b maps
  outline <- st_coordinates(st_cast(st_cast(st_geometry(poly), 'MULTIPOLYGON'), 'POLYGON')) %>% as_tibble() %>%
    mutate(grp = interaction(L1, L2, drop = TRUE))
  # capital shown as a square, other cities as crosses (original)
  pts <- pts %>% left_join(city_meta %>% distinct(country_code, city_name, .keep_all = TRUE) %>% select(country_code, city_name, fcode),
                           by = intersect(c('country_code', 'city_name'), names(pts))) %>%
    mutate(city_category = ifelse(!is.na(fcode) & fcode == 'PPLC', 'capital', 'non-capital'))
  raster <- if (is.null(fill_limits)) {
    raster_layer(g, 'lon', 'lat', 'value', cols[1], cols[2], interpolate = FALSE)
  } else {
    raster_layer(g, 'lon', 'lat', 'value', cols[1], cols[2], limits = fill_limits, interpolate = FALSE)
  }
  p <- ggplot() +
    raster +
    geom_path(data = outline, aes(X, Y, group = grp), colour = 'grey20', linewidth = LW_MIN) +
    geom_point(data = pts %>% filter(city_category == 'non-capital'), aes(lon, lat), shape = 4,
               size = 0.65 * marker_scale, stroke = 0.25 * marker_scale, colour = if (cc == 'UA') 'black' else 'white') +
    geom_point(data = pts %>% filter(city_category == 'capital'), aes(lon, lat), shape = 15,
               size = 1.05, colour = if (cc == 'UA') 'black' else 'white') +
    coord_fixed(ratio = 1 / cos(mean(range(g$lat)) * pi / 180), expand = FALSE) +
    scale_x_continuous(labels = lon_lab) + scale_y_continuous(labels = lat_lab) +
    theme_map_axes() + theme(plot.margin = margin(0, 4, 1, 1, 'mm'))
  # ggplot places endpoint labels above/below a horizontal colour bar. The previous figure instead used
  # low label | gradient | high label on one line, so construct that compact legend explicitly.
  leg <- ggplot(tibble(x = seq(0.36, 0.64, length.out = 600), y = 0.5), aes(x, y, fill = x)) +
    geom_raster() + scale_fill_gradient(low = cols[1], high = cols[2], guide = 'none') +
    annotate('text', x = 0.33, y = 0.5, label = legend_labels[1], hjust = 1,
             family = FONT, size = (BASE - 0.5) * PT, lineheight = 0.9) +
    annotate('text', x = 0.67, y = 0.5, label = legend_labels[2], hjust = 0,
             family = FONT, size = (BASE - 0.5) * PT, lineheight = 0.9) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = 'off') +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
  p <- leg / p + plot_layout(heights = c(0.13, 1))
  list(plot = p, data = pts %>% select(any_of(c('nodeID', 'city_name', 'city', 'lat', 'lon', 'pre', 'post', 'pre_post_change', value))))   # city values; the smoothed grid is derived
}

## a — change in local music (absolute % change, quartiles), as in the original Fig. 4a
mus <- function(cc) read_rds(sprintf('Dataset/lyrics/%s_lyrics_change_by_city.rds', ifelse(cc == 'UA','ukranian','russian'))) %>%
  left_join(city_meta %>% select(nodeID, country_code, city_name, lat, lon), by = 'nodeID') %>%
  filter(country_code == cc) %>%
  mutate(abs_change = abs(pre_post_change / sum(pre + post)) * 100)
a_ua <- map_panel('UA', mus('UA'), 'abs_change', c(UKRAINE_COLOUR, UKRAINE_COLOUR2), c('Smaller\nincrease','Larger\nincrease'), NULL)
a_ru <- map_panel('RU', mus('RU'), 'abs_change', c(RUSSIAN_COLOUR2, RUSSIAN_COLOUR), c('Smaller\ndecline','Larger\ndecline'), NULL)
plot_save('Main/UA_local_music_change_map', c(75, 62), a_ua$data, a_ua$plot, source_name = 'Fig_4a_Ukraine')
plot_save('Main/RU_local_music_change_map', c(HALF_W, 60), a_ru$data, a_ru$plot, source_name = 'Fig_4a_Russia')

## b — Inglehart-Welzel maps (UA: survival vs self-expression; RU: traditional vs secular-rational)
iw <- function(cc) read_rds(sprintf('Dataset/Inglehart_Welzel/%s_wvs_iw_scores.rds', cc))
b_ua <- map_panel('UA', iw('UA'), 'survival_selfexpr', c(UKRAINE_COLOUR, UKRAINE_COLOUR2), c('Survival','Self-expression'), NULL,
                  quartile_before_crop = TRUE, marker_scale = 0.7, fill_limits = NULL)
b_ru <- map_panel('RU', iw('RU'), 'trad_secular', c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2), c('Traditional','Secular-rational'), NULL,
                  quartile_before_crop = TRUE, marker_scale = 0.7, fill_limits = NULL)
plot_save('Main/UA_iw_survival_selfexpr_map', c(45, 42), b_ua$data, b_ua$plot, source_name = 'Fig_4b_Ukraine_map')
plot_save('Main/RU_iw_trad_secular_map', c(50, 42), b_ru$data, b_ru$plot, source_name = 'Fig_4b_Russia_map')
