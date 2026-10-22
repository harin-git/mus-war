#' Temporal trends in local music in cities in Ukraine and Russia (Fig. 4c)
#' Run from the repository root. Saved bootstraps are used by default.
source('utils.R')

################################################################################
# PREPARATION AND SAVED BOOTSTRAPS
################################################################################
SIMULATION <- FALSE
n_boot <- 1000
occupied_2014 <- c('Sebastopol', 'Donetsk', 'Kerch', 'Luhansk')
ru_census <- read.csv('Dataset/census/russia_census_city_mapped.csv')
minority_cities <- ru_census$city_name[ru_census$local_prop < 50]

if (SIMULATION) {
  chart_clean <- read_rds('Dataset/chart/longitudinal_postsoviet_chart.rds') %>%
    ungroup() %>% filter(study_window, country_code %in% c('RU', 'UA'), lyrics_lang_prob >= 0.7)
  city_meta <- read_rds('Dataset/meta/city_metadata.rds')
  city_boot <- vector('list', n_boot)
  for (i in seq_len(n_boot)) {
    smp <- chart_clean %>% group_by(country_code, nodeID, date) %>% sample_n(n(), replace = TRUE)
    city_boot[[i]] <- smp %>% count(country_code, nodeID, date, lyrics_lang) %>%
      group_by(country_code, nodeID, date) %>% mutate(prop = n / sum(n) * 100, boot = i) %>%
      filter((country_code == 'RU' & lyrics_lang == 'ru') | (country_code == 'UA' & lyrics_lang == 'uk'))
    print(i)
  }
  city_boot_output <- bind_rows(city_boot) %>% ungroup() %>%
    left_join(city_meta %>% select(nodeID, city_name), by = 'nodeID') %>%
    mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post'))
  write_rds(city_boot_output, sprintf('Bootstraps/city_level_local_music_boot%s.rds', n_boot))
} else {
  boot_load()
  city_boot_output <- read_rds(sprintf('Bootstraps/city_level_local_music_boot%s.rds', n_boot)) %>% ungroup()
}

################################################################################
# ORIGINAL PLOTTING PATH — daily means, original LOESS, colours and line types
################################################################################
city_agg <- city_boot_output %>%
  group_by(country_code, city_name, nodeID, date, lyrics_lang) %>%
  reframe(get_boot_mean_ci(prop, 'boot')) %>% ungroup() %>% mutate(date = as.Date(date))

plot_city_level <- function(country) {
  if (country == 'UA') {
    lang <- 'uk'
    city_categories <- c('Occupied', 'Not occupied')
    sub_cities <- c(occupied_2014, 'Mariupol')
    colour_pal <- c(UKRAINE_COLOUR2, UKRAINE_COLOUR)
  } else {
    lang <- 'ru'
    city_categories <- c('Minority', 'Non-minority')
    sub_cities <- minority_cities
    colour_pal <- c(RUSSIAN_COLOUR, RUSSIAN_COLOUR2)
  }
  plot_cities <- city_agg %>% filter(country_code == country, lyrics_lang == lang) %>%
    select(city_name, date, mean = boot_m) %>%
    complete(city_name, date, fill = list(mean = 0)) %>%
    mutate(country_code = country,
           group = case_when(
             country == 'UA' & city_name %in% occupied_2014 ~ 'Occupied in 2014',
             country == 'UA' & city_name == 'Mariupol' ~ 'Occupied in 2022 (Mariupol)',
             country == 'UA' ~ 'Not occupied',
             city_name %in% minority_cities ~ 'Minority region',
             TRUE ~ 'Non-minority region'))
  cols <- if (country == 'UA')
    c(`Not occupied` = UKRAINE_COLOUR2, `Occupied in 2014` = UKRAINE_COLOUR,
      `Occupied in 2022 (Mariupol)` = UKRAINE_COLOUR) else
    c(`Minority region` = RUSSIAN_COLOUR, `Non-minority region` = RUSSIAN_COLOUR2)
  ggplot(plot_cities, aes(date, mean, colour = group, group = city_name)) +
    geom_smooth(aes(linetype = group), method = 'loess', formula = y ~ x,
                se = FALSE, span = 1, linewidth = 0.3) +
    geom_vline(xintercept = WAR_START, linetype = 'dashed', linewidth = LW) +
    scale_colour_manual(values = cols, name = NULL) +
    scale_linetype_manual(values = setNames(ifelse(names(cols) == 'Occupied in 2022 (Mariupol)',
                                                  'dashed', 'solid'), names(cols)), name = NULL) +
    coord_cartesian(ylim = c(0, 60)) + date_x + labs(x = NULL, y = 'Local music (%)') +
    theme(legend.position = 'bottom', legend.key.width = unit(4, 'mm'))
}

ua_plot <- plot_city_level('UA')
ru_plot <- plot_city_level('RU')
# Preserve the latest approved separate panels, not the older HEAD layout.
plot_save('Main/UA_city_level_local_music', c(85, 45), ua_plot$data, ua_plot, source_name = 'Fig_4c_Ukraine')
plot_save('Main/RU_city_level_local_music', c(85, 45), ru_plot$data, ru_plot, source_name = 'Fig_4c_Russia')

################################################################################
# UKRAINE — corrected baseline and paired bootstrap contrasts (s07/s10)
################################################################################
# Complete across BOTH periods before classifying them. Grouping complete() by
# pre_post loses Kerch and Sebastopol from the pre-invasion baseline because they
# have no Ukrainian-language rows then. Do not reuse the old 26.6395 pp contrast.
ua_cities <- city_boot_output %>% filter(country_code == 'UA', lyrics_lang == 'uk') %>%
  select(boot, city_name, date, prop) %>%
  complete(city_name, date, boot, fill = list(prop = 0)) %>%
  mutate(pre_post = ifelse(as.Date(date) < WAR_START, 'pre', 'post'),
         occupy = ifelse(city_name %in% occupied_2014, 'occupied', 'not_occupied'))

ukraine_homogeneity <- ua_cities %>% group_by(pre_post, boot) %>%
  summarise(spread = sd(prop), .groups = 'drop') %>% group_by(pre_post) %>% reframe(boot_test(spread))
occupation_boot <- ua_cities %>% group_by(occupy, boot) %>%
  summarise(pre = mean(prop[pre_post == 'pre']), post = mean(prop[pre_post == 'post']),
            welch(prop[pre_post == 'post'], prop[pre_post == 'pre']), .groups = 'drop') %>%
  mutate(diff = post - pre)
ukraine_change <- bind_rows(
  occupation_boot %>% group_by(occupy) %>% reframe(boot_test(diff)) %>% mutate(stat = 'diff pp'),
  occupation_boot %>% group_by(occupy) %>% reframe(boot_test(d)) %>% mutate(stat = 'd'))
occupation_wide <- occupation_boot %>% select(occupy, boot, diff) %>%
  pivot_wider(names_from = occupy, values_from = diff)
occupation_contrast <- boot_test(occupation_wide$not_occupied - occupation_wide$occupied) %>%
  mutate(stat = 'not_occupied - occupied change (pp)')

# A separate city-level sensitivity comparison, not a t-test across bootstrap draws.
occupation_city_changes <- ua_cities %>% group_by(city_name, occupy, pre_post) %>%
  summarise(m = mean(prop), .groups = 'drop') %>%
  pivot_wider(names_from = pre_post, values_from = m) %>% mutate(ch = post - pre)
occ_non <- occupation_city_changes$ch[occupation_city_changes$occupy == 'not_occupied']
occ_yes <- occupation_city_changes$ch[occupation_city_changes$occupy == 'occupied']
occupation_city_test <- welch(occ_non, occ_yes) %>%
  mutate(lo = t.test(occ_non, occ_yes)$conf.int[1], hi = t.test(occ_non, occ_yes)$conf.int[2],
         wilcox_p = wilcox.test(occ_non, occ_yes)$p.value)

# Mariupol monthly MEAN CI: first average days within each saved draw, then
# summarise monthly means across draws. Pooling days x draws gives the wrong CI.
# As in s07, use the saved nonzero-language rows; B explicitly records incomplete
# early months (notably November/December). March-May each have all 1,000 draws.
# Even with B = 1,000, some draw-days are absent, so these are the reviewed s07
# conditional monthly means, not zero-filled all-chart-day estimates.
mariupol_month_coverage <- city_boot_output %>% filter(nodeID == 1179) %>%
  mutate(month = format(as.Date(date), '%m')) %>% group_by(month) %>%
  summarise(saved_rows = n(), distinct_dates = n_distinct(date), draws = n_distinct(boot),
            missing_draw_days = distinct_dates * n_boot - saved_rows, .groups = 'drop')
if (any(mariupol_month_coverage$missing_draw_days > 0)) {
  warning('Mariupol s07 monthly means condition on saved nonzero-language rows; missing draw-days remain. Inspect mariupol_month_coverage before treating these as all-chart-day means.')
}
mariupol_month_boot <- city_boot_output %>% filter(nodeID == 1179) %>%
  mutate(month = format(as.Date(date), '%m')) %>% group_by(month, boot) %>%
  summarise(m = mean(prop), .groups = 'drop')
mario <- mariupol_month_boot %>% group_by(month) %>% reframe(boot_test(m))

################################################################################
# RUSSIA — region-defined minority comparison, city-weighted (s07/s12)
################################################################################
# The census classification is at region level, not city ethnicity.
ru_cities <- city_boot_output %>% filter(country_code == 'RU', lyrics_lang == 'ru') %>%
  mutate(city_type = ifelse(city_name %in% minority_cities, 'min', 'notmin'))
ru_agg <- ru_cities %>% group_by(city_type, city_name, pre_post) %>%
  summarise(m = mean(prop), .groups = 'drop') %>%
  pivot_wider(names_from = pre_post, values_from = m) %>% mutate(pre_post_diff = post - pre)
minority_change_boot <- ru_cities %>% group_by(boot, city_type, city_name, pre_post) %>%
  summarise(m = mean(prop), .groups = 'drop') %>%
  pivot_wider(names_from = pre_post, values_from = m) %>% mutate(ch = post - pre) %>%
  group_by(boot, city_type) %>% summarise(ch = mean(ch), .groups = 'drop') %>%
  pivot_wider(names_from = city_type, values_from = ch)
minority_comparison <- bind_rows(
  boot_test(minority_change_boot$min) %>% mutate(stat = 'minority change pp'),
  boot_test(minority_change_boot$notmin) %>% mutate(stat = 'non-minority change pp'),
  boot_test(minority_change_boot$min - minority_change_boot$notmin) %>% mutate(stat = 'minority - non-minority'))
# City-level Welch comparison retained explicitly as a sensitivity analysis.
ru_min <- ru_agg$pre_post_diff[ru_agg$city_type == 'min']
ru_non <- ru_agg$pre_post_diff[ru_agg$city_type == 'notmin']
minority_city_test <- welch(ru_min, ru_non) %>%
  mutate(lo = t.test(ru_min, ru_non)$conf.int[1], hi = t.test(ru_min, ru_non)$conf.int[2],
         wilcox_p = wilcox.test(ru_min, ru_non)$p.value,
         sw_min = shapiro.test(ru_min)$p.value, sw_notmin = shapiro.test(ru_non)$p.value,
         var_F_p = var.test(ru_min, ru_non)$p.value)
ru_cities_decline <- ru_cities %>% group_by(city_name, pre_post, boot) %>%
  summarise(m = mean(prop), .groups = 'drop') %>%
  pivot_wider(names_from = pre_post, values_from = m) %>% mutate(diff = post - pre) %>%
  group_by(city_name) %>% reframe(boot_test(diff)) %>% arrange(est)

print(ukraine_change, width = Inf)
print(occupation_contrast, width = Inf)
print(occupation_city_test, width = Inf)
print(mario, width = Inf)
print(minority_comparison, width = Inf)
print(minority_city_test, width = Inf)
print(ru_cities_decline, n = Inf, width = Inf)
