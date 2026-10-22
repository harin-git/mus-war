#' Perform dimensionality reduction using UMAP and compare the distribution density pre vs. post-invasion 
#' Lyrical features vectors are extracted from LLM (see Analysis/Extra/extraction_word_embedding.R script)
#' Acoustic features are extracted using Essentia (https://essentia.upf.edu)
#' Related to Fig.3 and Supplementary Fig. 4 in paper

# load study-wide functions and global variables
source('utils.R')
require(uwot) # for UMAP
require(philentropy) # for JSD computation
require(parallel) # for running bootstraps in parallel

################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded
UMAP_SEED <- 42      # UMAP is stochastic: fixed seed, single thread
KDE_H <- 1           # kernel width of the 2D KDE, for both lyrics and acoustics
n_boot <- 1000

# load the dataset
song_meta <- read_rds('Dataset/meta/song_metadata.rds') 
lyrics_embedding <- read_rds('Dataset/lyrics/UA_RU_lyrics_embedding_gpt.rds') %>% rename(trackID = track_id)

# grid limits of a UMAP layout (rounded range plus a margin of 1)
umap_limits <- function(umap_df) c(range(round(umap_df$x)) + c(-1, 1), range(round(umap_df$y)) + c(-1, 1))

# computes the KDE estimation in 2D space, normalised to sum to 1
get_kde <- function(df, lyrics_lang = c('uk', 'ru'), type = c('pre', 'post'), lims, h = KDE_H, nbin = 512){
  df <- df %>% filter(pre_post == type, lyrics_language == lyrics_lang)
  kde <- MASS::kde2d(df$x, df$y, h = c(h, h), n = nbin, lims = lims)
  comb <- expand.grid(kde$x, kde$y)
  dens <- kde$z %>% as.vector()
  tibble(x = comb$Var1, y = comb$Var2, density = dens / sum(dens), pre_post = type)
}

# JSD between the pre- and post-invasion KDEs of one language
get_jsd <- function(df, lyrics_lang, lims, h = KDE_H){
  philentropy::JSD(rbind(get_kde(df, lyrics_lang, 'pre', lims, h)$density,
                         get_kde(df, lyrics_lang, 'post', lims, h)$density))
}

# bootstrap the songs (with replacement, within pre and post) and compare the JSD with a shuffled-label baseline.
# Each replicate has its own seed, so the result does not depend on the number of cores.
boot_kde <- function(umap_df, nboot = n_boot, seed = UMAP_SEED, h = KDE_H){
  lims <- umap_limits(umap_df)
  umap_df <- umap_df %>% mutate(pre_post = as.character(pre_post))
  mclapply(seq_len(nboot), function(i) {
    set.seed(seed * 100000 + i)
    map_dfr(c('uk', 'ru'), function(lang) {
      boot_songs <- umap_df %>% filter(lyrics_language == lang) %>%
        group_by(pre_post) %>% slice_sample(prop = 1, replace = TRUE) %>% ungroup()
      boot_songs_shuffle <- boot_songs %>% mutate(pre_post = sample(pre_post, n(), replace = TRUE))
      jsd_dist <- get_jsd(boot_songs, lang, lims, h)
      jsd_dist_baseline <- get_jsd(boot_songs_shuffle, lang, lims, h)
      tibble(boot = i, country_code = c(uk = 'UA', ru = 'RU')[[lang]], jsd_dist, jsd_dist_baseline,
             estimate = jsd_dist - jsd_dist_baseline)
    })
  }, mc.cores = max(1, detectCores() - 1)) %>% bind_rows()
}

# report the JSD minus baseline per language, and the Ukrainian minus Russian difference (paired by replicate)
report_jsd <- function(boot_df){
  boot_df %>% group_by(country_code) %>% reframe(get_boot_mean_ci(estimate, 'jsd')) %>% print(width = Inf)
  boot_df %>% select(boot, country_code, estimate) %>%
    pivot_wider(names_from = country_code, values_from = estimate) %>%
    reframe(get_boot_mean_ci(UA - RU, 'jsd_diff')) %>% print(width = Inf)
}


################################################################################
# LYRICS UMAP (Fig. 3a)
################################################################################
# add meta information about the song and unpack the embedding (one row per song and country)
lyrics_embedding <- lyrics_embedding %>%
  left_join(song_meta %>% select(trackID, pre_post, lyrics_language), by = 'trackID') %>%
  group_by(trackID, country_code) %>%
  slice(1) %>%
  ungroup()
lyrics_feat <- do.call(rbind, lyrics_embedding$embedding)

# compute umap
set.seed(UMAP_SEED)
um <- umap(lyrics_feat, n_neighbors = 10, n_components = 2, min_dist = 0.1, metric = 'cosine', n_threads = 1, seed = UMAP_SEED)
umap_lyrics <- tibble(trackID = lyrics_embedding$trackID, country_code = lyrics_embedding$country_code,
                      lyrics_language = lyrics_embedding$lyrics_language,
                      pre_post = factor(lyrics_embedding$pre_post, c('pre', 'post')), x = um[,1], y = um[,2])
lyrics_lims <- umap_limits(umap_lyrics)

# songs per language and period
umap_lyrics %>% filter(lyrics_language %in% c('uk', 'ru')) %>% count(lyrics_language, pre_post)

# UMAP of all songs with the KDE of local songs pre and post (KDE drawn at print resolution)
umap_panel <- function(umap_df, lims, lang, col){
  one <- function(pp, title) {
    k <- get_kde(umap_df, lang, pp, lims, nbin = raster_n(HALF_W / 2))
    ggplot() +
      raster_layer(k, 'x', 'y', 'density', 'white', col, limits = c(0, max(k$density))) +
      geom_point(data = umap_df, aes(x, y), size = 0.2, alpha = 0.25, shape = 16) +
      coord_fixed(xlim = lims[1:2], ylim = lims[3:4], expand = FALSE) +
      labs(x = 'UMAP 1', y = 'UMAP 2', title = title) +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))
  }
  one('pre', 'Pre-invasion') + (one('post', 'Post-invasion') + theme(axis.title.y = element_blank()))
}
lyrics_source <- umap_lyrics %>% select(trackID, country_code, lyrics_language, pre_post, umap1 = x, umap2 = y)
plot_save('Main/umap_ukrainian_lyrics', c(HALF_W, 40), lyrics_source, umap_panel(umap_lyrics, lyrics_lims, 'uk', UKRAINE_COLOUR), source_name = 'Fig_3a_Ukraine')
plot_save('Main/umap_russian_lyrics', c(HALF_W, 40), lyrics_source, umap_panel(umap_lyrics, lyrics_lims, 'ru', RUSSIAN_COLOUR), source_name = 'Fig_3a_Russia')

## JSD DISTANCE
if(SIMULATION){
  bind_lyrics_kde_boot <- boot_kde(umap_lyrics)
  bind_lyrics_kde_boot %>% write_rds(sprintf('Bootstraps/umap_lyrics_kde_jsd_boot%s.rds', n_boot))
} else {
  boot_load()
  bind_lyrics_kde_boot <- read_rds(sprintf('Bootstraps/umap_lyrics_kde_jsd_boot%s.rds', n_boot))
}

# report JSD stats (bootstrap mean, 95% CI, z, P) and the Ukrainian minus Russian difference
report_jsd(bind_lyrics_kde_boot)


################################################################################
# ACOUSTIC UMAP (Supplementary Fig. 4)
################################################################################
# acoustic features of songs discovered in Ukraine and Russia (one row per song), z-scored
acoustic_embedding <- song_meta %>%
  filter(!is.na(lyrics_language), !is.na(mfcc1), country_code %in% c('RU', 'UA')) %>%
  select(trackID, pre_post, lyrics_language, loudness:mfcc12) %>%
  group_by(trackID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(across(loudness:mfcc12, ~ as.numeric(scale(.x))))
acoustic_feat <- acoustic_embedding %>% select(loudness:mfcc12) %>% as.matrix()

# compute umap
set.seed(UMAP_SEED)
um <- umap(acoustic_feat, n_neighbors = 10, n_components = 2, min_dist = 0.1, metric = 'cosine', n_threads = 1, seed = UMAP_SEED)
umap_acoustic <- tibble(trackID = acoustic_embedding$trackID, lyrics_language = acoustic_embedding$lyrics_language,
                        pre_post = acoustic_embedding$pre_post, x = um[,1], y = um[,2])
acoustic_lims <- umap_limits(umap_acoustic)

# songs per language and period
umap_acoustic %>% filter(lyrics_language %in% c('uk', 'ru')) %>% count(lyrics_language, pre_post)

# plots the KDE overlaying on the umap with all points
plot_umap_density <- function(general_umap, country_kde, country_code, lims){
  ggplot(country_kde) +
    geom_tile(aes(x, y, fill = density)) +
    scale_fill_gradient(low = 'white', high = switch(country_code, RU = RUSSIAN_COLOUR, UA = UKRAINE_COLOUR), guide = 'none') +
    geom_point(data = general_umap %>% select(x, y), aes(x, y), colour = 'black', size = 0.5, alpha = 0.2) +
    labs(y = 'UMAP2', x = 'UMAP1') +
    facet_wrap(~ pre_post) +
    theme_void() +
    theme(strip.text = element_blank()) +
    scale_x_continuous(limits = lims[1:2]) +
    scale_y_continuous(limits = lims[3:4])
}
acoustic_kde <- function(lang) bind_rows(get_kde(umap_acoustic, lang, 'pre', acoustic_lims),
                                         get_kde(umap_acoustic, lang, 'post', acoustic_lims)) %>%
  mutate(pre_post = factor(pre_post, levels = c('pre', 'post')))
acoustic_source <- umap_acoustic %>% select(trackID, lyrics_language, pre_post, umap1 = x, umap2 = y)
plot_save('SI/umap_ukrainian_acoustic', c(150, 80), acoustic_source,
          plot_umap_density(umap_acoustic, acoustic_kde('uk'), 'UA', acoustic_lims), source_name = 'Supplementary_Fig_4_Ukraine')
plot_save('SI/umap_russian_acoustic', c(150, 80), acoustic_source,
          plot_umap_density(umap_acoustic, acoustic_kde('ru'), 'RU', acoustic_lims), source_name = 'Supplementary_Fig_4_Russia')

## JSD DISTANCE
if(SIMULATION){
  bind_acoustic_kde_boot <- boot_kde(umap_acoustic)
  bind_acoustic_kde_boot %>% write_rds(sprintf('Bootstraps/umap_acoustic_kde_jsd_boot%s.rds', n_boot))
  # sensitivity to the kernel width (Methods): h = 2 on the same layout
  report_jsd(boot_kde(umap_acoustic, h = 2))
} else {
  boot_load()
  bind_acoustic_kde_boot <- read_rds(sprintf('Bootstraps/umap_acoustic_kde_jsd_boot%s.rds', n_boot))
}

# report JSD stats and the Ukrainian minus Russian difference
report_jsd(bind_acoustic_kde_boot)
