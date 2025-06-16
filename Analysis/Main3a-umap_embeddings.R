#' Perform dimensionality reduction using UMAP and compare the distribution density pre vs. post-invasion 
#' Lyrical features vectors are extracted from LLM (see Analysis/Extra/extraction_word_embedding.R script)
#' Acoustic features are extracted using Essentia (https://essentia.upf.edu)
#' Related to Fig.3 in paper

# load study-wide functions and global variables
source('utils.R')
require(uwot) # for UMAP
require(philentropy) # for JSD computation

################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load the dataset
song_meta <- read_rds('Dataset/meta/song_metadata.rds') 
lyrics_embedding <- read_rds('Dataset/lyrics/UA_RU_lyrics_embedding_gpt.rds') %>% rename(trackID = track_id)

# computes the KDE estimation in 2D space
get_kde <- function(df, lyrics_lang = c('uk', 'ru'), type = c('pre', 'post'), h = 1, nbin = 512){
  # filter the dataset
  df <- df %>%
    filter(pre_post == type,
           lyrics_language == lyrics_lang)
  
  # compute KDE estimates
  kde <- MASS::kde2d(
    df$x,
    df$y,
    h = c(h, h),
    n = nbin,
    lims = c(x_range, y_range)
  )
  
  # make all combinations of x and y
  comb <- expand.grid(kde$x, kde$y)
  dens <- kde$z %>% as.vector()
  
  # normalize density to sum to 1
  norm_dens <- dens / sum(dens)
  
  # output
  tibble(x = comb$Var1,
         y = comb$Var2,
         density = norm_dens,
         pre_post = type)
  
}

# plots the KDE overlaying on the umap with all points
plot_umap_density <- function(general_umap, country_kde, country_code, labels = FALSE){
  ggplot(country_kde) +
    # plot the KDE estimate
    geom_tile(aes(x, y, fill = density)) +
    # fill colours depending on the country
    scale_fill_gradient(low = 'white',
                        high = switch(country_code, RU = RUSSIAN_COLOUR, UA = UKRAINE_COLOUR),
                        guide = 'none') +
    # overlay the points of all songs
    geom_point(
      data = general_umap %>% select(x, y),
      aes(x, y),
      colour = 'black',
      size = 0.5,
      alpha = 0.2
    ) +
    # set theme
    labs(y = 'UMAP2', x = 'UMAP1') +
    facet_wrap( ~ pre_post) +
   ( if(labels == FALSE){
      theme_void() +
      theme(strip.text = element_blank())
    }) +
    scale_x_continuous(limits = x_range) +
    scale_y_continuous(limits = y_range)
    
}

# bootstrap the songs in KDE with replacement
boot_kde <- function(umap_df, lyrics_lang = c('uk', 'ru'), country_code = c('UA', 'RU'), nboot = 1000){
  message(sprintf('beginning %s bootstraps for %s', nboot, country_code))
  store = list()
  for (i in 1:nboot) {
    boot_songs <- umap_df %>%
      filter(lyrics_language == lyrics_lang) %>%
      group_by(pre_post) %>%
      sample_n(size = n(), replace = TRUE) %>%
      ungroup()
    
    # compute KDE separately for pre and post
    pre_boot <- get_kde(boot_songs, lyrics_lang, 'pre')
    post_boot <- get_kde(boot_songs, lyrics_lang, 'post')
    
    # get the JSD distance between the two KDE spaces
    jsd_dist <- philentropy::JSD(rbind(pre_boot$density, post_boot$density))
    
    # make a random baseline by shuffling the pre and post labels
    boot_songs_shuffle <- boot_songs %>%
      mutate(pre_post = sample(pre_post, n(), replace = TRUE))
    
    # make a baseline with shuffled labels
    pre_shuffled <- get_kde(boot_songs_shuffle, lyrics_lang, 'pre')
    post_shuffled <- get_kde(boot_songs_shuffle, lyrics_lang, 'post')
    jsd_dist_baseline <- philentropy::JSD(rbind(pre_shuffled$density, post_shuffled$density))
    
    # calculate actual JSD - baseline as effect size
    estimate <- jsd_dist - jsd_dist_baseline
    
    # store the boot result
    store[[i]] <- tibble(boot = i, country_code, jsd_dist, jsd_dist_baseline, estimate)
    print(i)
  }
  do.call(rbind, store)
}


################################################################################
# LYRICS UMAP
################################################################################
# add meta information about the song and unpack the embedding
lyrics_embedding <- lyrics_embedding %>% left_join(song_meta %>% select(trackID, pre_post, lyrics_language))

# unpack the embedding vectors into columns
lyrics_embedding <- lyrics_embedding %>% 
  group_by(trackID, country_code) %>%
  slice(1) %>%
  unnest(embedding) %>%
  mutate(index = paste0('d', 1:n())) %>% # make embedding index
  pivot_wider(names_from = index, values_from = embedding) %>%
  ungroup()

# separate feature set and metadata
lyrics_feat <- lyrics_embedding %>% select(-trackID, -pre_post, -lyrics_language, - country_code)
lyrics_meta <- lyrics_embedding %>% select(trackID, pre_post, lyrics_language, country_code)
lyrics_meta$lyrics_language_category <- ifelse(lyrics_meta$lyrics_language %in% c('uk', 'ru'), lyrics_meta$lyrics_language, 'Other')

# compute umap
umap_lyrics <- lyrics_feat %>% umap(n_neighbors = 10, n_components = 2, min_dist = 0.1, metric = 'cosine')
umap_lyrics <- cbind.data.frame(setNames(as.data.frame(umap_lyrics), c("x", "y")), lyrics_meta)
umap_lyrics$pre_post <- factor(umap_lyrics$pre_post, levels = c('pre', 'post'))

# define the general range of umap space as a grid
x_range <- umap_lyrics$x %>% range() %>% round()
x_range <- c(x_range[1] - 1, x_range[2] + 1)
y_range <- umap_lyrics$y %>% range() %>% round()
y_range <- c(y_range[1] - 1, y_range[2] + 1)

# for each country, see the KDE distribution of the pre and post songs
ua_pre_lyrics <- get_kde(umap_lyrics, 'uk', 'pre')
ua_post_lyrics <- get_kde(umap_lyrics, 'uk', 'post')
ua_lyrics_kde <- rbind(ua_pre_lyrics, ua_post_lyrics) %>% 
  mutate(pre_post = factor(pre_post, levels = c('pre', 'post')))

ru_pre_lyrics <- get_kde(umap_lyrics, 'ru', 'pre')
ru_post_lyrics <- get_kde(umap_lyrics, 'ru', 'post')
ru_lyrics_kde <- rbind(ru_pre_lyrics, ru_post_lyrics) %>% 
  mutate(pre_post = factor(pre_post, levels = c('pre', 'post')))

# plot the two umaps with KDE overlayed
plot_umap_density(umap_lyrics, ua_lyrics_kde, 'UA')
plot_save('Main/umap_ukrainian_lyrics', c(150, 80))

plot_umap_density(umap_lyrics, ru_lyrics_kde, 'RU')
plot_save('Main/umap_russian_lyrics', c(150, 80))


## JSD DISTANCE
# compute the KDE differences
n_boot <- 1000

if(SIMULATION){
  ua_lyrics_kde_boot <- boot_kde(umap_lyrics, 'uk', 'UA', n_boot)
  ru_lyrics_kde_boot <- boot_kde(umap_lyrics, 'ru', 'RU', n_boot)
  bind_lyrics_kde_boot <- rbind(ua_lyrics_kde_boot, ru_lyrics_kde_boot)
  
  # save bootstrapped data
  bind_lyrics_kde_boot %>% write_rds(sprintf('Bootstraps/umap_lyrics_kde_jsd_boot%s.rds', n_boot))
} else {
  boot_load()
  bind_lyrics_kde_boot <- read_rds(sprintf('Bootstraps/umap_lyrics_kde_jsd_boot%s.rds', n_boot))
}

# aggregate and compare the two distribution difference
kde_lyrics_boot_agg <- bind_lyrics_kde_boot %>%
  group_by(country_code) %>%
  reframe(get_boot_mean_ci(estimate, 'boot')) %>%
  mutate(country_code = factor(country_code, levels = c('UA', 'RU')))

# report JSD stats
kde_lyrics_boot_agg

# get group level difference
bind_lyrics_kde_boot %>%
  select(country_code, estimate) %>%
  pivot_wider(names_from = country_code, values_from = estimate) %>%
  reframe(get_t_stat(UA %>% unlist(), RU %>% unlist()))


################################################################################
# ACOUSTIC
################################################################################
# get all acoustic features for the songs
acoustic_embedding <- song_meta %>%
  filter(!is.na(lyrics_language), !is.na(mfcc1), country_code %in% c('RU', 'UA')) %>%
  select(trackID, pre_post, lyrics_language, loudness:mfcc12) %>%
  group_by(trackID) %>%
  slice(1) %>%
  ungroup()

# normalize the acoustic features
acoustic_embedding <- acoustic_embedding %>%
  mutate(across(loudness:mfcc12, scale))

# separate feature set and metadata
acoustic_feat <- acoustic_embedding %>% select(-trackID, -pre_post, -lyrics_language)
acoustic_meta <- acoustic_embedding %>% select(trackID, pre_post, lyrics_language)
acoustic_meta$lyrics_language_category <- ifelse(acoustic_meta$lyrics_language %in% c('uk', 'ru'), acoustic_meta$lyrics_language, 'Other')

# compute umap
umap_acoustic <- acoustic_feat %>% umap(n_neighbors = 10, n_components = 2, min_dist = 0.1, metric = 'cosine')
umap_acoustic <- cbind.data.frame(setNames(as.data.frame(umap_acoustic), c("x", "y")), acoustic_meta)
umap_acoustic$pre_post <- factor(umap_acoustic$pre_post, levels = c('pre', 'post'))

# define the general range of umap space as a grid
x_range <- umap_acoustic$x %>% range() %>% round()
x_range <- c(x_range[1] - 1, x_range[2] + 1)
y_range <- umap_acoustic$y %>% range() %>% round()
y_range <- c(y_range[1] - 1, y_range[2] + 1)

# for each country, see the KDE distribution of the pre and post songs
ua_pre_acoustic <- get_kde(umap_acoustic, 'uk', 'pre', h = 2)
ua_post_acoustic <- get_kde(umap_acoustic, 'uk', 'post', h = 2)
ua_acoustic_kde <- rbind(ua_pre_acoustic, ua_post_acoustic) %>% 
  mutate(pre_post = factor(pre_post, levels = c('pre', 'post')))

ru_pre_acoustic <- get_kde(umap_acoustic, 'ru', 'pre', h = 2)
ru_post_acoustic <- get_kde(umap_acoustic, 'ru', 'post', h = 2)
ru_acoustic_kde <- rbind(ru_pre_acoustic, ru_post_acoustic) %>% 
  mutate(pre_post = factor(pre_post, levels = c('pre', 'post')))

# plot the two umaps with KDE
plot_umap_density(umap_acoustic, ua_acoustic_kde, 'UA')
plot_save('SI/umap_ukrainian_acoustic', c(150, 80))

plot_umap_density(umap_acoustic, ru_acoustic_kde, 'RU')
plot_save('SI/umap_russian_acoustic', c(150, 80))


# compute the KDE differences
n_boot <- 1000

if(SIMULATION){
  ua_acoustic_kde_boot <- boot_kde(umap_acoustic, 'uk', 'UA', n_boot)
  ru_acoustic_kde_boot <- boot_kde(umap_acoustic, 'ru', 'RU', n_boot)
  bind_acoustic_kde_boot <- rbind(ua_acoustic_kde_boot, ru_acoustic_kde_boot)
  
  # save bootstrapped data
  bind_acoustic_kde_boot %>% write_rds(sprintf('Bootstraps/umap_acoustic_kde_jsd_boot%s.rds', n_boot))
} else {
  boot_load()
  bind_acoustic_kde_boot <- read_rds(sprintf('Bootstraps/umap_acoustic_kde_jsd_boot%s.rds', n_boot))
}

# aggregate and compare the two distribution difference
kde_acoustic_boot_agg <- bind_acoustic_kde_boot %>%
  group_by(country_code) %>%
  reframe(get_boot_mean_ci(estimate, 'boot')) %>%
  mutate(country_code = factor(country_code, levels = c('UA', 'RU')))

# report the statistics
kde_acoustic_boot_agg

# get group level difference
bind_acoustic_kde_boot %>%
  select(country_code, estimate) %>%
  pivot_wider(names_from = country_code, values_from = estimate) %>%
  reframe(get_t_stat(UA %>% unlist(), RU %>% unlist()))

