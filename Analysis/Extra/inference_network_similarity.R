#' Compare the stability of the network globally and within regions pre and post invasion
#' Related to Supplementary Information in paper

# load study-wide functions and global variables
source('utils.R')

################################################################################
# PREPARATION
################################################################################
SIMULATION <- FALSE # if FALSE, pre-computed bootstrap is loaded

# load data
city_meta <- read_rds('Dataset/meta/city_metadata.rds') %>%
  select(ID = nodeID, city_name, country_code, worldbank_region)
pre_edge_untrimmed <- read.csv('Dataset/network/netinf/pre_E10000_B1000_agg.csv')
post_edge_untrimmed <- read.csv('Dataset/network/netinf/post_E10000_B1000_agg.csv')


################################################################################
# PREPARATION
################################################################################
# load edge tables for each bootstrap. This requires you to first run "infer_diffusion_network.R" script
boot_dir <- 'XXX' # directory where each NETINF output is stored

# make a dummy edge list
dim <- city_meta$ID %>% n_distinct()
dummy_edge <- expand.grid(Source = 1:dim, Target = 1:dim)

# add source and target regions and countries
dummy_edge <- dummy_edge %>%
  left_join(city_meta, by = c('Source' = 'ID')) %>%
  rename(source_country = country_code, source_region = worldbank_region) %>%
  left_join(city_meta, by = c('Target' = 'ID')) %>%
  rename(target_country = country_code, target_region = worldbank_region)

# load each bootstrap and fill the matrix
pre_files <- list.files(file.path(boot_dir, 'pre'))
post_files <- list.files(file.path(boot_dir, 'post'))

pre_edge_list <- purrr::map(file.path(boot_dir, 'pre', pre_files), read.csv)
post_edge_list <- purrr::map(file.path(boot_dir, 'post', post_files), read.csv)

# rbind with the list number as a new column
pre_edge <- purrr::map2(pre_edge_list, 1:length(pre_edge_list), ~mutate(.x, boot = .y))
pre_edge <- do.call(rbind, pre_edge)

post_edge <- purrr::map2(post_edge_list, 1:length(post_edge_list), ~mutate(.x, boot = .y))
post_edge <- do.call(rbind, post_edge)

################################################################################
# NETWORK RELIABILITY AS FUNCTION OF N SIMULATIONS (split-half bootstrap correlation)
################################################################################
# n splits
n_splits <- seq(2, 500, 10)

# perform bootstrap correlations
n_boot <- 1
network_split_half <- list()
for (n in n_splits) {
  message(sprintf('Splitting into %s', n))
  
  boot_store = list()
  for (i in 1:n_boot) {
    # do a random split half
    smp <- sample(1:1000, n*2, replace = FALSE)
    half1 <- smp[1:n]
    half2 <- smp[(n+1):(n*2)]
    
    # aggregate the weights of the split subgroups
    pre1 <- pre_edge %>%
      filter(boot %in% half1) %>%
      group_by(Source, Target) %>%
      summarise(pre_weight1 = n(), .groups = 'drop')
    pre2 <- pre_edge %>%
      filter(boot %in% half2) %>%
      group_by(Source, Target) %>%
      summarise(pre_weight2 = n(), .groups = 'drop')
    
    post1 <- post_edge %>%
      filter(boot %in% half1) %>%
      group_by(Source, Target) %>%
      summarise(post_weight1 = n(), .groups = 'drop')
    post2 <- post_edge %>%
      filter(boot %in% half2) %>%
      group_by(Source, Target) %>%
      summarise(post_weight2 = n(), .groups = 'drop')
    
    # fill in the dummy 
    edge_fill <- dummy_edge %>%
      left_join(pre1, by = join_by(Source, Target)) %>%
      left_join(pre2, by = join_by(Source, Target)) %>%
      left_join(post1, by = join_by(Source, Target)) %>%
      left_join(post2, by = join_by(Source, Target))
    
    # fill in zeros
    edge_fill[is.na(edge_fill)] <- 0
    
    # overall correlation
    boot_store[[i]] <- edge_fill %>%
      summarise(pre_r = cor(pre_weight1, pre_weight2),
                post_r = cor(post_weight1, post_weight2)) %>%
      cbind(boot = i)
    print(i)
  }
  network_split_half[[n]] <- do.call(rbind, boot_store)
}

# save the bootstrap outputs
network_split_half_output <- do.call(rbind, network_split_half)
network_split_half_output$n_split <- n_splits
network_split_half_output$n_simulation <- n_splits*2

# plot the split-half correlations over the number of splits
agg_net_split <- network_split_half_output %>%
  group_by(n_simulation) %>%
  summarise(r = mean(c(pre_r, post_r)))

agg_net_split %>%
  ggplot(aes(x = n_simulation, r)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 100, colour = 'red') +
  labs(x = 'N network simulations', y = 'Network split-half correlation')

# save the plot for SI
plot_save('SI/network_split_half_pre', c(100, 80))


################################################################################
# PRE VS. POST BETWEEN SIMILARITY
################################################################################
# perform bootstrap correlations
n_boot <- 1000

if(SIMULATION){
  global <- list()
  within_region <- list()
  within_country <- list()
  for (i in 1:n_boot) {
    # sample half unique
    pre_sample <- pre_edge_list %>% sample(500)
    pre_sample <- do.call(rbind, pre_sample)
    post_sample <- post_edge_list %>% sample(500)
    post_sample <- do.call(rbind, post_sample)
    
    # aggregate weights
    pre_sample <- pre_sample %>%
      group_by(Source, Target) %>%
      summarise(pre_weight = n())
    
    post_sample <- post_sample %>%
      group_by(Source, Target) %>%
      summarise(post_weight = n())
    
    # fill in the dummy 
    edge_fill <- dummy_edge %>%
      left_join(pre_sample) %>%
      left_join(post_sample)
    
    # fill in zeros
    edge_fill[is.na(edge_fill)] <- 0
    
    # overall correlation
    global[[i]] <- edge_fill %>%
      summarise(r = cor(pre_weight, post_weight, method = 'pearson'),
                rho = cor(pre_weight, post_weight, method = 'spearman'),
                sim = calculate_cosine_similarity(pre_weight, post_weight)) %>%
      cbind(boot = i, type = 'global')
    
    # within region
    within_region[[i]] <- edge_fill %>%
      filter(source_region == target_region) %>%
      group_by(source_region) %>%
      summarise(r = cor(pre_weight, post_weight, method = 'pearson'),
                rho = cor(pre_weight, post_weight, method = 'spearman'),
                sim = calculate_cosine_similarity(pre_weight, post_weight),
      ) %>%
      cbind(boot = i, type = 'within_region')
    
    # within country
    within_country[[i]] <- edge_fill %>%
      filter(source_country == target_country) %>%
      group_by(source_country) %>%
      summarise(r = cor(pre_weight, post_weight, method = 'pearson'),
                rho = cor(pre_weight, post_weight, method = 'spearman'),
                sim = calculate_cosine_similarity(pre_weight, post_weight),
      )
    
    print(i)
  }
  
  # save the bootstrap outputs
  global_output <- do.call(rbind, global) %>% cbind(region = 'Global')
  global_output %>% write_rds(sprintf('Bootstraps/global_network_cor_boot%s.rds', n_boot))
  
  within_region_output <- do.call(rbind, within_region)
  within_region_output %>% write_rds(sprintf('Bootstraps/region_network_cor_boot%s.rds', n_boot))
  
  within_country_output <- do.call(rbind, within_country)
  within_country_output %>% write_rds(sprintf('Bootstraps/country_network_cor_boot%s.rds', n_boot))
} else {
  boot_load()
  global_output <- read_rds(sprintf('Bootstraps/global_network_cor_boot%s.rds', n_boot))
  within_region_output <- read_rds(sprintf('Bootstraps/region_network_cor_boot%s.rds', n_boot))
  within_country_output <- read_rds(sprintf('Bootstraps/country_network_cor_boot%s.rds', n_boot))
}

# join the correlations and plot by global, within-region and within-country
joined <- global_output %>% select(region, r, rho, sim) %>% cbind(type = 'global') %>%
  rbind(within_region_output %>% select(region = source_region, r, rho, sim) %>% cbind(type = 'region')) %>%
  rbind(within_country_output %>% select(region = source_country, r, rho, sim) %>% cbind(type = 'country'))

joined_agg <- joined %>%
  na.omit() %>%
  group_by(region, type) %>%
  reframe(get_boot_mean_ci(r, 'pearson'),
          get_boot_mean_ci(rho, 'spearman'),
          get_boot_mean_ci(sim, 'cosine')
          )

# use pearson for plotting. put them in right order
joined_agg <- joined_agg %>%
  arrange(pearson_m) %>%
  group_by(type) %>%
  mutate(order = 1:n()) %>%
  ungroup() %>%
  mutate(order = ifelse(region == 'Global', 0, order))

# report global stats in the text
joined_agg %>%
  filter(region == 'Global') %>%
  select(type, pearson_m, pearson_lower_ci, pearson_upper_ci, order)


################################################################################
# CORRELATION MATRIX
################################################################################
## WITHIN REGION COMPARISON
# make a new reordered dictionary
reorder_dict <- city_meta %>%
  arrange(worldbank_region, country_code, ID) %>%
  mutate(reorderID = 1:n() %>% as.factor()) %>% 
  left_join(WORLDBANK_REGION_COLOUR)

get_matrix_pre_post <- function(pre_post, type = c('postsoviet', 'within-region', 'global'), fill_scale = c(0, 14)){
  graph <- switch(pre_post,
                  pre = graph_from_data_frame(pre_edge_untrimmed %>% mutate(Weight = log(Weight)), directed = FALSE),
                  post = graph_from_data_frame(post_edge_untrimmed %>% mutate(Weight = log(Weight)), directed = FALSE)
                  )
  
  # make matrix using weights
  adj_mat <- as.matrix(get.adjacency(graph, attr="Weight"))
  
  # convert to data frame
  df_mat <- as.data.frame(as.table(adj_mat))
  colnames(df_mat) <- c("from", "to", "weight")
  df_mat <- df_mat %>%
    mutate(across(from:to, as.numeric))
  
  # join with reorderID
  joined <- df_mat %>%
    left_join(reorder_dict %>% select(reorderID, ID, worldbank_region), by = c('from' = 'ID')) %>%
    rename(x = reorderID, from_region = worldbank_region) %>%
    left_join(reorder_dict %>% select(reorderID, ID, worldbank_region), by = c('to' = 'ID')) %>%
    rename(y = reorderID, to_region = worldbank_region) %>%
    mutate(within_region = ifelse(from_region == to_region, from_region, NA))
  
  # plot 
  plot_df <- switch(type,
                    postsoviet = joined %>% filter(within_region == "Post-Soviet"),
                    'within-region' = joined %>% filter(!is.na(within_region)),
                    global = joined
                    )
  
  plot_output <- plot_df %>%
    ggplot(aes(
      x = x,
      y = y,
      fill = weight
    )) +
    geom_tile() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = switch(type,
                          postsoviet = element_blank(),
                          within = element_blank(),
                          global = element_line(size = 0.1, color = reorder_dict$region_colour)
                          ),
      legend.position = 'bottom'
    ) +
    scale_fill_viridis(limits = fill_scale, guide = FALSE) +
    labs(title = sprintf('%s %s', pre_post, type), fill = "Log weight")
  
  switch(type,
         postsoviet = plot_output,
         'within-region' = plot_output + facet_wrap(~ within_region, scale = 'free'),
         global = plot_output
         )
}

# plot global pre and post matrix
pre_global <- get_matrix_pre_post('pre', 'global')
post_global <- get_matrix_pre_post('post', 'global')

# plot pre and post within community connections
pre_within <- get_matrix_pre_post('pre', 'within-region')
post_within <- get_matrix_pre_post('post', 'within-region')

# bind all the plots
(bind_plot <- (pre_global + post_global)/(pre_within + post_within) + plot_layout(guides = "collect") & theme(legend.position = 'bottom'))

plot_save('SI/pre_post_network_matrix', c(300, 300), pdf = FALSE)
