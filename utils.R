#' All functions and highlevel objects are defined here and loaded with source('utils.R')
#' across all the scripts in the repo for analyses

################################################################################
# ALWAYS ON-DEMAND PACKAGES
################################################################################
# data wrangling and statistics
library(tidyverse)
library(gam) 

# network graphs
library(igraph)

# plotting
library(patchwork)
library(viridis)


################################################################################
# DOWNLOAD DATA
################################################################################
# helper function to fetch data from OSF
download_data <- function(bootstrapped_data = FALSE){
  project <- osf_retrieve_node("ra38k")
  osf_files <- osf_ls_files(project)
  
  # if Bootstraps or Dataset folder already exists, stop and make error
  current_dir <- list.files(getwd())
  if('Bootstraps' %in% current_dir | 'Dataset' %in% current_dir){
    stop('Either Dataset or Bootstraps folder already exists in the current directory. Please remove them before downloading again.')
  } else {
    if(bootstrapped_data){
      filtered_list <- osf_files
    } else {
      filtered_list <- subset(osf_files, name == 'Dataset')
    }
  }
  
  # filter the type of data 
  message('Download starting')
  download_path <- osf_download(filtered_list, progress = TRUE)
  
  # verify the file was downloaded locally
  if(file.exists(download_path$local_path)){message('Success!')}
}

# load bootstrap files
boot_load <- function(){
  if(!file.exists('Bootstraps')){
    stop('Bootstraps folder does not exist. Please download the data first using download_data.R script or
         manually through the OSF project page https://osf.io/ra38k/')
  }
  
  message('Loading pre-computed bootstrap...')
}



################################################################################
# THEMES & AESTHETICS
################################################################################
# Themes setup
options(
  # set default colors in ggplot2 to colorblind-friendly 
  # Okabe-Ito and Viridis palettes
  ggplot2.discrete.colour = ggokabeito::palette_okabe_ito(),
  ggplot2.discrete.fill = ggokabeito::palette_okabe_ito(),
  ggplot2.continuous.colour = 'viridis',
  ggplot2.continuous.fill =  'viridis',
  # set theme font and size
  book.base_family = "sans",
  book.base_size = 10
)

# set default theme
theme_set(
  theme_classic(
    base_size = getOption("book.base_size"), 
    base_family = getOption("book.base_family")
  ) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
)


theme_map <- function(...) {
  background_col <- 'white'
  # background_col <- "#f5f5f2"
  
  theme_minimal() +
    theme(
      # text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # add a subtle grid
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = background_col, color = NA), 
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.background = element_rect(fill = background_col, color = NA), 
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.background = element_rect(fill = background_col, color = NA),
      legend.title = element_text(size = 13),
      legend.position = 'top',
      legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      plot.caption = element_text(size = 9, 
                                  hjust = .5, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184"),
    )
}

################################################################################
# GLOBAL VARIABLES
################################################################################
WAR_START <- as.Date('2022-02-24')
SUB_COUNTRIES <- c('UA', 'RU', 'BY', 'KZ')

UKRAINE_COLOUR <- '#0057b7'
UKRAINE_COLOUR2 <- '#edc40c'

RUSSIAN_COLOUR <- '#E4181C'
RUSSIAN_COLOUR2 <- '#1C3578'

BELARUS_COLOUR <- '#00AF66'
KAZAKHSTAN_COLOUR <- '#00AFCA'
ENGLISH_COLOUR <- 'orange'

WORLDBANK_REGION_COLOUR <- tibble(
  worldbank_region = c(
    'Europe',
    'North America',
    'East Asia & Pacific',
    'Latin America & Caribbean',
    'Post-Soviet',
    'South Asia',
    'Middle East & North Africa',
    'Sub-Saharan Africa'
  ),
  region_colour = c(
    "#56B4E9",
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#FFA500",
    "#FF5768",
    "#CC79A7",
    "#F0E442"
  )
)

################################################################################
# STATISTICS
################################################################################
# for comparing groups and reporitng t-stat and effect size
get_t_stat <- function(group1, group2){
  t_stat <- t.test(group1, group2, na.rm = TRUE)  
  
  n1 <- sum(!is.na(group1))
  n2 <- sum(!is.na(group2))
  
  p <- t_stat$p.value
  t <- t_stat$statistic
  
  lower_ci <- t_stat$conf.int[[1]]
  upper_ci <- t_stat$conf.int[[2]]
  
  mean_diff <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
  
  pooled_sd <- sqrt(((n1 - 1) * sd(group1, na.rm = TRUE)^2 + (n2 - 1) * sd(group2, na.rm = TRUE)^2) / (n1 + n2 - 2))
  cohensD <- mean_diff / pooled_sd
  
  tibble(t, p, mean_diff, lower_ci, upper_ci, pooled_sd, cohensD)
}

# for bootstrap confidence estimates
get_boot_mean_ci <- function(x, var_name, obs_stat = 0, alpha = 0.05, ...){
  low <- alpha / 2
  high <- 1 - alpha / 2

  # parametric p-value
  z_score <- (mean(x) - obs_stat) / sd(x)
  p <- signif(2 * pnorm(-abs(z_score)), 3)  # two-tailed
  
  # make t-stat labels
  p_labels <- ifelse(p >= 0.05, 'ns', ifelse(p < 0.001, '***', ifelse(p < 0.01, '**', '*')))
  
  output <- tibble(m = mean(x),
         lower_ci = quantile(x, low),
         upper_ci = quantile(x, high),
         p,
         p_labels
         )
  colnames(output) <- paste(var_name, colnames(output), sep = '_')
  return(output)
}

# for standard error
SE <- function(x){
  n <- length(x)
  standard_deviation <- sd(x)
  standard_error <- standard_deviation / sqrt(n)
}

# for proportion equal to 1
get_proportion <- function(x) {
  t <- table(x) 
  tibble(name = names(t), value = t %>% as.numeric()) %>%
    mutate(prop = t / sum(t))
}

# for margin of error
CI <- function(x, alpha = 0.05){
  mean_value <- mean(x)
  
  # Compute the size
  n <- length(x)
  
  # Find the standard deviation
  standard_deviation <- sd(x)
  
  # Find the standard error
  standard_error <- standard_deviation / sqrt(n)
  degrees_of_freedom = n - 1
  t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
  margin_error <- t_score * standard_error
  return(margin_error)
}

# for 95% CI
calculate_95_ci <- function(mean, sd, n) {
  t_star <- qt(0.975, df = n - 1)  # t-value for 95% CI (two-tailed)
  ci_lower <- mean - t_star * (sd / sqrt(n))
  ci_upper <- mean + t_star * (sd / sqrt(n))
  return(list(ci_lower = ci_lower, ci_upper = ci_upper))
}

# for eculidean distances
euclidean <- function(a, b){sqrt(sum((a - b)^2))}

# for normalizing between 0 and 1
normalize_zero_one <- function(x, na.rm = TRUE){
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

# for normalizing sum to one
normalize_sum_to_one <- function(x, na.rm = TRUE){
  x / sum(x, na.rm = na.rm)
}

# for fitting GAM to bootstrapped data
gam_fit <- function(x, y, n_spline = 2){
  boot_data <- data.frame(x = x, y = y)
  output_data <- data.frame(x = x)
  gam_model <- gam::gam(y ~ s(x, 2), data = boot_data)
  
  output_data$y <- predict(gam_model, output_data, type = 'response')
  output_data
}

# for cosine similarity between two vectors
calculate_cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# for finding closest points between two locations
find_closest_point <- function(lon, lat, dataset) {
  d <- geosphere::distVincentySphere(c(lon, lat), cbind(dataset$lon, dataset$lat))
  closest_index <- which.min(d)
  city_name <- dataset[closest_index, ]$city_name
  nodeID <- dataset[closest_index, ]$nodeID
  return(tibble(nodeID, city_name, distance = min(d)))
}


################################################################################
# NETWORK
################################################################################
# for creating node dictionary file to insert into NETINF
make_dummy_dictionary <- function(df, save_path){
  nodes <- df$nodeID %>% unique() %>% sort()
  node_txt <- c(paste0(nodes, ',', nodes), '')
  
  # combine with the computed cascade
  netinf_txt <- c(node_txt)
  
  # save the file
  write.table(netinf_txt, save_path, sep = "\n",
              row.names = FALSE, col.names = FALSE, quote= FALSE)
  
  message(sprintf('Dummy dictionary created in %s', save_path))
}

# for creating song cascade file to insert into NETINF
get_song_cascade <- function(df, save_path){
  arrange_by_date <- df %>%
    group_by(track_id, nodeID) %>%
    filter(date == min(date)) %>% # consider only first appearance
    group_by(track_id) %>%
    mutate(day_since = as.numeric(as.Date(date) - min(as.Date(date))) + 1) %>% # calculate day since it first appeared
    arrange(track_id, nodeID) %>%
    mutate(node_day = paste(nodeID, day_since, sep = ',')) %>% # make into nodeID, day format
    summarise(cascade = paste(node_day, collapse = ','))
  
  # make node to node table for netinf input
  nodes <- df$nodeID %>% unique() %>% sort()
  node_txt <- c(paste0(nodes, ',', nodes), '')
  
  # combine with the computed cascade
  netinf_txt <- c(node_txt, arrange_by_date$cascade)
  
  # save the file
  write.table(netinf_txt, save_path, sep = "\n",
              row.names = FALSE, col.names = FALSE, quote= FALSE)
}


# for running the NETINF algorithm
run_netinf <- function(netinf_folder_path, 
                       input_txt_path, 
                       groundtruth_txt_path,
                       output_path,
                       E = 10000, # number of edges
                       S = 1, 
                       A = 1){ # exponential alpha value
  # Parameters for NETINF:
  #   
  # -i:Input cascades (one file) (default:'example-cascades.txt')
  # -n:Input ground-truth network (one file) (default:'example-network.txt')
  # -o:Output file name(s) prefix (default:'network')
  # -e:Number of iterations (default:'5')
  # -a:Alpha for exponential model (default:1)
  # -s:How much additional files to create?
  #   1:info about each edge, 2:objective function value (+upper bound), 3:Precision-recall plot, 4:all-additional-files (default:1)
  # (default:1)
  
  # run NETINF
  cmd <- sprintf('cd "%s";
                ./netinf -i:"%s" -n:"%s" -o:%s -e:%s -s:%s -a:%s',
                 netinf_folder_path, 
                 input_txt_path, 
                 groundtruth_txt_path, 
                 output_path %>% str_remove('.txt'),
                 E, S, A)
  system(cmd)
  
  # open txt file, edit, and save as gephi format
  netinf_txt <- read.delim(output_path, sep = '\n')
  reshape <- reshape2::colsplit(netinf_txt[, 1], ',', c('Source', 'Target'))
  edge_start <- which(reshape$Source != reshape$Target) %>% min()
  file_gephi <- reshape[edge_start:nrow(reshape), ]
  
  # save to folder
  print('writing file suitable for gephi...')
  write.csv(file_gephi, str_replace(output_path, '.txt', '.csv'), row.names = FALSE)
}

# for adding edge meta data to the network
add_edge_metadata <- function(edge_data, node_data) {
  edges_metada <- edge_data %>%
    left_join(node_data, by = c('Source' = 'nodeID')) %>%
    rename(source_country = country_code, source_city = city_name, source_region = worldbank_region) %>%
    mutate(source_places = n_distinct(Source)) %>%
    left_join(node_data, by = c('Target' = 'nodeID')) %>%
    rename(target_country = country_code, target_city = city_name, target_region = worldbank_region) %>% 
    group_by(target_region) %>%
    mutate(target_places = n_distinct(Source)) %>%
    ungroup()
  
  edges_metada
}

# for computing edge density in the network
get_density <- function(edge_table) {
  graph <- graph.data.frame(edge_table, directed = TRUE)
  edges <- ecount(graph)
  vertices <- vcount(graph)
  density_normalized <- edges / (vertices * (vertices - 1))
  return(density_normalized)
}

# for computing connection counts between two networks
get_connection_counts = function(total_nodes, pre_network_module, post_network_module){
  n_overlap = intersect(
    paste(pre_network_module$Source, pre_network_module$Target, sep = '-'),
    paste(post_network_module$Source, post_network_module$Target, sep = '-')
  ) %>% length()
  
  n_pre <- nrow(pre_network_module)
  n_post <- nrow(post_network_module)
  
  # calculate the Jaccard distance between the pre and post
  jaccard_dist <- (n_overlap * 2) / (n_pre + n_post)
  
  # percentage change in the number of edges
  edge_change <- signif((n_post - n_pre) / (n_pre + n_post) * 100, 4)
  
  # total possible connections
  pre_connect <- signif((n_pre / total_nodes) * 100, 4)
  post_connect <-  signif((n_post / total_nodes) * 100, 4)
  connect_change <- post_connect - pre_connect
  
  # output
  tibble(jaccard_dist, edge_change, pre_connect, post_connect, connect_change)
}

# for computing within network edge statistics
get_edges_within_regions = function(all_regions, pre_network, post_network){
  module_density = list()
  for (i in 1:length(all_regions)) {
    module_nodeIDs <- node_dict %>% filter(worldbank_region == all_regions[i])
    module_nodeIDs = as_vector(module_nodeIDs$ID)
    
    # filter only module nodes
    pre_network_module <- pre_network %>% filter(Source %in% module_nodeIDs & Target %in% module_nodeIDs) %>% 
      select(Source:Target) %>% arrange(Source, Target)
    post_network_module <- post_network %>% filter(Source %in% module_nodeIDs & Target %in% module_nodeIDs) %>% 
      select(Source:Target) %>% arrange(Source, Target)
    
    # get edge density
    pre_density <- signif(get_density(pre_network_module) * 100, 4)
    post_density <- signif(get_density(post_network_module) * 100, 4)
    density_change <- post_density - pre_density
    
    # get pre-post overlap
    n_nodes <- length(module_nodeIDs)
    
    # all possible combination of nodes
    total_nodes <- n_nodes * (n_nodes - 1)
    
    # get connection stats
    counts_connections <- get_connection_counts(total_nodes, pre_network_module, post_network_module)
    
    module_density[[i]] <- tibble(region = all_regions[i],
                                  pre_density,
                                  post_density,
                                  density_change) %>% cbind(counts_connections)
    print(sprintf('module %s out of %s', i, length(all_regions)))
  }
  region_density_all <- do.call(rbind, module_density)
  return(region_density_all) 
}



################################################################################
# PLOTTING
################################################################################
# for saving plots. Defaults to making both PNG and PDF versions.
plot_save <- function(plot_name, size = c(183, 100), pdf = TRUE){ 
  # if no dir called Plots, make one
  if(!dir.exists('Plots')){dir.create('Plots')}
  
  # default size for journal is 183mm wide
  message(sprintf('saving plot to: %s', plot_name))
  
  # saves both pdf and png versions of the plot
  if(pdf){ # when too much data pdf becomes too large
    ggsave(
      filename = paste0(plot_name, '.pdf'),
      width = size[1],
      height = size[2],
      units = 'mm',
      path = 'Plots',
      dpi = 'print',
      device = 'pdf'
    )
  }
  
  ggsave(
    filename = paste0(plot_name, '.png'),
    width = size[1],
    height = size[2],
    units = 'mm',
    path = 'Plots',
    dpi = 'print',
    device = 'png'
  )
}
