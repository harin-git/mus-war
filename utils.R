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

# Publication figure settings (Nature Portfolio artwork guide). Applied to every plot through theme_set().
FULL_W  <- 180       # mm, 2-column width
HALF_W  <- 88        # mm, 1-column width
MAX_H   <- 185       # mm, max figure height
FONT    <- 'Arial'   # sans-serif
TXT_MIN <- 5; TXT_MAX <- 7   # pt
BASE    <- 6.5       # pt, base text size
RASTER_DPI <- 300    # min resolution of embedded bitmaps at final size
LW      <- 0.25      # standard line width (~0.53 pt)
LW_MIN  <- 0.12      # thinnest hairline (~0.26 pt)
PT      <- 1 / .pt   # pt -> ggplot text size units
raster_n <- function(width_mm) ceiling(width_mm / 25.4 * RASTER_DPI * 1.05)  # grid cells needed across a panel

theme_pub <- function(base = BASE) {
  theme_classic(base_size = base, base_family = FONT) +
    theme(
      line = element_line(linewidth = LW),
      axis.line = element_line(linewidth = LW, colour = 'black'),
      axis.ticks = element_line(linewidth = LW, colour = 'black'),
      axis.ticks.length = unit(0.8, 'mm'),
      axis.text = element_text(size = base - 0.5, colour = 'black'),
      axis.title = element_text(size = base),
      strip.background = element_blank(),
      strip.text = element_text(size = base, face = 'plain', margin = margin(0.5, 0, 0.8, 0, 'mm')),
      legend.title = element_text(size = base),
      legend.text = element_text(size = base - 0.5),
      legend.key.size = unit(2.5, 'mm'),
      legend.background = element_blank(),
      plot.title = element_text(size = base, face = 'plain', hjust = 0),
      plot.margin = margin(1, 1, 1, 1, 'mm'),
      panel.spacing = unit(2, 'mm')
    )
}
theme_set(theme_pub())
# let the default pdf device (e.g. Rscript's Rplots.pdf) draw 'Arial' as Helvetica
if (!'Arial' %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial = grDevices::pdfFonts()$Helvetica)

# facet strips as boxes (theme_classic default weight), used for the faceted trend plots
strip_box <- theme(strip.background = element_rect(fill = 'white', colour = 'black', linewidth = rel(2)),
                   strip.text = element_text(size = TXT_MAX, margin = margin(0.6, 0, 0.6, 0, 'mm')))

# map style for all maps: no frame or grid, lat/long axis labels
theme_map_axes <- function(base = BASE) theme_pub(base) + theme(axis.line = element_blank(), axis.title = element_blank())
lon_lab <- function(x) ifelse(x == 0, '0°', ifelse(x < 0, paste0(-x, '°W'), paste0(x, '°E')))
lat_lab <- function(x) ifelse(x == 0, '0°', ifelse(x < 0, paste0(-x, '°S'), paste0(x, '°N')))
date_x <- scale_x_date(date_breaks = '1 month', date_labels = '%b', expand = expansion(mult = 0.01))

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

LANG_COLS <- c(Ukrainian = UKRAINE_COLOUR, Russian = RUSSIAN_COLOUR, Kazakh = KAZAKHSTAN_COLOUR,
               English = ENGLISH_COLOUR, Other = 'grey60')
REGION_COLS <- setNames(WORLDBANK_REGION_COLOUR$region_colour, WORLDBANK_REGION_COLOUR$worldbank_region)

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

# for bootstrap confidence estimates: mean, 95% percentile CI, bootstrap z and two-tailed P
# (z = (mean - null) / bootstrap SD, P from the standard normal; p_perc = two-tailed percentile P)
get_boot_mean_ci <- function(x, var_name, obs_stat = 0, alpha = 0.05, p_parametric = TRUE, ...){
  # Saved language proportions can carry class 'table'; intervals must be plain numeric.
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  z <- (mean(x) - obs_stat) / sd(x)
  output <- tibble(m = mean(x),
                   lower_ci = unname(quantile(x, alpha / 2)),
                   upper_ci = unname(quantile(x, 1 - alpha / 2)),
                   z = z,
                   p = if (p_parametric) 2 * pnorm(-abs(z)) else min(1, 2 * min(mean(x <= obs_stat), mean(x >= obs_stat))),
                   p_perc = min(1, 2 * min(mean(x <= obs_stat), mean(x >= obs_stat))))
  colnames(output) <- paste(var_name, colnames(output), sep = '_')
  return(output)
}

# report a P value as in the paper (three decimals, < 0.001 below)
fmt_p <- function(p) ifelse(p < 0.001, 'P < 0.001', sprintf('P = %.3f', p))

# Bootstrap diagnostics and Welch summaries shared by the analysis scripts.
boot_test <- function(x, null = 0) {
  x <- x[is.finite(x)]
  B <- length(x)
  est <- mean(x); se <- sd(x)
  z <- (est - null) / se
  p_z <- 2 * pnorm(-abs(z))
  p_perc <- min(1, 2 * min(mean(x <= null), mean(x >= null)))
  sw <- if (B >= 3 && B <= 5000 && se > 0) shapiro.test(x)$p.value else NA_real_
  tibble(B, est, lo = unname(quantile(x, 0.025)), hi = unname(quantile(x, 0.975)),
         se, z, p_z, p_perc, shapiro_p = sw)
}

# Welch t-test + Cohen's d (pooled SD, as in utils.R get_t_stat) with df
welch <- function(g1, g2) {
  g1 <- g1[!is.na(g1)]; g2 <- g2[!is.na(g2)]
  if (length(g1) < 2 || length(g2) < 2 || (var(g1) == 0 && var(g2) == 0))
    return(tibble(t = NA_real_, df = NA_real_, p = NA_real_, d = NA_real_,
                  n1 = length(g1), n2 = length(g2)))
  tt <- t.test(g1, g2)
  n1 <- length(g1); n2 <- length(g2)
  sp <- sqrt(((n1 - 1) * var(g1) + (n2 - 1) * var(g2)) / (n1 + n2 - 2))
  tibble(t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
         d = (mean(g1) - mean(g2)) / sp, n1, n2)
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

# jaccard similarity between two vectors
jaccard_similarity <- function(vec1, vec2) {
    intersection = length(intersect(vec1, vec2))
    union = length(vec1) + length(vec2) - intersection
    return (intersection/union)
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
    group_by(trackID, nodeID) %>%
    filter(date == min(date)) %>% # consider only first appearance
    group_by(trackID) %>%
    mutate(day_since = as.numeric(as.Date(date) - min(as.Date(date))) + 1) %>% # calculate day since it first appeared
    arrange(trackID, nodeID) %>%
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
# Save the last (or given) plot at its printed size in mm: editable SVG, a PDF made from the SVG (for linked
# placement in Illustrator), a 300-dpi PNG, and the plotted data as a source-data CSV when `data` is given.
# Output goes to Plots/ (or the PLOT_DIR environment variable).
plot_save <- function(plot_name, size = c(FULL_W, 100), data = NULL, plot = last_plot(), pdf = TRUE, source_name = NULL){
  root <- Sys.getenv('PLOT_DIR', 'Plots')
  f <- file.path(root, plot_name)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  message(sprintf('saving plot to: %s', f))
  svglite::svglite(paste0(f, '.svg'), width = size[1] / 25.4, height = size[2] / 25.4, fix_text_size = FALSE)
  print(plot); invisible(dev.off())
  if (pdf) rsvg::rsvg_pdf(paste0(f, '.svg'), paste0(f, '.pdf'))
  ggsave(paste0(f, '.png'), plot, width = size[1], height = size[2], units = 'mm', dpi = 300, bg = 'white')
  if(!is.null(data)){
    # Exploratory plots without a manuscript figure number are not release Source Data.
    folder <- if (!is.null(source_name) && startsWith(source_name, 'Unnumbered_')) 'exploratory_data' else 'source_data'
    sd <- file.path(root, folder); dir.create(sd, showWarnings = FALSE)
    if (is.null(source_name)) stop('Provide an explicit figure-numbered source_name when saving Source Data.')
    if (length(source_name) != 1L || grepl('[/\\\\]', source_name))
      stop('source_name must be a filename stem, not a path.')
    write_csv(data, file.path(sd, paste0(source_name, '.csv')))
  }
  check_panel(paste0(f, '.svg'), size[1], size[2])
  invisible(plot)
}

# Check an exported SVG against the artwork guide (size, 5-7 pt text, lines >= 0.25 pt, bitmaps >= 300 dpi); warns only
check_panel <- function(svg, w, h) {
  s <- paste(readLines(svg, warn = FALSE), collapse = '\n')
  issues <- c()
  if (w > FULL_W) issues <- c(issues, sprintf('width %s mm > %s mm', w, FULL_W))
  if (h > MAX_H)  issues <- c(issues, sprintf('height %s mm > %s mm', h, MAX_H))
  fs <- as.numeric(regmatches(s, gregexpr("(?<=font-size: )[0-9.]+(?=px)", s, perl = TRUE))[[1]])
  if (length(fs) && (min(fs) < TXT_MIN - 0.01 || max(fs) > TXT_MAX + 0.01))
    issues <- c(issues, sprintf('text %.1f-%.1f pt (allowed %s-%s)', min(fs), max(fs), TXT_MIN, TXT_MAX))
  sw <- as.numeric(regmatches(s, gregexpr("(?<=stroke-width: )[0-9.]+", s, perl = TRUE))[[1]]); sw <- sw[sw > 0]
  if (length(sw) && min(sw) < 0.25) issues <- c(issues, sprintf('hairline %.2f pt < 0.25 pt', min(sw)))
  im <- regmatches(s, gregexpr("<image width='[0-9.]+'[^>]*base64,[A-Za-z0-9+/=]+", s))[[1]]
  for (x in im) {
    wpt <- as.numeric(sub("<image width='([0-9.]+)'.*", '\\1', x))
    raw <- jsonlite::base64_dec(substr(sub('.*base64,', '', x), 1, 64))
    px <- sum(as.integer(raw[17:20]) * 256^(3:0))
    if (px / (wpt / 72) < RASTER_DPI * 0.95) issues <- c(issues, sprintf('embedded bitmap %.0f dpi < %s dpi', px / (wpt / 72), RASTER_DPI))
  }
  if (length(issues)) warning(basename(svg), ': ', paste(unique(issues), collapse = '; '), call. = FALSE)
  invisible(issues)
}

# Embedded raster layer for dense surfaces (KDE, smoothed maps): one image in the SVG instead of thousands of tiles.
# df must be a regular grid; empty rows/columns stay transparent so the image keeps its position.
raster_layer <- function(df, x, y, value, low, high, limits = range(df[[value]], na.rm = TRUE), interpolate = TRUE) {
  step <- function(v) { d <- diff(v); min(d[d > 1e-9]) }
  sx <- step(sort(unique(df[[x]]))); sy <- step(sort(unique(df[[y]])))
  xs <- seq(min(df[[x]]), max(df[[x]]) + sx / 2, by = sx); ys <- seq(min(df[[y]]), max(df[[y]]) + sy / 2, by = sy)
  m <- matrix(NA_real_, nrow = length(ys), ncol = length(xs))
  m[cbind(round((df[[y]] - min(ys)) / sy) + 1, round((df[[x]] - min(xs)) / sx) + 1)] <- df[[value]]
  ramp <- scales::col_numeric(c(low, high), domain = limits, na.color = NA, alpha = TRUE)
  img <- matrix(ramp(pmin(pmax(m, limits[1]), limits[2])), nrow = nrow(m))
  img <- img[nrow(img):1, , drop = FALSE]
  hx <- diff(xs[1:2]) / 2; hy <- diff(ys[1:2]) / 2
  annotation_raster(img, xmin = min(xs) - hx, xmax = max(xs) + hx, ymin = min(ys) - hy, ymax = max(ys) + hy, interpolate = interpolate)
}
