#' PCA analysis of the WVS data
#' Related to Fig.3 in paper

# load study-wide functions and global variables
source('utils.R')

# cluster analysis
require(ggrepel) # avoid overlapping labels in plot
require(factoextra) # PCA analysis
require(psych) # PCA analysis
require(ggfortify) # plot PCAs
require(plotly) # plot 3D PCAs
require(proxy) # for cosine similarity calculations
require(cowplot)

################################################################################
# DATA PREPARATION
################################################################################
city_meta <- read_rds("Dataset/meta/city_metadata.rds")
wvs <- read_rds('Dataset/wvs/clean_wvs.rds')

################################################################################
# FUNCTIONS
################################################################################
# find the closest point in dataset2 for each point in dataset1
find_closest_point <- function(lon, lat, dataset) {
  d <- geosphere::distVincentySphere(c(lon, lat), cbind(dataset$lon, dataset$lat))
  closest_index <- which.min(d)
  city_name <- dataset[closest_index, ]$city_name
  nodeID <- dataset[closest_index, ]$nodeID
  return(tibble(nodeID, city_name, distance = min(d)))
}

explore_pca <- function(data, cnt, topN){
  cnt_ft <- data %>% filter(country_code == cnt)
  
  # drop questions more than one NAs
  include_cols <- colSums(is.na(data)) == 0
  n_q <- sum(include_cols)
  
  message(sprintf('In %s there are %s questions remain after droping questions with no data', cnt, n_q - 6))
  cnt_ft <- cnt_ft[, include_cols]
  
  # perform PCA
  feat <- cnt_ft %>% select(starts_with('Q'))
  meta <- cnt_ft %>% select(!starts_with('Q'))
  pca_res <- prcomp(feat)
  
  # results
  sum_pca = summary(pca_res)
  scree_plot = fviz_eig(pca_res, addlabels = TRUE)
  eig.val <- get_eigenvalue(pca_res)
  
  top_vars_pca1 = fviz_contrib(pca_res, choice = "var", axes = 1, top = topN)
  top_vars_pca2 = fviz_contrib(pca_res, choice = "var", axes = 2, top = topN)
  top_vars_pca3 = fviz_contrib(pca_res, choice = "var", axes = 3, top = topN)
  
  return(list(
    sum_pca = sum_pca,
    scree_plot = scree_plot,
    eig.val = eig.val,
    top_vars_pca1 = top_vars_pca1,
    top_vars_pca2 = top_vars_pca2,
    top_vars_pca3 = top_vars_pca3,
    loadings =  pca_res$rotation[, 1:3]
  ))
}

################################################################################
# MAP PROJECTION
################################################################################
# filter for ukraine and russia
ua_ru_qs <-  wvs %>%
  filter(country_code %in% c('RUS', 'UKR')) %>%
  mutate(country_code = ifelse(country_code == 'RUS', 'RU', 'UA'))

# project the points to map polygons to get meta data about the regions
sf::sf_use_s2(FALSE)
map <- read_rds('Dataset/census/world_map.rds')
crs <-  "+proj=longlat +datum=WGS84"
sf_map <- map %>% sf::st_as_sf(CRS = crs)

region_points <- ua_ru_qs %>%
  select(region_id, lon, lat) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = crs)
mapping <- sf::st_within(region_points, sf_map) %>% as.character() %>% as.numeric()
mapping[is.na(mapping)] <- 1 # fake number to match the cases
mapped_regions <- map[mapping, ] %>% as_tibble() %>% select(iso_3166_2, city_name = name, name_local, type)

# re-bind with wvs and remove the random 1 row region that was used as a filler
ua_ru_qs <- cbind(mapped_regions, ua_ru_qs) %>%
  filter(iso_3166_2 != map$iso_3166_2[1])

# recode crimea region as UA not RU - matching th the local music map
ua_ru_qs <- ua_ru_qs %>%
  mutate(country_code = ifelse(iso_3166_2 %in% c('UA-43', 'UA-40'), 'UA', country_code))

# report statistics
ua_ru_qs %>%
  group_by(country_code) %>%
  summarise(n_regions = n_distinct(lat, lon), 
            n_q = ncol(ua_ru_qs) - 8,
            n_response = n()
            )

################################################################################
# PCA
################################################################################
# group by region and take the mean of each region
region_grouping <- ua_ru_qs %>%
  # take 3 significant figs of lat and lon to reduce noise
  mutate(lat = signif(lat, 3), lon = signif(lon, 3)) %>%
  # group by each region
  group_by(country_code, region_id, iso_3166_2, city_name, lat, lon) %>%
  # get the mean of each region
  summarise(across(starts_with('Q'), function(x){mean(x,na.rm = T)})) %>%
  # replace the NAs by the regional mean
  group_by(country_code, iso_3166_2) %>%
  mutate(across(starts_with('Q'), function(x){ifelse(is.na(x), mean(x, na.rm = T), x)})) %>%
  ungroup()

# do a global and separate PCAs for each country
for (cnt in c('UA', 'RU', 'Global')) {
  if(cnt != 'Global'){
    # filter each country
    cnt_ft <- region_grouping %>% filter(country_code == cnt)
  } else {
    # no filter
    cnt_ft <- region_grouping
  }

  # drop questions more than one NAs
  include_cols <- colSums(is.na(cnt_ft)) == 0
  n_q <- sum(include_cols)
  
  message(sprintf('In %s there are %s questions remain after droping questions with no data', cnt, n_q - 6))
  cnt_ft <- cnt_ft[, include_cols]
  
  # perform PCA
  feat <- cnt_ft %>% select(starts_with('Q'))
  meta <- cnt_ft %>% select(!starts_with('Q'))
  pca_res <- prcomp(feat)
  explore_pca_qs <- pca_res[["rotation"]] %>% as.data.frame()
  sum_pca <- summary(pca_res)
  
  (elbow <- fviz_nbclust(feat, kmeans, method = "wss") +
      geom_vline(xintercept = 4, linetype = 2)+
      labs(subtitle = "Elbow method"))
  
  (silhouette <- fviz_nbclust(feat, kmeans, method = "silhouette")+
      labs(subtitle = "Silhouette method"))
  
  # PCA loadings up to 3rd PC
  pca_loadings <- meta %>%
    mutate(PC1 = pca_res$x[,'PC1'], 
           PC2 = pca_res$x[,'PC2'], 
           PC3 = pca_res$x[,'PC3'])
  
  # save data
  pca_loadings %>% write_rds(sprintf('Dataset/wvs/%s_wvs_pca_loadings.rds', cnt))
}


################################################################################
# INTERPRETING PCA
################################################################################
# analyze PC results
pca_UA <- run_pca(region_grouping, "UA", 20)

# explore PCA results
pca_UA <- explore_pca(region_grouping, "UA", 10)

pca_UA[1] # sum_pca
pca_UA[2] # scree_plot
pca_UA[3] # eig.val
pca_UA[4] # top_vars_pca1
pca_UA[5] # top_vars_pca2
pca_UA[6] # top_vars_pca3
pca_UA[7] # loading

pca_RU = explore_pca(region_grouping, "RU", 10)
pca_RU[1] # sum_pca
pca_RU[2] # scree_plot
pca_RU[3] # eig.val
pca_RU[4] # top_vars_pca1
pca_RU[5] # top_vars_pca2
pca_RU[6] # top_vars_pca3

plot_grid(pca_UA[[4]], pca_RU[[4]], ncol = 1 ,labels = c("UA", "RU"))
plot_grid(pca_UA[[5]], pca_RU[[5]], ncol = 1 ,labels = c("UA", "RU"))

