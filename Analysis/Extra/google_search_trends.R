#' Analyze Google search trends of top 10 Ukrainian songs across time and regions
#' Replication of Fig. 4 in paper, included as Supplementary Information

# load study-wide functions and global variables
source('utils.R')
require(psych) # for correlation with WVS

################################################################################
# PREPARE DATA
################################################################################
# clean the google trends data
dir <- 'Dataset/private/google_ua_top10_songs'

# load the data
files <- list.files(dir)

store_trend = list()
store_map = list()
for(i in 1:length(files)){
    file_name <- files[i]

    # load the data
    d <- read.delim2(paste0(dir, '/', file_name), header = TRUE)

    # extract the medium and type
    if(substr(file_name, 1, 2) == 'gg'){
        medium <- 'Google'
    } else {
        medium <- 'YouTube'
    }

    if(substr(file_name, 4, 6) == 'geo'){
        type <- 'region'
    } else {
        type <- 'country'
    }

    # if interest by geographical region
    if(type == 'region'){
        # extract the search term
        search_term <- d[1, 1] %>% 
            str_extract("Region,([^:]+):") %>%  # Extract everything between "Region," and ":"
            str_remove("Region,") %>%           # Remove "Region,"
            str_remove(":") %>%                 # Remove ":"
            str_trim()                          # Remove any whitespace
        
        regions <- d[2:nrow(d), 1] %>%
            reshape2::colsplit(., ",", names = c("region", "interest"))

        store_map[[i]] <- cbind(medium, term = search_term, regions)
    }

    # if interest by country
    if(type == 'country'){
        # extract the search term
        search_term <- d[1, 1] %>% 
            str_extract("Day,([^:]+):") %>%  # Extract everything between "Region," and ":"
            str_remove("Day,") %>%           # Remove "Region,"
            str_remove(":") %>%                 # Remove ":"
            str_trim()                          # Remove any whitespace

        timeline <- d[2:nrow(d), 1] %>%
            reshape2::colsplit(., ",", names = c("date", "interest"))
        
        store_trend[[i]] <- cbind(medium, term = search_term, timeline)
    }
}

trend_output <- do.call(rbind, store_trend) %>% as_tibble()
region_output <- do.call(rbind, store_map) %>% as_tibble()


################################################################################
# SEARCH TREND OVER TIME
################################################################################
# clean and fill missing values
trend_clean <- trend_output %>%
    filter(medium == 'Google') %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(interest = as.numeric(interest)) %>%
    complete(date, term, medium, fill = list(interest = 0)) %>%
    mutate(interest = ifelse(is.na(interest), 0, interest)) %>%
    # add labels for the terms
    mutate(label = case_when(
        term == "війна" ~ "war",
        (term == "youtube" & medium == "Google") ~ "baseline",
        TRUE ~ "search term"
    ))

trend_plot <- trend_clean %>%
    group_by(date, label, medium) %>%
    summarise(mean_interest = mean(interest))

trend_plot %>%
    ggplot(aes(x = date, y = mean_interest, 
               color = interaction(medium, label))) +
    geom_line() +
    # facet_wrap(~medium) +
    scale_color_manual(values = c(
        "Google.baseline" = "gray50",
        "Google.search term" = UKRAINE_COLOUR,
        "Google.war" = "red"
        # "YouTube.baseline" = "gray90",
        # "YouTube.search term" = "red",
        # "YouTube.war" = "red"
    )) +
    # scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
    labs(title = "", 
         x = "Date", 
         y = "Mean Interest",
         color = "") +
    theme(legend.position = "none") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", 
        limits = c(min(as.Date(trend_plot$date)), max(as.Date(trend_plot$date)) + 7)) +
    geom_vline(xintercept = WAR_START, linetype = 'dashed')

# save outcome
plot_save('SI/google_search_trend_overtime', c(100, 70))
trend_clean %>% write.csv("Dataset/private/ua_google_trends.csv", row.names = FALSE)

# get mean and ci
trend_pre_post <- trend_clean %>%
    filter(label == 'search term', medium == 'Google') %>% 
    mutate(pre_post = ifelse(date < WAR_START, 'pre', 'post')) %>%
    # aggregate by date
    group_by(date, pre_post) %>%
    summarise(interest = mean(interest))

trend_pre_post %>%
    group_by(pre_post) %>%
    summarise(m = mean(interest), ci = CI(interest)) %>%
    mutate(ci_lower = round(m - ci, 2), ci_upper = round(m + ci, 2))

# get effect size
trend_pre_post %>%
    ungroup() %>%
    select(pre_post, interest) %>%
    pivot_wider(names_from = pre_post, values_from = interest) %>%
    summarise(get_t_stat(post %>% unlist(), pre %>% unlist()))

################################################################################
# REGIONAL TREND
################################################################################
region_clean <- region_output %>%
    mutate(interest = as.numeric(interest)) %>%
    complete(region, term, medium, fill = list(interest = 0)) %>%
    mutate(interest = ifelse(is.na(interest), 0, interest))

# exclude baseline and war
region_clean <- region_clean %>%
    filter(term != "google" & term != "війна" & term != "youtube")

# make interest aggregate by region
interest_agg <- region_clean %>%
    group_by(region, medium) %>%
    summarise(interest = mean(interest))

# save the interest agg data and add iso_3166_2 code using LLM
# interest_agg %>% write.csv("Dataset/private/ua_google_trends_region.csv", row.names = FALSE)
interest_agg <- read.csv("Dataset/private/ua_google_trends_region.csv")

# add map data
# define CRS
library(sf)
sf_use_s2(FALSE)
CRS <-  "+proj=longlat +datum=WGS84"

ukraine_regions <- read_rds('Dataset/census/ukraine_regions_map.rds')
russian_regions <- read_rds('Dataset/census/russian_regions_map.rds')

# update ukraine regions including crimea
region <- ukraine_regions %>% bind_rows(russian_regions %>% filter(iso_3166_2 %in% c('UA-43', 'UA-40')))

# Transform geometries to the specified CRS
region <- st_transform(region, CRS) 

# join ethnic data 
plot_data <- region %>% left_join(interest_agg, by = 'iso_3166_2')

# plot the census map
NQUANTILE <- 10
COLOR_GRADIENT <- c(UKRAINE_COLOUR, UKRAINE_COLOUR2)

plot_data %>%
  filter(medium == "Google") %>%
  mutate(quant = ntile(interest, NQUANTILE)) %>%
  ggplot() +
  geom_sf(aes(fill = quant)) +
  labs(title = "Google", subtitle = "Interest by Region: Top10 Ukrainian Songs", fill = '') +
  theme_map() +
  scale_fill_gradient(low = COLOR_GRADIENT[1], high = COLOR_GRADIENT[2], guide = F)

plot_save('SI/google_search_trend_map', c(80, 70))

# compute stats with WVS
wvs_pca <- read_rds('Dataset/wvs/global_wvs_pca_loadings.rds')
interest_google <- interest_agg %>% filter(medium == 'Google')
wvs_google <- interest_google %>% left_join(wvs_pca, by = 'iso_3166_2')

# aggregate by iso_3166_2
wvs_google_agg <- wvs_google %>%
    group_by(iso_3166_2, interest) %>%
    summarise(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3)) %>%
    ungroup()

cor_result <- corr.test(wvs_google_agg %>% select(interest, PC1:PC3), adjust = 'bonferroni', method = 'spearman')
print(cor_result, short=FALSE) 
print(cor_result$stars, quote=FALSE, short=FALSE) 
