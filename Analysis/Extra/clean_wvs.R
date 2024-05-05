#' Cleaning world-value-survey dataset
#' The data can be downloaded from: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

# load study-wide functions and global variables
source('utils.R')

# load wvs data
load("~/WVS7/WVS_Cross-National_Wave_7_rData_v5_0.rdata")
wvs <- `WVS_Cross-National_Wave_7_v5_0`

# get all questions for each city in the wvs
all_qs <- wvs %>%
  select(country_code = B_COUNTRY_ALPHA,
         region_id = N_REGION_ISO,
         lng = O1_LONGITUDE,
         lat = O2_LATITUDE,
         starts_with('Q')
  ) %>%
  filter(lng < 360, lat > -90, lat < 90) %>%
  select(-Q_MODE)

# less than 0 is NA. Normalize
clean_qs <- all_qs %>% 
  mutate_at(vars(starts_with('Q')), ~ifelse(. < 0, NA, .)) %>%
  mutate_at(vars(starts_with('Q')), scale)

# remove columns with all NAs
clean_qs <- clean_qs[,colSums(is.na(clean_qs)) != nrow(clean_qs)]

# save the cleaned dataset
clean_qs %>% write_rds('Dataset/wvs/clean_wvs.rds')
