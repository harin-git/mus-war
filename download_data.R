#' Run the script to fetch the data from the OSF repository

require(osfr)

# download without bootstrapped outcomes
download_data(bootstrapped_data = FALSE)

# download with bootstrapped outcomes (large and may take some time)
download_data(bootstrapped_data = TRUE)
