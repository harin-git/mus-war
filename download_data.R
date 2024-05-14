#' Run the script to fetch the data from the OSF repository
require(osfr)
source('utils.R')

# Choose between the two options to fetch data.
# download without bootstrapped outcomes (3.27GB)
download_data(bootstrapped_data = FALSE)

# download with bootstrapped outcomes (5.30GB). Necessary to reproduce results by saving time.
download_data(bootstrapped_data = TRUE)

