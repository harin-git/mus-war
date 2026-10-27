#' Run the script to fetch the data from the OSF repository
require(osfr)
source('utils.R')

# Download once, including saved bootstraps for reproduction without recomputing them.
# To download only Dataset, change TRUE to FALSE in this call.
# Run in the repository root before Dataset or Bootstraps folders exist.
download_data(bootstrapped_data = TRUE)

