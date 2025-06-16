#' The NETINF algorithm is used to infer the global diffusion network using song cascades. 
#' It needs to be downloaded to run the code below.
#' Visit the SNAP website for download of the software: http://snap.stanford.edu/netinf/
#' Related to Fig.5 in paper

# load study-wide functions and global variables
source('utils.R')
library(doParallel) # for parallel processing
library(foreach)
library(progressr)


################################################################################
## DATA PREPARATION
################################################################################
chart_data <- read_rds('Dataset/chart/study_set_chart.rds')

# make node table
nodetable <- chart_data %>% select(ID = nodeID, country_code, Label = city_name) %>% distinct(ID, Label, country_code)
nodetable %>% write.csv('netinf/node_table.csv', row.names = FALSE)


################################################################################
## SETTINGS
################################################################################
N_BOOT <- 1000                  # number of bootstraps to compute te network. Note it take very long!
N_EDGES <- 10000                # number of edges to retain. Through trial and error, we found after 10k network structure doesn't change much
N_CORE <- 16                    # number of cores to parallel process
NSAMPLE <- 100                  # number of songs to bootstrap sample
NETINF_FOLDER <- 'XXX'          # should be absolute path where netinf.exec file is
SAVE_PATH <- 'netinf/inference' # path to save the output
INVASION_PERIOD <- 'pre'        # set value 'all', 'pre' or 'post'
SAVE_NAME <- sprintf('%s_E%s_B%s', INVASION_PERIOD, N_EDGES, N_BOOT)

DT <- chart_data %>% filter(pre_post == INVASION_PERIOD)


################################################################################
## EXECUTE
################################################################################
# make output dir if it doesn't already exist
new_dir_path <- file.path(SAVE_PATH, SAVE_NAME)
if(!dir.exists(new_dir_path)){
  message(sprintf("creating output directory at %s", new_dir_path))
  dir.create(new_dir_path, recursive = TRUE)
} else {
  message('Output directory already exists')
}

# make dummy ground truth dictionary
netinf_groundtruth_txt <- file.path(SAVE_PATH, 'ground_truth.txt')
make_dummy_dictionary(DT, netinf_groundtruth_txt)

# Bootstrap n times
registerDoParallel(cores = N_CORE)
message(sprintf('Parallel processing NETINF for %s times with %s cores', N_BOOT, N_CORE))
pb <- progress::progress_bar$new(total = N_BOOT)
for(i in i:N_BOOT) {
  print(i)
  message('sampling songs at random')
  # sample at random across all cities by the minimum number to balance the number of unique songs
  sampleDT <- DT %>%
    group_by(nodeID) %>%
    filter(trackID %in% sample(unique(trackID), NSAMPLE))
  
  # make network for NETINF
  netinf_input_txt <- file.path(SAVE_PATH, SAVE_NAME, sprintf('%s_%s_input.txt', SAVE_NAME, i))
  get_song_cascade(sampleDT, netinf_input_txt)
  
  # Automate netinf
  message('running NETINF')
  netinf_output_txt <- file.path(SAVE_PATH, SAVE_NAME, sprintf('%s_%s_output.txt', SAVE_NAME, i))
  run_netinf(NETINF_FOLDER,
             netinf_input_txt,
             netinf_groundtruth_txt,
             netinf_output_txt,
             E = N_EDGES)
  pb$tick()
}

  