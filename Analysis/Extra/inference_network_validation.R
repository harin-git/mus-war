#' Compare the inference network with co-occurence network
#' Related to Methods and Supplementary Information in paper

# load study-wide functions and global variables
source('utils.R')

# load coco network
pre_coco <- read_rds('Dataset/network/coco/pre_cooccurence.rds')
post_coco <- read_rds('Dataset/network/coco/post_cooccurence.rds')

# load netinf network
pre_netinf <- read.csv('Dataset/network/netinf/pre_E10000_B1000_agg.csv')
post_netinf <- read.csv('Dataset/network/netinf/post_E10000_B1000_agg.csv')

# filter only the post soviet cities in netinf
post_soviet_nodes <- pre_coco$Source %>% unique()

pre_netinf <- pre_netinf %>% filter(Source %in% post_soviet_nodes & Target %in% post_soviet_nodes)
post_netinf <- post_netinf %>% filter(Source %in% post_soviet_nodes & Target %in% post_soviet_nodes)

# Compare all directed, non-self city pairs, including pairs absent from NETINF.
# Weight.x is NETINF; Weight.y is co-occurrence. Missing edges in either are zero.
join_networks <- function(netinf, coco) {
  pairs <- expand_grid(Source = post_soviet_nodes, Target = post_soviet_nodes) %>%
    filter(Source != Target)
  stopifnot(!anyDuplicated(netinf[c('Source', 'Target')]),
            !anyDuplicated(coco[c('Source', 'Target')]))
  pairs %>%
    left_join(netinf %>% select(Source, Target, Weight.x = Weight), by = c('Source', 'Target')) %>%
    left_join(coco %>% select(Source, Target, Weight.y = Weight), by = c('Source', 'Target')) %>%
    replace_na(list(Weight.x = 0, Weight.y = 0))
}
pre_netinf <- join_networks(pre_netinf, pre_coco)
post_netinf <- join_networks(post_netinf, post_coco)

# Descriptive Pearson correlations. Dyads share cities, so do not report an
# independence-based P value. These are Fisher-z intervals, not bootstrap CIs.
network_summary <- function(x, period) {
  r <- cor(x$Weight.x, x$Weight.y)
  ci <- tanh(atanh(r) + c(-1, 1) * qnorm(0.975) / sqrt(nrow(x) - 3))
  tibble(period, n_pairs = nrow(x), r, fisher_lower_ci = ci[1],
         fisher_upper_ci = ci[2], interval = '95% Fisher-z CI (descriptive)')
}
network_validation <- bind_rows(network_summary(pre_netinf, 'pre'),
                                network_summary(post_netinf, 'post'))
print(network_validation, width = Inf)
