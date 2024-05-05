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

# fill in zeros for missing edges
pre_netinf <- pre_netinf %>% full_join(pre_coco, by = c('Source', 'Target')) %>% replace_na(list(Weight.y = 0))
post_netinf <- post_netinf %>% full_join(post_coco, by = c('Source', 'Target')) %>% replace_na(list(Weight.y = 0))

# test correlations
cor.test(pre_netinf$Weight.x, pre_netinf$Weight.y)
cor.test(post_netinf$Weight.x, post_netinf$Weight.y)
