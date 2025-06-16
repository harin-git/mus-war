#' Aligning the layouts of the pre- and post-invasion networks.
#' The script outputs edge and node tables to be visualized using Gephi (https://gephi.org/)
#' Related to Fig.5 in paper

# load study-wide functions and global variables
source('utils.R')

method <- 'coco' # 'netinf' for using network inference, 'coco' for co-occurrence approach

# load data
switch(method,
       netinf = {
         # Using the network inference through NETINF
         pre_edge <- read.csv('Dataset/network/netinf/pre_E10000_B1000_agg.csv')
         post_edge <- read.csv('Dataset/network/netinf/post_E10000_B1000_agg.csv')
         
         # apply 5% threshold
         trim_weight <- 1000 * 0.05
       },
       coco = {
         library(backbone)

         # using co-occurrence method
         pre_edge <- read_rds('Dataset/network/coco/pre_cooccurence.rds')
         post_edge <- read_rds('Dataset/network/coco/post_cooccurence.rds')

         # remove duplicates
         pre_edge <- pre_edge %>% rowwise() %>% mutate(key = sort(c(Source, Target)) %>% paste(collapse = '-'))
         pre_edge <- pre_edge %>% group_by(key) %>% slice(1) %>% ungroup()
         
         post_edge <- post_edge %>% rowwise() %>% mutate(key = sort(c(Source, Target)) %>% paste(collapse = '-'))
         post_edge <- post_edge %>% group_by(key) %>% slice(1) %>% ungroup()
         
         # use backbone algorithm to trim the edges. 
         # he thresholds are roughly matched to retain 1,000 edges in both groups
         pre_graph <- igraph::graph_from_data_frame(pre_edge)
         E(pre_graph)$weight <- pre_edge$Weight
         pre_backbone <- disparity(pre_graph, alpha = 0.285, narrative = TRUE, class = "igraph")
         remain_pre <- pre_backbone %>% igraph::as_edgelist() %>% nrow()
         
         post_graph <- igraph::graph_from_data_frame(post_edge)
         E(post_graph)$weight <- post_edge$Weight
         post_backbone <- disparity(post_graph, alpha = 0.23, narrative = TRUE, class = "igraph")
         remain_post <- post_backbone %>% igraph::as_edgelist() %>% nrow()

         message(sprintf('After applying the backbone algorithm, %s pre and %s post edges remain', remain_pre, remain_post))

         # update as post and pre edge with uniform weights
         pre_edge <- pre_backbone %>% igraph::as_edgelist() %>% as_tibble() %>% cbind(Weight = 1)
         pre_edge <- pre_edge %>% rename(Source = V1, Target = V2)
         pre_edge <- pre_edge %>% mutate(across(where(~is.character(.)), as.numeric))

         post_edge <- post_backbone %>% igraph::as_edgelist() %>% as_tibble() %>% cbind(Weight = 1)
         post_edge <- post_edge %>% rename(Source = V1, Target = V2)
         post_edge <- post_edge %>% mutate(across(where(~is.character(.)), as.numeric))

       }
       )

# fill in the matrix
dim <- 1423
  
pre_mat <- Matrix::sparseMatrix(
  i = pre_edge$Source,
  j = pre_edge$Target,
  x = pre_edge$Weight,
  dims = c(dim, dim),
  symmetric = FALSE) %>%
  as.matrix()

pre_mat_t <- t(pre_mat)

post_mat <- Matrix::sparseMatrix(
  i = post_edge$Source,
  j = post_edge$Target,
  x = post_edge$Weight,
  dims = c(dim, dim),
  symmetric = FALSE) %>%
  as.matrix()

post_mat_t <- t(post_mat)

# join the two matrices and transpose
join_mat <- rbind(cbind(pre_mat, pre_mat_t), cbind(post_mat, post_mat_t))
cor_mat <- cor(join_mat)

# convert back to source and target
edgetable <- cbind(Source = 1:(dim*2), join_mat %>% as_tibble())
edgetable <- edgetable %>% pivot_longer(V1:V2846, names_to = 'Target', values_to = 'Weight')
edgetable$Target <- str_remove_all(edgetable$Target, 'V') %>% as.numeric()
edgetable <- edgetable %>% filter(Weight > 0)

# make nodetable
citymeta <- read_rds('Dataset/meta/city_metadata.rds')
nodetable <- citymeta %>% select(ID = nodeID, Label = city_name, country_code, country_name, continent, worldbank_region)
nodetable <- nodetable %>% filter(ID %in% unique(c(edgetable$Source, edgetable$Target)))

pre_node <- nodetable %>% cbind(pre_post = 'pre')

# stack the pre node table with post
post_node <- nodetable %>% cbind(pre_post = 'post')
post_node$ID <- post_node$ID + dim

nodetable <- pre_node %>% rbind(post_node)

# add colour HEX
nodetable <- nodetable %>%
  left_join(WORLDBANK_REGION_COLOUR) %>%
  rename(colour = region_colour)

# export for table to be inserted into Gephi
edgetable %>% write_csv(sprintf('Dataset/network/%s/aigned_edgetable.csv', method))
nodetable %>% write_csv(sprintf('Dataset/network/%s/aigned_nodetable.csv', method))

