library(geosphere)
library(igraph)
library(dplyr)
library(blockTools)
library(foreach)
library(doParallel)
library(rlang)
library(Matrix)

setwd('~/Desktop/mit_15840_paper/')
airbnb_listings <- read.csv('Airbnb_Listings_Miami.csv', row.names=1)
set.seed(15840)

Mode <- function(vector) { 
  as.numeric(names(sort(-table(vector)))[1])
}

imputeMode <- function(vector) { 
  vector[is.na(vector)] <- Mode(vector)
  return(vector)
}

imputeMean <- function(vector) { 
  vector[is.na(vector)] <- mean(vector, na.rm=T)
  return(vector)
}

create_df_for_blocking <- function(df, var_name) { 
  df %>%
    group_by(!!var_name) %>% 
    summarise(n = n(),
              avg_reviews_scaled = mean(reviews_scaled),
              avg_satisfaction_scaled = mean(satisfaction_scaled),
              avg_accom_scaled = mean(accomm_scaled),
              avg_bed_scaled = mean(bed_scaled),
              avg_bath_scaled = mean(bath_scaled),
              avg_minstay_scaled = mean(minstay_scaled),
              avg_lat = mean(latitude),
              avg_lon = mean(longitude),
              pct_private_room = mean(`Private room`),
              pct_shared_room = mean(`Shared room`),
              pct_entire_home = mean(`Entire home/apt`),
              sd_reviews_scaled = sd(reviews_scaled),
              sd_satisfaction_scaled = sd(satisfaction_scaled),
              sd_accom_scaled = sd(accomm_scaled),
              sd_bed_scaled = sd(bed_scaled),
              sd_bath_scaled = sd(bath_scaled),
              sd_minstay_scaled = sd(minstay_scaled),
              sd_lat = sd(latitude),
              sd_lon = sd(longitude)) %>% 
    ungroup() -> df
  
  return(df)
}

generate_blocks <- function(blocking_df, var_name) {
  blocks <- blockTools::block(as.data.frame(blocking_df), id.vars=var_name, 
                              block.vars = c('n', 
                                             'avg_reviews_scaled',
                                             'avg_satisfaction_scaled',
                                             'avg_bed_scaled',
                                             'avg_bath_scaled',
                                             'avg_minstay_scaled',
                                             'avg_lat',
                                             'avg_lon',
                                             'pct_private_room',
                                             'pct_shared_room'))
}

airbnb_listings, clusters_distance_room_type_accom, 
graph_distance_room_type_accom, .75, .5, 100, blocks_base

hajek_probabilities <- function(dataframe, clusters, graph, threshold, treatment_prob, n_iter, n_clusters, blocks) { 
  n_effective_treatment <- matrix(0L, nrow = nrow(dataframe), ncol = nrow(dataframe))
  n_effective_control <- matrix(0L, nrow = nrow(dataframe), ncol = nrow(dataframe))
  
  cl <- makeSOCKcluster(n_clusters)
  registerDoSNOW(cl)
  tmp <- foreach(i=1:n_iter, .combine = '+', .packages=c('blockTools', 'igraph', 
                                                     'Matrix', 'dplyr'),
                 .multicombine= TRUE,
                 .maxcombine = 2) %dopar% {
    n_communities <- length(unique(clusters))
    #vertsInAssignments <- membership(clusters) %in% which(assignments == 1)
    assignments <- rep(0, n_communities)
    assignments[as.numeric(
      as.character(unlist(blockTools::assignment(blocks)[[1]][[1]]['Treatment 1'])))] <- 1
    vertsInAssignments <- clusters %in% which(assignments == 1)
    dataframe$cluster_membership <- clusters
    dataframe$pct_treated_neighbors <- sapply(1:vcount(graph), FUN=function(i){
      neigh <- neighbors(graph, i)
      pct <- sum(vertsInAssignments[neigh])/length(neigh)
      ifelse(is.nan(pct), assignments[dataframe$cluster_membership[i]], pct)
    })
    dataframe %>% mutate(
      effective_treatment = ifelse(assignments[cluster_membership] == 1 & pct_treated_neighbors >= threshold, 1, 0),
      effective_control = ifelse(assignments[cluster_membership] == 0 & pct_treated_neighbors <= (1-threshold), 1, 0)
    ) -> dataframe
    
    n_effective_treatment = dataframe$effective_treatment %o% dataframe$effective_treatment
    n_effective_control = dataframe$effective_control %o% dataframe$effective_control
    
    as(bdiag(n_effective_control, n_effective_treatment), "sparseMatrix")
  }
  stopCluster(cl)
  
  prob_effective_treatment <- tmp[1:nrow(dataframe), 1:nrow(dataframe)]/n_iter
  prob_effective_control <- tmp[(nrow(dataframe)+1):(2*nrow(dataframe)), (nrow(dataframe)+1):(2*nrow(dataframe))]/n_iter
  
  return(list(prob_effective_treatment, prob_effective_control))
}

mile_in_meters <- 1609.34

airbnb_listings$accommodates <- imputeMode(airbnb_listings$accommodates)
airbnb_listings$bedrooms <- imputeMode(airbnb_listings$bedrooms)
airbnb_listings$bathrooms <- imputeMode(airbnb_listings$bathrooms)
airbnb_listings$minstay[airbnb_listings$minstay > 30] <- 30
airbnb_listings$minstay <- imputeMode(airbnb_listings$minstay)
airbnb_listings$overall_satisfaction <- imputeMean(airbnb_listings$overall_satisfaction)

airbnb_listings$accommodates[is.na(airbnb_listings$accommodates)] <- Mode(airbnb_listings$accommodates)

distances <- matrix(, nrow=nrow(airbnb_listings), ncol=nrow(airbnb_listings))
room_types <- matrix(, nrow=nrow(airbnb_listings), ncol = nrow(airbnb_listings))
accommodates <- matrix(, nrow=nrow(airbnb_listings), ncol = nrow(airbnb_listings))


for (i in 1:nrow(airbnb_listings)) {
  for (j in i:nrow(airbnb_listings)) {
    distances[i, j] <- distHaversine(c(airbnb_listings$longitude[i], 
                                       airbnb_listings$latitude[i]), 
                                     c(airbnb_listings$longitude[j], airbnb_listings$latitude[j]))
    distances[j, i] <- distances[i, j]
    room_types[i, j] <- ifelse(airbnb_listings$room_type[i] == airbnb_listings$room_type[j], 1, 0)
    room_types[j, i] <- room_types[i, j]
    accommodates[i, j] <- ifelse(abs(airbnb_listings$accommodates[i] - airbnb_listings$accommodates[j]) <= 1, 1, 0)
    accommodates[j, i] <- accommodates[i, j]
  }
  print(i)
}

save(accommodates, distances, room_types, file='parsed_data.Rdata')

### Create graph and communities for distance only graph
distances_binary <- ifelse(distances <= mile_in_meters, 1, 0)
graph_distance_only <- graph_from_adjacency_matrix(distances_binary, mode = 'undirected', diag = FALSE)
clusters_distance_only <- membership(cluster_louvain(graph_distance_only))

### Create graph and communities for distance plus same room type graph
distances_plus_same_type <- distances_binary + room_types
distances_plus_same_type_binary <- ifelse(distances_plus_same_type == 2, 1, 0)
graph_distance_room_type <- graph_from_adjacency_matrix(distances_plus_same_type_binary, mode='undirected',
                                                        diag = FALSE)
clusters_distance_room_type <- membership(cluster_louvain(graph_distance_room_type))

### Create graph and communities for distance plus same room type plus same size graph
distances_plus_same_type_plus_accom <- distances_plus_same_type + accommodates
distances_plus_same_type_plus_accom_binary <- ifelse(distances_plus_same_type_plus_accom == 3, 1, 0)
graph_distance_room_type_accom <- graph_from_adjacency_matrix(distances_plus_same_type_plus_accom_binary, 
                                                              mode='undirected', diag = FALSE)
clusters_distance_room_type_accom <- membership(cluster_louvain(graph_distance_room_type_accom))

clusters_distance_room_type_accom_fg <- cluster_fast_greedy(graph_distance_room_type_accom)


clusters_distance_room_type_accom_100 <- cutree(as.hclust(clusters_distance_room_type_accom_fg), k=100)
clusters_distance_room_type_accom_200 <- cutree(as.hclust(clusters_distance_room_type_accom_fg), k=200)
clusters_distance_room_type_accom_500 <- cutree(as.hclust(clusters_distance_room_type_accom_fg), k=500)
clusters_distance_room_type_accom_1000 <- cutree(as.hclust(clusters_distance_room_type_accom_fg), k=1000)

graph_distance_room_type_accom_rw_01 <- rewire(graph_distance_room_type_accom, each_edge(prob = 0.01))
graph_distance_room_type_accom_rw_02 <- rewire(graph_distance_room_type_accom, each_edge(prob = 0.02))
graph_distance_room_type_accom_rw_05 <- rewire(graph_distance_room_type_accom, each_edge(prob = 0.05))
graph_distance_room_type_accom_rw_10 <- rewire(graph_distance_room_type_accom, each_edge(prob = 0.1))
graph_distance_room_type_accom_rw_15 <- rewire(graph_distance_room_type_accom, each_edge(prob = 0.15))

clusters_distance_room_type_accom_rw_01 <- membership(cluster_louvain(graph_distance_room_type_accom_rw_01))
clusters_distance_room_type_accom_rw_02 <- membership(cluster_louvain(graph_distance_room_type_accom_rw_02))
clusters_distance_room_type_accom_rw_05 <- membership(cluster_louvain(graph_distance_room_type_accom_rw_05))
clusters_distance_room_type_accom_rw_10 <- membership(cluster_louvain(graph_distance_room_type_accom_rw_10))
clusters_distance_room_type_accom_rw_15 <- membership(cluster_louvain(graph_distance_room_type_accom_rw_15))

airbnb_listings %>%
  mutate(`Entire home/apt` = ifelse(as.character(room_type) == 'Entire home/apt', 1, 0),
         `Private room` = ifelse(as.character(room_type) == 'Private room', 1, 0),
         `Shared room` = ifelse(as.character(room_type) == 'Shared room', 1, 0)) %>%
  dplyr::select(room_id, host_id, reviews, overall_satisfaction, 
                accommodates, bedrooms, bathrooms, price, minstay, latitude, longitude,
                `Entire home/apt`, `Private room`, `Shared room`) -> airbnb_listings

airbnb_listings %>% 
  mutate(reviews_scaled = as.numeric(scale(reviews, scale=TRUE)),
         satisfaction_scaled = as.numeric(scale(overall_satisfaction, scale=TRUE)),
         accomm_scaled = as.numeric(scale(overall_satisfaction, scale=TRUE)),
         bed_scaled = as.numeric(scale(bedrooms, scale=TRUE)),
         bath_scaled = as.numeric(scale(bathrooms, scale=TRUE)),
         minstay_scaled = as.numeric(scale(minstay, scale=TRUE))) -> airbnb_listings

df_for_blocking <- airbnb_listings
df_for_blocking$cluster_assignment <- clusters_distance_room_type_accom
df_for_blocking$cluster_assignment_100 <- clusters_distance_room_type_accom_100
df_for_blocking$cluster_assignment_200 <- clusters_distance_room_type_accom_200
df_for_blocking$cluster_assignment_500 <- clusters_distance_room_type_accom_500
df_for_blocking$cluster_assignment_1000 <- clusters_distance_room_type_accom_1000
df_for_blocking$cluster_assignment_rw_01 <- clusters_distance_room_type_accom_rw_01
df_for_blocking$cluster_assignment_rw_02 <- clusters_distance_room_type_accom_rw_02
df_for_blocking$cluster_assignment_rw_05 <- clusters_distance_room_type_accom_rw_05
df_for_blocking$cluster_assignment_rw_10 <- clusters_distance_room_type_accom_rw_10
df_for_blocking$cluster_assignment_rw_15 <- clusters_distance_room_type_accom_rw_15

df_for_blocking_base <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment'))
df_for_blocking_base_100 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_100'))
df_for_blocking_base_200 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_200'))
df_for_blocking_base_500 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_500'))
df_for_blocking_base_1000 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_1000'))
df_for_blocking_base_rw_01 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_rw_01'))
df_for_blocking_base_rw_02 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_rw_02'))
df_for_blocking_base_rw_05 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_rw_05'))
df_for_blocking_base_rw_10 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_rw_10'))
df_for_blocking_base_rw_15 <- create_df_for_blocking(df_for_blocking, sym('cluster_assignment_rw_15'))

save(df_for_blocking_base, df_for_blocking_base_100, df_for_blocking_base_200,
     df_for_blocking_base_500, df_for_blocking_base_1000, df_for_blocking_base_rw_01,
     df_for_blocking_base_rw_02, df_for_blocking_base_rw_05, df_for_blocking_base_rw_10,
     df_for_blocking_base_rw_15, file='df_for_blocking.Rdata')

blocks_base <- generate_blocks(df_for_blocking_base, 'cluster_assignment')
blocks_base_100 <- generate_blocks(df_for_blocking_base_100, 'cluster_assignment_100')
blocks_base_200 <- generate_blocks(df_for_blocking_base_200, 'cluster_assignment_200')
blocks_base_500 <- generate_blocks(df_for_blocking_base_500, 'cluster_assignment_500')
blocks_base_1000 <- generate_blocks(df_for_blocking_base_1000, 'cluster_assignment_1000')
blocks_base_rw_01 <- generate_blocks(df_for_blocking_base_rw_01, 'cluster_assignment_rw_01')
blocks_base_rw_02 <- generate_blocks(df_for_blocking_base_rw_02, 'cluster_assignment_rw_02')
blocks_base_rw_05 <- generate_blocks(df_for_blocking_base_rw_05, 'cluster_assignment_rw_05')
blocks_base_rw_10 <- generate_blocks(df_for_blocking_base_rw_10, 'cluster_assignment_rw_10')
blocks_base_rw_15 <- generate_blocks(df_for_blocking_base_rw_15, 'cluster_assignment_rw_15')

hajek_probabilities_75_50_drta_base <- hajek_probabilities(airbnb_listings, clusters_distance_room_type_accom, 
                                                 graph_distance_room_type_accom, .75, .5, 25, 3, blocks_base)

hajek_probabilities_50_50_drta <- hajek_probabilities(airbnb_listings, clusters_distance_room_type_accom, 
                                                      graph_distance_room_type_accom, .5, .5, 100, blocks)

hajek_probabilities_95_50_drta <- hajek_probabilities(airbnb_listings, clusters_distance_room_type_accom, 
                                                      graph_distance_room_type_accom, .95, .5, 100, blocks)

save(hajek_probabilities_50_50_drta, hajek_probabilities_75_50_drta, hajek_probabilities_95_50_drta, file='hajek_probabilities_blocked.Rdata')

hajek_probabilities_50_50_drta[[1]] <- ifelse(is.na(hajek_probabilities_50_50_drta[[1]]), 0, hajek_probabilities_50_50_drta[[1]])
hajek_probabilities_50_50_drta[[2]] <- ifelse(is.na(hajek_probabilities_50_50_drta[[2]]), 0, hajek_probabilities_50_50_drta[[2]])

hajek_probabilities_75_50_drta[[1]] <- ifelse(is.na(hajek_probabilities_50_50_drta[[1]]), 0, hajek_probabilities_50_50_drta[[1]])
hajek_probabilities_75_50_drta[[2]] <- ifelse(is.na(hajek_probabilities_50_50_drta[[2]]), 0, hajek_probabilities_50_50_drta[[2]])

hajek_probabilities_95_50_drta[[1]] <- ifelse(is.na(hajek_probabilities_50_50_drta[[1]]), 0, hajek_probabilities_50_50_drta[[1]])
hajek_probabilities_95_50_drta[[2]] <- ifelse(is.na(hajek_probabilities_50_50_drta[[2]]), 0, hajek_probabilities_50_50_drta[[2]])

airbnb_listings$prob_treat_50_thresh <- diag(hajek_probabilities_50_50_drta[[1]])
airbnb_listings$prob_control_50_thresh <- diag(hajek_probabilities_50_50_drta[[2]])

airbnb_listings$prob_treat_75_thresh <- diag(hajek_probabilities_75_50_drta[[1]])
airbnb_listings$prob_control_75_thresh <- diag(hajek_probabilities_75_50_drta[[2]])

airbnb_listings$prob_treat_95_thresh <- diag(hajek_probabilities_95_50_drta[[1]])
airbnb_listings$prob_control_95_thresh <- diag(hajek_probabilities_95_50_drta[[2]])

save(airbnb_listings, clusters_distance_room_type_accom, graph_distance_room_type_accom, 
     clusters_distance_room_type, graph_distance_room_type, graph_distance_only, clusters_distance_only,
     blocks, df_for_blocking, 
     file = 'data_for_simulation_blocked.Rdata')

#hajek_probabilities_75_50_drt <- hajek_probabilities(airbnb_listings, clusters_distance_room_type, 
#                                                      graph_distance_room_type, .75, .5, 100)
#
#hajek_probabilities_50_50_drt <- hajek_probabilities(airbnb_listings, clusters_distance_room_type, 
#                                                      graph_distance_room_type, .5, .5, 100)
#
#hajek_probabilities_95_50_drt <- hajek_probabilities(airbnb_listings, clusters_distance_room_type, 
#                                                      graph_distance_room_type, .95, .5, 100)
#
#hajek_probabilities_75_50_d <- hajek_probabilities(airbnb_listings, clusters_distance_only, 
#                                                     graph_distance_only, .75, .5, 100)
#
#hajek_probabilities_50_50_d <- hajek_probabilities(airbnb_listings, clusters_distance_only, 
#                                                     graph_distance_only, .5, .5, 100)
#
#hajek_probabilities_95_50_d <- hajek_probabilities(airbnb_listings, clusters_distance_only, 
#                                                     graph_distance_only, .95, .5, 100)#