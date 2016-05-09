library(geosphere)
library(igraph)
library(dplyr)

setwd('~/Desktop/mit_15840_paper/')
airbnb_listings <- read.csv('Airbnb_Listings_Miami.csv', row.names=1)

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

hajek_probabilities <- function(dataframe, clusters, graph, threshold, treatment_prob, n_iter) { 
  n_effective_treatment <- rep(0, nrow(dataframe))
  n_effective_control <- rep(0, nrow(dataframe))
  
  for (i in 1:n_iter) {
    n_communities <- length(clusters)
    assignments <- rbinom(n_communities, 1, treatment_prob)
    vertsInAssignments <- membership(clusters) %in% which(assignments == 1)
    dataframe$cluster_membership <- membership(clusters)
    dataframe$pct_treated_neighbors <- sapply(1:vcount(graph), FUN=function(i){
      neigh <- neighbors(graph, i)
      sum(vertsInAssignments[neigh])/length(neigh)
    })
    dataframe %>% mutate(
      effective_treatment = ifelse(assignments[cluster_membership] == 1 & pct_treated_neighbors >= threshold, 1, 0),
      effective_control = ifelse(assignments[cluster_membership] == 0 & pct_treated_neighbors <= (1-threshold), 1, 0)
    ) -> dataframe
    
    n_effective_treatment = n_effective_treatment + dataframe$effective_treatment
    n_effective_control = n_effective_control + dataframe$effective_control
    print(i)
  }
  
  prob_effective_treatment <- n_effective_treatment/n_iter
  prob_effective_control <- n_effective_control/n_iter
  
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
clusters_distance_only <- cluster_louvain(graph_distance_only)

### Create graph and communities for distance plus same room type graph
distances_plus_same_type <- distances_binary + room_types
distances_plus_same_type_binary <- ifelse(distances_plus_same_type == 2, 1, 0)
graph_distance_room_type <- graph_from_adjacency_matrix(distances_plus_same_type_binary, mode='undirected',
                                                        diag = FALSE)
clusters_distance_room_type <- cluster_louvain(graph_distance_room_type)

### Create graph and communities for distance plus same room type plus same size graph
distances_plus_same_type_plus_accom <- distances_plus_same_type + accommodates
distances_plus_same_type_plus_accom_binary <- ifelse(distances_plus_same_type_plus_accom == 3, 1, 0)
graph_distance_room_type_accom <- graph_from_adjacency_matrix(distances_plus_same_type_plus_accom_binary, 
                                                              mode='undirected', diag = FALSE)
clusters_distance_room_type_accom <- cluster_louvain(graph_distance_room_type_accom)

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

hajek_probabilities_75_50_drta <- hajek_probabilities(airbnb_listings, clusters_distance_room_type_accom, 
                                                 graph_distance_room_type_accom, .75, .5, 100)

hajek_probabilities_50_50_drta <- hajek_probabilities(airbnb_listings, clusters_distance_room_type_accom, 
                                                      graph_distance_room_type_accom, .5, .5, 100)

hajek_probabilities_95_50_drta <- hajek_probabilities(airbnb_listings, clusters_distance_room_type_accom, 
                                                      graph_distance_room_type_accom, .95, .5, 100)

save(hajek_probabilities_50_50_drta, hajek_probabilities_75_50_drta, hajek_probabilities_95_50_drta, file='hajek_probabilities.Rdata')

hajek_probabilities_50_50_drta[[1]][is.na(hajek_probabilities_50_50_drta[[1]])] <- 0
hajek_probabilities_50_50_drta[[2]][is.na(hajek_probabilities_50_50_drta[[2]])] <- 0

hajek_probabilities_75_50_drta[[1]][is.na(hajek_probabilities_75_50_drta[[1]])] <- 0
hajek_probabilities_75_50_drta[[2]][is.na(hajek_probabilities_75_50_drta[[2]])] <- 0

hajek_probabilities_95_50_drta[[1]][is.na(hajek_probabilities_95_50_drta[[1]])] <- 0
hajek_probabilities_95_50_drta[[2]][is.na(hajek_probabilities_95_50_drta[[2]])] <- 0

airbnb_listings$prob_treat_50_thresh <- hajek_probabilities_50_50_drta[[1]]
airbnb_listings$prob_control_50_thresh <- hajek_probabilities_50_50_drta[[2]]

airbnb_listings$prob_treat_75_thresh <- hajek_probabilities_75_50_drta[[1]]
airbnb_listings$prob_control_75_thresh <- hajek_probabilities_75_50_drta[[2]]

airbnb_listings$prob_treat_95_thresh <- hajek_probabilities_95_50_drta[[1]]
airbnb_listings$prob_control_95_thresh <- hajek_probabilities_95_50_drta[[2]]

save(airbnb_listings, clusters_distance_room_type_accom, graph_distance_room_type_accom, 
     clusters_distance_room_type, graph_distance_room_type, graph_distance_only, clusters_distance_only,
     file = 'data_for_simulation.Rdata')


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