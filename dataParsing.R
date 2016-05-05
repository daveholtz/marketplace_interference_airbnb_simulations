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
  mutate(reviews_scaled = scale(reviews, scale=TRUE),
         satisfaction_scaled = scale(overall_satisfaction, scale=TRUE),
         accomm_scaled = scale(overall_satisfaction, scale=TRUE),
         bed_scaled = scale(bedrooms, scale=TRUE),
         bath_scaled = scale(bathrooms, scale=TRUE),
         minstay_scaled = scale(minstay, scale=TRUE)) -> airbnb_listings

save(airbnb_listings, clusters_distance_room_type_accom, graph_distance_room_type_accom, 
     clusters_distance_room_type, graph_distance_room_type, graph_distance_only, clusters_distance_only,
     file = 'data_for_simulation.Rdata')