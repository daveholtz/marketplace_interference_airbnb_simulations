library(dplyr)
library(igraph)
library(foreach)
library(doMC)
registerDoMC(4)

set.seed(15840)

simulate_graph_randomization <- function(data, pct, effect_size, community) {
  original_data <- data
  n_communities <- length(community)
  assignments <- rbinom(n_communities, 1, pct)
  data$cluster_membership <- membership(community)
  search_weights <- prop.table(runif(6))
  data %>% 
    mutate(price = ifelse(assignments[cluster_membership] == 1, price*(1+effect_size), price)) %>%
    mutate(price_scaled = as.numeric(scale(price, scale=TRUE))) %>%
    mutate(search_quality = search_weights[1]*reviews_scaled + search_weights[2]*satisfaction_scaled + 
             search_weights[3]*bed_scaled + search_weights[4]*bath_scaled - 
             search_weights[5]*minstay_scaled - search_weights[6]*price_scaled) -> data
  
  max_lon <- max(data$longitude)
  max_lat <- max(data$latitude)
  min_lat <- min(data$latitude)
  min_lon <- min(data$longitude)
  min_acc <- min(data$accommodates)
  #max_acc <- max(data$accommodates)
  max_acc <- 4
  
  booked_rooms <- filter(data, room_id == -1)
  
  for (j in 1:1000) {
    min_search_lat <- runif(1, min=min_lat, max=max_lat)
    max_search_lat <- runif(1, min=min_search_lat, max=max_lat)
    min_search_lon <- runif(1, min=min_lon, max=max_lon)
    max_search_lon <- runif(1, min=min_search_lon, max=max_lon)
    min_search_acc <- floor(runif(1, min=min_acc, max=max_acc))
    search_room_type <- floor(runif(1, min=1, max=4))
    searcher_weights <- prop.table(runif(6))
    
    data %>%
      filter(latitude >= min_search_lat & latitude <= max_search_lat &
               longitude >= min_search_lon & longitude <= max_search_lon & 
               accommodates >= min_search_acc & 
               ((search_room_type == 1 & `Entire home/apt` == 1) | 
                  (search_room_type == 2 & `Private room` == 1) | 
                  (search_room_type == 3 & `Shared room` == 1))) %>%
      arrange(desc(search_quality)) %>% 
      head(n=10) %>%
      mutate(searcher_quality = searcher_weights[1]*reviews_scaled + searcher_weights[2]*satisfaction_scaled + 
               searcher_weights[3]*bed_scaled + searcher_weights[4]*bath_scaled - 
               searcher_weights[5]*minstay_scaled - searcher_weights[6]*price_scaled) %>%
      arrange(desc(searcher_quality)) %>%
      head(n=1) -> searcher_selection
    
    if (nrow(searcher_selection) == 1) {
      booked_rooms <- rbind(booked_rooms, searcher_selection)
      data <- filter(data, room_id != searcher_selection$room_id[1])
    }
  }
  
  treatment_booked <- nrow(filter(booked_rooms, assignments[cluster_membership] == 1))
  treatment_unbooked <- nrow(filter(data, assignments[cluster_membership] == 1))
  treatment_revenue <- sum(filter(booked_rooms, assignments[cluster_membership] == 1)$price)
  control_booked <- nrow(filter(booked_rooms, assignments[cluster_membership] == 0))
  control_unbooked <- nrow(filter(data, assignments[cluster_membership] == 0))
  control_revenue <- sum(filter(booked_rooms, assignments[cluster_membership] == 0)$price)
  p_value <- ifelse(pct %in% c(0,1), NA, prop.test(c(treatment_booked, control_booked), 
                                                     c(treatment_booked + treatment_unbooked, 
                                                       control_booked + control_unbooked))$p.value)
  
  data.frame(treatment_booked = treatment_booked, 
             treatment_unbooked = treatment_unbooked, 
             treatment_booking_rate = treatment_booked/(treatment_unbooked + treatment_booked),
             treatment_revenue = treatment_revenue,
             treatment_revenue_per_user = treatment_revenue/(treatment_unbooked + treatment_booked),
             control_booked = control_booked,
             control_unbooked = control_unbooked,
             control_booking_rate = control_booked/(control_unbooked + control_booked),
             control_revenue = control_revenue,
             control_revenue_per_user = control_revenue/(control_unbooked + control_booked),
             p_value = p_value
             )
}

simulate_ind_randomization <- function(data, pct, effect_size) {
  original_data <- data
  data$assignment <- rbinom(nrow(data), 1, pct)
  search_weights <- prop.table(runif(6))
  data %>% 
    mutate(price = ifelse(assignment == 1, price*(1+effect_size), price)) %>%
    mutate(price_scaled = as.numeric(scale(price, scale=TRUE))) %>%
    mutate(search_quality = search_weights[1]*reviews_scaled + search_weights[2]*satisfaction_scaled + 
             search_weights[3]*bed_scaled + search_weights[4]*bath_scaled - 
             search_weights[5]*minstay_scaled - search_weights[6]*price_scaled) -> data
  
  max_lon <- max(data$longitude)
  max_lat <- max(data$latitude)
  min_lat <- min(data$latitude)
  min_lon <- min(data$longitude)
  min_acc <- min(data$accommodates)
  #max_acc <- max(data$accommodates)
  max_acc <- 4
  
  booked_rooms <- filter(data, room_id == -1)
  
  for (j in 1:1000) {
    min_search_lat <- runif(1, min=min_lat, max=max_lat)
    max_search_lat <- runif(1, min=min_search_lat, max=max_lat)
    min_search_lon <- runif(1, min=min_lon, max=max_lon)
    max_search_lon <- runif(1, min=min_search_lon, max=max_lon)
    min_search_acc <- floor(runif(1, min=min_acc, max=max_acc))
    search_room_type <- floor(runif(1, min=1, max=4))
    searcher_weights <- prop.table(runif(6))
    
    data %>%
      filter(latitude >= min_search_lat & latitude <= max_search_lat &
               longitude >= min_search_lon & longitude <= max_search_lon & 
               accommodates >= min_search_acc & 
               ((search_room_type == 1 & `Entire home/apt` == 1) | 
                  (search_room_type == 2 & `Private room` == 1) | 
                  (search_room_type == 3 & `Shared room` == 1))) %>%
      arrange(desc(search_quality)) %>% 
      head(n=10) %>%
      mutate(searcher_quality = searcher_weights[1]*reviews_scaled + searcher_weights[2]*satisfaction_scaled + 
               searcher_weights[3]*bed_scaled + searcher_weights[4]*bath_scaled - 
               searcher_weights[5]*minstay_scaled - searcher_weights[6]*price_scaled) %>%
      arrange(desc(searcher_quality)) %>%
      head(n=1) -> searcher_selection
    
    if (nrow(searcher_selection) == 1) {
      booked_rooms <- rbind(booked_rooms, searcher_selection)
      data <- filter(data, room_id != searcher_selection$room_id[1])
    }
  }
  
  treatment_booked <- nrow(filter(booked_rooms, assignment == 1))
  treatment_unbooked <- nrow(filter(data, assignment == 1))
  treatment_revenue <- sum(filter(booked_rooms, assignment == 1)$price)
  control_booked <- nrow(filter(booked_rooms, assignment == 0))
  control_unbooked <- nrow(filter(data, assignment == 0))
  control_revenue <- sum(filter(booked_rooms, assignment == 0)$price)
  
  p_value <- ifelse(pct %in% c(0,1), 'NA', prop.test(c(treatment_booked, control_booked), 
                                                     c(treatment_booked + treatment_unbooked, 
                                                       control_booked + control_unbooked))$p.value)
  
  data.frame(treatment_booked = treatment_booked, 
             treatment_unbooked = treatment_unbooked, 
             treatment_booking_rate = treatment_booked/(treatment_unbooked + treatment_booked),
             treatment_revenue = treatment_revenue,
             treatment_revenue_per_user = treatment_revenue/(treatment_unbooked + treatment_booked),
             control_booked = control_booked,
             control_unbooked = control_unbooked,
             control_booking_rate = control_booked/(control_unbooked + control_booked),
             control_revenue = control_revenue,
             control_revenue_per_user = control_revenue/(control_unbooked + control_booked),
             p_value = p_value
  )
}

c <- simulate_ind_randomization(data = airbnb_listings, pct = .5,
                                effect_size = -.5)

d <- simulate_graph_randomization(data=airbnb_listings, pct=.5,
                                  effect_size = -.5, community=clusters_distance_room_type_accom)

treatment_reality <- foreach(i = 1:1000, .combine = rbind) %dopar% simulate_ind_randomization(data=airbnb_listings, 
                                                                                              pct=1, effect_size = -.5) 

control_reality <- foreach(i = 1:1000, .combine = rbind) %dopar% simulate_ind_randomization(data=airbnb_listings, 
                                                                                            pct=0, effect_size = -.5)