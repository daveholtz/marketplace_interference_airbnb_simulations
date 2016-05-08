library(dplyr)
library(igraph)
library(foreach)
library(doMC)
registerDoMC(4)
setwd('~/Desktop/mit_15840_paper/')
load('data_for_simulation.Rdata')

set.seed(15840)

pct_treated_neighbors <- function(graph, vertex, community, assignments) { 
  sum(
    ifelse(
      assignments[membership(community)[neighbors(graph, vertex)]] == 1,
      1, 0)
    )/length(neighbors(graph, vertex))
  }

simulate_graph_randomization <- function(data, pct, effect_size, community, graph) {
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
  
  data$pct_treated_neighbors <- sapply(1:nrow(data), 
                                       pct_treated_neighbors, 
                                       graph = graph, community = community, assignments = assignments)
  
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
    print(j)
  }
  
  treatment_booked <- nrow(filter(booked_rooms, assignments[cluster_membership] == 1))
  treatment_unbooked <- nrow(filter(data, assignments[cluster_membership] == 1))
  treatment_revenue <- sum(filter(booked_rooms, assignments[cluster_membership] == 1)$price)
  control_booked <- nrow(filter(booked_rooms, assignments[cluster_membership] == 0))
  control_unbooked <- nrow(filter(data, assignments[cluster_membership] == 0))
  control_revenue <- sum(filter(booked_rooms, assignments[cluster_membership] == 0)$price)
  
  eff_treatment_booked_75 <- nrow(filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                        pct_treated_neighbors >= .75))
  eff_treatment_unbooked_75 <- nrow(filter(data, assignments[cluster_membership] == 1 & 
                                          pct_treated_neighbors >= .75))
  eff_treatment_revenue_75 <- sum(filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                        pct_treated_neighbors >= .75)$price)
  eff_control_booked_75 <- nrow(filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                        (1-pct_treated_neighbors) >= .75))
  eff_control_unbooked_75 <- nrow(filter(data, assignments[cluster_membership] == 0 & 
                                          (1-pct_treated_neighbors) >= .75))
  eff_control_revenue_75 <- sum(filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                        (1-pct_treated_neighbors) >= .75)$price)
  
  eff_treatment_booked_50 <- nrow(filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                           pct_treated_neighbors >= .5))
  eff_treatment_unbooked_50 <- nrow(filter(data, assignments[cluster_membership] == 1 & 
                                             pct_treated_neighbors >= .5))
  eff_treatment_revenue_50 <- sum(filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                           pct_treated_neighbors >= .5)$price)
  eff_control_booked_50 <- nrow(filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                         (1-pct_treated_neighbors) >= .5))
  eff_control_unbooked_50 <- nrow(filter(data, assignments[cluster_membership] == 0 & 
                                           (1-pct_treated_neighbors) >= .5))
  eff_control_revenue_50 <- sum(filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                         (1-pct_treated_neighbors) >= .5)$price)
  
  eff_treatment_booked_95 <- nrow(filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                           pct_treated_neighbors >= .95))
  eff_treatment_unbooked_95 <- nrow(filter(data, assignments[cluster_membership] == 1 & 
                                             pct_treated_neighbors >= .95))
  eff_treatment_revenue_95 <- sum(filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                           pct_treated_neighbors >= .95)$price)
  eff_control_booked_95 <- nrow(filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                         (1-pct_treated_neighbors) >= .95))
  eff_control_unbooked_95 <- nrow(filter(data, assignments[cluster_membership] == 0 & 
                                           (1-pct_treated_neighbors) >= .95))
  eff_control_revenue_95 <- sum(filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                         (1-pct_treated_neighbors) >= .95)$price)
  
  
  
  p_value <- ifelse(pct %in% c(0,1), NA, prop.test(c(treatment_booked, control_booked), 
                                                     c(treatment_booked + treatment_unbooked, 
                                                       control_booked + control_unbooked))$p.value)
  eff_p_value_75 <- ifelse(pct %in% c(0,1), NA, prop.test(c(eff_treatment_booked_75, eff_control_booked_75), 
                                                       c(eff_treatment_booked_75 + eff_treatment_unbooked_75, 
                                                         eff_control_booked_75 + eff_control_unbooked_75))$p.value)
  eff_p_value_50 <- ifelse(pct %in% c(0,1), NA, prop.test(c(eff_treatment_booked_50, eff_control_booked_50), 
                                                          c(eff_treatment_booked_50 + eff_treatment_unbooked_50, 
                                                            eff_control_booked_50 + eff_control_unbooked_50))$p.value)
  eff_p_value_95 <- ifelse(pct %in% c(0,1), NA, prop.test(c(eff_treatment_booked_95, eff_control_booked_95), 
                                                          c(eff_treatment_booked_95 + eff_treatment_unbooked_95, 
                                                            eff_control_booked_95 + eff_control_unbooked_95))$p.value)
  
  hajek_estimator_75 <- sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                     pct_treated_neighbors >= .75 & prob_treat_75_thresh != 0)$prob_treat_75_thresh)/(
                                       sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                                      pct_treated_neighbors >= .75 & 
                                                      prob_treat_75_thresh != 0)$prob_treat_75_thresh) + 
                                         sum(1/filter(data, assignments[cluster_membership] == 1 & 
                                                        pct_treated_neighbors >= .75 & 
                                                        prob_treat_75_thresh != 0)$prob_treat_75_thresh)
                                     ) - sum(1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                        (1-pct_treated_neighbors) >= .75 & 
                                                        prob_control_75_thresh != 0)$prob_control_75_thresh)/(sum(
                                                          1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                                         (1-pct_treated_neighbors) >= .75 & 
                                                                         prob_control_75_thresh != 0)$prob_control_75_thresh) + 
                                                            sum(1/filter(data, assignments[cluster_membership] == 0 & 
                                                                           (1-pct_treated_neighbors) >= .75 & 
                                                                           prob_control_75_thresh != 0)$prob_control_75_thresh))
  
  hajek_estimator_50 <- sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                       pct_treated_neighbors >= .5 & prob_treat_50_thresh != 0)$prob_treat_50_thresh)/(
                                         sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                                        pct_treated_neighbors >= .5 & 
                                                        prob_treat_50_thresh != 0)$prob_treat_50_thresh) + 
                                           sum(1/filter(data, assignments[cluster_membership] == 1 & 
                                                          pct_treated_neighbors >= .5 & 
                                                          prob_treat_50_thresh != 0)$prob_treat_50_thresh)
                                       ) - sum(1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                          (1-pct_treated_neighbors) >= .5 & 
                                                          prob_control_50_thresh != 0)$prob_control_50_thresh)/(sum(
                                                            1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                                       (1-pct_treated_neighbors) >= .5 & 
                                                                       prob_control_50_thresh != 0)$prob_control_50_thresh) + 
                                                              sum(1/filter(data, assignments[cluster_membership] == 0 & 
                                                                             (1-pct_treated_neighbors) >= .5 & 
                                                                             prob_control_50_thresh != 0)$prob_control_50_thresh))
  
  hajek_estimator_95 <- sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                       pct_treated_neighbors >= .95 & prob_treat_95_thresh != 0)$prob_treat_95_thresh)/(
                                         sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                                        pct_treated_neighbors >= .95 & 
                                                        prob_treat_95_thresh != 0)$prob_treat_95_thresh) + 
                                           sum(1/filter(data, assignments[cluster_membership] == 1 & 
                                                          pct_treated_neighbors >= .95 & 
                                                          prob_treat_95_thresh != 0)$prob_treat_95_thresh)
                                       ) - sum(1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                          (1-pct_treated_neighbors) >= .95 & 
                                                          prob_control_95_thresh != 0)$prob_control_95_thresh)/(sum(
                                                            1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                                       (1-pct_treated_neighbors) >= .95 & 
                                                                       prob_control_95_thresh != 0)$prob_control_95_thresh) + 
                                                              sum(1/filter(data, assignments[cluster_membership] == 0 & 
                                                                             (1-pct_treated_neighbors) >= .95 & 
                                                                             prob_control_95_thresh != 0)$prob_control_95_thresh))
  
  price_hajek_estimator_75 <- sum((filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                            pct_treated_neighbors >= .75 & 
                                            prob_treat_75_thresh != 0)$price)*(1/filter(booked_rooms, 
                                                                                        assignments[cluster_membership] == 1 &
                                            pct_treated_neighbors >= .75 & 
                                              prob_treat_75_thresh != 0)$prob_treat_75_thresh))/(
                                         sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                                        pct_treated_neighbors >= .75 & 
                                                        prob_treat_75_thresh != 0)$prob_treat_75_thresh) + 
                                           sum(1/filter(data, assignments[cluster_membership] == 1 & 
                                                          pct_treated_neighbors >= .75 & 
                                                          prob_treat_75_thresh != 0)$prob_treat_75_thresh)
                                       ) - sum((filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                         (1-pct_treated_neighbors) >= .75 & 
                                                         prob_control_75_thresh != 0)$price)*(1/filter(booked_rooms, 
                                                                assignments[cluster_membership] == 0 &
                                                                  (1-pct_treated_neighbors) >= .75 & 
                                                                  prob_control_75_thresh != 0)$prob_control_75_thresh))/(sum(
                                                            1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                                       (1-pct_treated_neighbors) >= .75 & 
                                                                       prob_control_75_thresh != 0)$prob_control_75_thresh) + 
                                                              sum(1/filter(data, assignments[cluster_membership] == 0 & 
                                                        (1-pct_treated_neighbors) >= .75 & 
                                                        prob_control_75_thresh != 0)$prob_control_75_thresh))
  
  price_hajek_estimator_50 <- sum((filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                            pct_treated_neighbors >= .5 & 
                                            prob_treat_50_thresh != 0)$price)*(1/filter(booked_rooms, 
                                  assignments[cluster_membership] == 1 &
                                    pct_treated_neighbors >= .5 & 
                                    prob_treat_50_thresh != 0)$prob_treat_50_thresh))/(
                                      sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                                     pct_treated_neighbors >= .5 & 
                                                     prob_treat_50_thresh != 0)$prob_treat_5_thresh) + 
                                        sum(1/filter(data, assignments[cluster_membership] == 1 & 
                                                       pct_treated_neighbors >= .5 & 
                                                       prob_treat_50_thresh != 0)$prob_treat_50_thresh)
                                    ) - sum((filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                      (1-pct_treated_neighbors) >= .5 & 
                                                      prob_control_50_thresh != 0)$price)*(1/filter(booked_rooms, 
                                       assignments[cluster_membership] == 0 &
                                         (1-pct_treated_neighbors) >= .5 & 
                                         prob_control_50_thresh != 0)$prob_control_50_thresh))/(sum(
                                           1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                                      (1-pct_treated_neighbors) >= .5 & 
                                                      prob_control_50_thresh != 0)$prob_control_50_thresh) + 
                                             sum(1/filter(data, assignments[cluster_membership] == 0 & 
                                                            (1-pct_treated_neighbors) >= .5 & 
                                                   prob_control_50_thresh != 0)$prob_control_50_thresh))  
  
  price_hajek_estimator_95 <- sum((filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                            pct_treated_neighbors >= .95 & 
                                            prob_treat_95_thresh != 0)$price)*(1/filter(booked_rooms, 
                assignments[cluster_membership] == 1 &
                  pct_treated_neighbors >= .95 & 
                  prob_treat_95_thresh != 0)$prob_treat_95_thresh))/(
                    sum(1/filter(booked_rooms, assignments[cluster_membership] == 1 & 
                                   pct_treated_neighbors >= .95 & 
                                   prob_treat_95_thresh != 0)$prob_treat_95_thresh) + 
                      sum(1/filter(data, assignments[cluster_membership] == 1 & 
                                     pct_treated_neighbors >= .95 & 
                                     prob_treat_95_thresh != 0)$prob_treat_95_thresh)
                  ) - sum((filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                    (1-pct_treated_neighbors) >= .95 & 
                                    prob_control_95_thresh != 0)$price)*(1/filter(booked_rooms, 
                  assignments[cluster_membership] == 0 &
                    (1-pct_treated_neighbors) >= .95 & 
                   prob_control_95_thresh != 0)$prob_control_95_thresh))/(sum(
                     1/filter(booked_rooms, assignments[cluster_membership] == 0 & 
                                (1-pct_treated_neighbors) >= .95 & 
                                prob_control_95_thresh != 0)$prob_control_95_thresh) + 
                       sum(1/filter(data, assignments[cluster_membership] == 0 & 
                       (1-pct_treated_neighbors) >= .95 & 
                         prob_control_95_thresh != 0)$prob_control_95_thresh))                                                            
  
  
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
             p_value = p_value,
             eff_treatment_booked_75 = eff_treatment_booked_75, 
             eff_treatment_unbooked_75 = eff_treatment_unbooked_75, 
             eff_treatment_booking_rate_75 = eff_treatment_booked_75/(eff_treatment_unbooked_75 + eff_treatment_booked_75),
             eff_treatment_revenue_75 = eff_treatment_revenue_75,
             eff_treatment_revenue_per_user_75 = eff_treatment_revenue_75/(eff_treatment_unbooked_75 + eff_treatment_booked_75),
             eff_control_booked_75 = eff_control_booked_75,
             eff_control_unbooked_75 = eff_control_unbooked_75,
             eff_control_booking_rate_75 = eff_control_booked_75/(eff_control_unbooked_75 + eff_control_booked_75),
             eff_control_revenue_75 = eff_control_revenue_75,
             eff_control_revenue_per_user_75 = eff_control_revenue_75/(eff_control_unbooked_75 + eff_control_booked_75),
             eff_p_value_75 = eff_p_value_75,
             eff_treatment_booked_50 = eff_treatment_booked_50, 
             eff_treatment_unbooked_50 = eff_treatment_unbooked_50, 
             eff_treatment_booking_rate_50 = eff_treatment_booked_50/(eff_treatment_unbooked_50 + eff_treatment_booked_50),
             eff_treatment_revenue_50 = eff_treatment_revenue_50,
             eff_treatment_revenue_per_user_50 = eff_treatment_revenue_50/(eff_treatment_unbooked_50 + eff_treatment_booked_50),
             eff_control_booked_50 = eff_control_booked_50,
             eff_control_unbooked_50 = eff_control_unbooked_50,
             eff_control_booking_rate_50 = eff_control_booked_50/(eff_control_unbooked_50 + eff_control_booked_50),
             eff_control_revenue_50 = eff_control_revenue_50,
             eff_control_revenue_per_user_50 = eff_control_revenue_50/(eff_control_unbooked_50 + eff_control_booked_50),
             eff_p_value_50 = eff_p_value_50,
             eff_treatment_booked_95 = eff_treatment_booked_95, 
             eff_treatment_unbooked_95 = eff_treatment_unbooked_95, 
             eff_treatment_booking_rate_95 = eff_treatment_booked_95/(eff_treatment_unbooked_95 + eff_treatment_booked_95),
             eff_treatment_revenue_95 = eff_treatment_revenue_95,
             eff_treatment_revenue_per_user_95 = eff_treatment_revenue_95/(eff_treatment_unbooked_95 + eff_treatment_booked_95),
             eff_control_booked_95 = eff_control_booked_95,
             eff_control_unbooked_95 = eff_control_unbooked_95,
             eff_control_booking_rate_95 = eff_control_booked_95/(eff_control_unbooked_95 + eff_control_booked_95),
             eff_control_revenue_95 = eff_control_revenue_95,
             eff_control_revenue_per_user_95 = eff_control_revenue_95/(eff_control_unbooked_95 + eff_control_booked_95),
             eff_p_value_95 = eff_p_value_95,
             hajek_estimator_75 = hajek_estimator_75,
             hajek_estimator_95 = hajek_estimator_95,
             hajek_estimator_50 = hajek_estimator_50,
             price_hajek_estimator_75 = price_hajek_estimator_75,
             price_hajek_estimator_95 = price_hajek_estimator_95,
             price_hajek_estimator_50 = price_hajek_estimator_50
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

treatment_reality <- foreach(i = 1:1000, .combine = rbind) %dopar% simulate_ind_randomization(data=airbnb_listings, 
                                                                                              pct=1, effect_size = -.5) 
control_reality <- foreach(i = 1:1000, .combine = rbind) %dopar% simulate_ind_randomization(data=airbnb_listings, 
                                                                                            pct=0, effect_size = -.5)
save(control_reality, treatment_reality, file='baseline_distributions.Rdata')

load('baseline_distributions.Rdata')

set.seed(15840)
treatment_50_cluster_drta <- foreach(i = 1:1000, .combine=rbind) %dopar% simulate_graph_randomization(
  data = airbnb_listings, pct=.5, effect_size = -.5, community=clusters_distance_room_type_accom, 
  graph = graph_distance_room_type_accom)
save(treatment_50_cluster_drta, file='cluster_performance_50_pct_negative_50_effect.Rdata')

set.seed(15840)
treatment_50_ind_drta <- foreach(i = 1:1000, .combine = rbind) %dopar% simulate_ind_randomization(data=airbnb_listings,
                                                                                                  pct=.5, effect_size = -.5)
