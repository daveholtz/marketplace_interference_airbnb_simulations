library(ggplot2)
library(ggmap)
library(dplyr)
library(knitr)
library(scales)
setwd('~/Desktop/mit_15840_paper/')

airbnb_listings_for_raw <- read.csv('Airbnb_Listings_Miami.csv', row.names=1)

airbnb_listings_for_raw %>%
  mutate(`Room Type` = room_type) -> airbnb_listings_for_map

Miami <- get_map('Miami, FL', zoom=10)

miami_map <- ggmap(Miami)
miami_map <- miami_map + geom_point(data = airbnb_listings_for_map, aes(longitude, latitude,
                                                                        color=`Room Type`), 
                                    size=.3, alpha=.7) + 
  xlab('Longitude') + ylab('Latitude') + ggtitle('Spatial Distribution of Miami Airbnb Listings') + 
  theme(plot.title = element_text(size = rel(1.25)))
ggsave(miami_map, file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/listings_map.png')

airbnb_listings_for_raw %>% 
  dplyr::select(-collected, -room_id, -host_id, -borough, -neighborhood) -> airbnb_data_for_table

counteractual_minus50 <-ggplot(control_reality, aes(x=control_booking_rate, fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality, aes(x=treatment_booking_rate, fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=percent) + xlab('Booking Rate') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Distribution of Counterfactual Outcomes (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(counteractual_minus50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/counterfactual_minus50.png')

t.test(control_reality$control_booking_rate, treatment_reality$treatment_booking_rate)

ind_minus_50 <- ggplot(treatment_50_ind_drta, aes(x=treatment_booking_rate-control_booking_rate)) + 
  geom_histogram() + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Ind. Assignment and \n Diff. in Means (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(ind_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/ind_minus50.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

clust_minus_50 <- ggplot(treatment_50_cluster_drta, aes(x=treatment_booking_rate-control_booking_rate)) + 
  geom_histogram() + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Graph Cluster Randomization and \n Diff. in Means (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(clust_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/clust_minus_50.png')

### Compare the distributions of the Effective Treatment Diff in Means
### estimator for -50% price change and booking rate
cluster_effective_minus_50 <- ggplot(treatment_50_cluster_drta, 
                                 aes(x=eff_treatment_booking_rate_95-eff_control_booking_rate_95, 
                                     fill='95% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_50_cluster_drta,
                                    aes(x=eff_treatment_booking_rate_50-eff_control_booking_rate_50, 
                                        fill='50% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram(data=treatment_50_cluster_drta, 
                 aes(x=eff_treatment_booking_rate_75-eff_control_booking_rate_75, 
                     fill='75% Treated Neighbors Threshold'), alpha = .25) + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Graph Cluster Randomization\n and Eff. Treat. (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(cluster_effective_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/cluster_effective_minus_50.png')  

### Compare the distributions of the Hajek estimator for -50% price change and booking rate
cluster_hajek_minus_50 <- ggplot(treatment_50_cluster_drta, 
                                 aes(x=hajek_estimator_95, fill='95% Treated Neighbors Threshold'),
                                 alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_50_cluster_drta,
                                    aes(x=hajek_estimator_50, fill='50% Treated Neighbors Threshold'),
                                    alpha = .25) + 
  geom_histogram(data=treatment_50_cluster_drta, 
                 aes(x=hajek_estimator_75, fill='75% Treated Neighbors Threshold'),
                 alpha = .25) + scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Graph Cluster Randomization and\n Hajek (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(cluster_hajek_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/cluster_hajek_minus_50.png')  







price_counteractual_minus50 <- ggplot(control_reality, aes(x=control_revenue_per_user, 
                                                          fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality, aes(x=treatment_revenue_per_user, 
                                             fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=dollar) + xlab('Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Distribution of Counterfactual Outcomes (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_counteractual_minus50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_counterfactual_minus50.png')

t.test(control_reality$control_revenue_per_user, treatment_reality$treatment_revenue_per_user)

price_ind_minus_50 <- ggplot(treatment_50_ind_drta, aes(x=treatment_revenue_per_user-control_revenue_per_user)
                       ) + 
  geom_histogram() + 
  scale_x_continuous(labels=dollar) + xlab('Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Ind. Assignment and \n Diff. in Means (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_ind_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_ind_minus50.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

price_clust_minus_50 <- ggplot(treatment_50_cluster_drta, 
                               aes(x=treatment_revenue_per_user-control_revenue_per_user)) + 
  geom_histogram() + 
  scale_x_continuous(labels=dollar) + xlab('Difference in Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Graph Cluster Randomization and \n Diff. in Means (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_clust_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_clust_minus_50.png')

### Compare the distributions of the Effective Treatment Diff in Means
### estimator for -50% price change and booking rate
price_cluster_effective_minus_50 <- ggplot(treatment_50_cluster_drta, 
                                     aes(x=eff_treatment_revenue_per_user_95-
                                           eff_control_revenue_per_user_95, 
                                         fill='95% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_50_cluster_drta,
                                    aes(x=eff_treatment_revenue_per_user_50-
                                          eff_control_revenue_per_user_50, 
                                        fill='50% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram(data=treatment_50_cluster_drta, 
                 aes(x=eff_treatment_revenue_per_user_75-
                       eff_control_revenue_per_user_75, 
                     fill='75% Treated Neighbors Threshold'), alpha = .25) + 
  scale_x_continuous(labels=dollar) + xlab('Difference in Average Revenue Per Listing (USD)') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Graph Cluster Randomization\n and Eff. Treat. (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_cluster_effective_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_cluster_effective_minus_50.png')  

### Compare the distributions of the Hajek estimator for -50% price change and booking rate
price_cluster_hajek_minus_50 <- ggplot(treatment_50_cluster_drta, 
                                 aes(x=price_hajek_estimator_95, fill='95% Treated Neighbors Threshold'),
                                 alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_50_cluster_drta,
                                    aes(x=price_hajek_estimator_50, fill='50% Treated Neighbors Threshold'),
                                    alpha = .25) + 
  geom_histogram(data=treatment_50_cluster_drta, 
                 aes(x=price_hajek_estimator_75, fill='75% Treated Neighbors Threshold'),
                 alpha = .25) + scale_x_continuous(labels=percent) + 
  xlab('Difference in Average Revenue Per Listing (USD)') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('ATE with Graph Cluster Randomization and\n Hajek (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_cluster_hajek_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_cluster_hajek_minus_50.png')  

