library(ggplot2)
library(ggmap)
library(dplyr)
library(knitr)
library(scales)
setwd('~/Desktop/mit_15840_paper/')

load('data_for_simulation.Rdata')
load('baseline_distributions.Rdata')
load('cluster_performance_50_pct_negative_50_effect.Rdata')
load('ind_performance_50_pct_negative_50_effect.Rdata')
load('treatment_minus_20_distribution.Rdata')
load('ind_performance_50_pct_negative_20_effect.Rdata')
load('cluster_performance_50_pct_negative_20_effect.Rdata')

airbnb_listings_for_raw <- read.csv('Airbnb_Listings_Miami.csv', row.names=1)

airbnb_listings_for_raw %>%
  mutate(`Room Type` = room_type) -> airbnb_listings_for_map

Miami <- get_map('Miami, FL', zoom=10)

miami_map <- ggmap(Miami)
miami_map <- miami_map + geom_point(data = airbnb_listings_for_map, aes(longitude, latitude,
                                                                        color=`Room Type`), 
                                    size=.3, alpha=.7) + 
  xlab('Longitude') + ylab('Latitude') + ggtitle('Geospatial Distribution of Miami Airbnb Listings') + 
  theme(plot.title = element_text(size = rel(1.25)))
ggsave(miami_map, file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/listings_map.png')

airbnb_listings_for_raw %>% 
  dplyr::select(-collected, -room_id, -host_id, -borough, -neighborhood) -> airbnb_data_for_table

transitivity(graph_distance_room_type_accom)

counteractual_minus50 <-ggplot(control_reality, aes(x=control_booking_rate, fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality, aes(x=treatment_booking_rate, fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=percent) + xlab('Booking Rate') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Distribution of Counterfactual Outcomes \n Booking Rate (Effect Size = -50%)') + 
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
  ggtitle('Booking Rate ATE with Ind. Assignment and \n Diff. in Means (Effect Size = -50%)') + 
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
  ggtitle('Booking Rate ATE with Graph Cluster Randomization and \n Diff. in Means (Effect Size = -50%)') + 
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
  ggtitle('Booking Rate ATE with Graph Cluster Randomization\n and FNTR Eff. Treat. (Effect Size = -50%)') + 
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
  ggtitle('Booking Rate ATE with Graph Cluster Randomization and\n FNTR Hajek (Effect Size = -50%)') + 
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
  ggtitle('Distribution of Counterfactual Outcomes \n Average Revenue Per Listing (Effect Size = -50%)') + 
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
  ggtitle('Average Revenue Per Listing ATE with Ind. Assignment and \n Diff. in Means (Effect Size = -50%)') + 
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
  ggtitle('Average Revenue Per Listing ATE with Graph Cluster Randomization and \n Diff. in Means (Effect Size = -50%)') + 
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
  ggtitle('Average Revenue Per Listing ATE \nwith Graph Cluster Randomization\n and FNTR Eff. Treat. (Effect Size = -50%)') + 
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
  ggtitle('Average Revenue Per Listing ATE \n with Graph Cluster Randomization and\n FNTR Hajek (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_cluster_hajek_minus_50, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_cluster_hajek_minus_50.png')  

counteractual_minus20 <-ggplot(control_reality, aes(x=control_booking_rate, fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality_20, aes(x=treatment_booking_rate, fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=percent) + xlab('Booking Rate') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Distribution of Counterfactual Outcomes \n Booking Rate (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(counteractual_minus20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/counterfactual_minus20.png')

t.test(control_reality$control_booking_rate, treatment_reality_20$treatment_booking_rate)

ind_minus_20 <- ggplot(treatment_20_ind_drta, aes(x=treatment_booking_rate-control_booking_rate)) + 
  geom_histogram() + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Booking Rate ATE with Ind. Assignment and \n Diff. in Means (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(ind_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/ind_minus20.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

clust_minus_20 <- ggplot(treatment_20_cluster_drta, aes(x=treatment_booking_rate-control_booking_rate)) + 
  geom_histogram() + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Booking Rate ATE with Graph Cluster Randomization and \n Diff. in Means (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(clust_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/clust_minus_20.png')

### Compare the distributions of the Effective Treatment Diff in Means
### estimator for -50% price change and booking rate
cluster_effective_minus_20 <- ggplot(treatment_20_cluster_drta, 
                                     aes(x=eff_treatment_booking_rate_95-eff_control_booking_rate_95, 
                                         fill='95% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_20_cluster_drta,
                                    aes(x=eff_treatment_booking_rate_50-eff_control_booking_rate_50, 
                                        fill='50% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram(data=treatment_20_cluster_drta, 
                 aes(x=eff_treatment_booking_rate_75-eff_control_booking_rate_75, 
                     fill='75% Treated Neighbors Threshold'), alpha = .25) + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Booking Rate ATE with Graph Cluster Randomization\n and FNTR Eff. Treat. (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(cluster_effective_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/cluster_effective_minus_20.png')  

### Compare the distributions of the Hajek estimator for -50% price change and booking rate
cluster_hajek_minus_20 <- ggplot(treatment_20_cluster_drta, 
                                 aes(x=hajek_estimator_95, fill='95% Treated Neighbors Threshold'),
                                 alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_20_cluster_drta,
                                    aes(x=hajek_estimator_50, fill='50% Treated Neighbors Threshold'),
                                    alpha = .25) + 
  geom_histogram(data=treatment_20_cluster_drta, 
                 aes(x=hajek_estimator_75, fill='75% Treated Neighbors Threshold'),
                 alpha = .25) + scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Booking Rate ATE with Graph Cluster Randomization and\n FNTR Hajek (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(cluster_hajek_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/cluster_hajek_minus_20.png')  

price_counteractual_minus20 <- ggplot(control_reality, aes(x=control_revenue_per_user, 
                                                           fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality_20, aes(x=treatment_revenue_per_user, 
                                             fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=dollar) + xlab('Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Distribution of Counterfactual Outcomes \n Average Revenue Per Listing (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_counteractual_minus20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_counterfactual_minus20.png')

t.test(control_reality$control_revenue_per_user, treatment_reality_20$treatment_revenue_per_user)

price_ind_minus_20 <- ggplot(treatment_20_ind_drta, aes(x=treatment_revenue_per_user-control_revenue_per_user)
) + 
  geom_histogram() + 
  scale_x_continuous(labels=dollar) + xlab('Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Average Revenue Per Listing ATE with Ind. Assignment and \n Diff. in Means (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_ind_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_ind_minus20.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

price_clust_minus_20 <- ggplot(treatment_20_cluster_drta, 
                               aes(x=treatment_revenue_per_user-control_revenue_per_user)) + 
  geom_histogram() + 
  scale_x_continuous(labels=dollar) + xlab('Difference in Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Average Revenue Per Listing ATE with Graph Cluster Randomization and \n Diff. in Means (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_clust_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_clust_minus_20.png')

### Compare the distributions of the Effective Treatment Diff in Means
### estimator for -50% price change and booking rate
price_cluster_effective_minus_20 <- ggplot(treatment_20_cluster_drta, 
                                           aes(x=eff_treatment_revenue_per_user_95-
                                                 eff_control_revenue_per_user_95, 
                                               fill='95% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_20_cluster_drta,
                                    aes(x=eff_treatment_revenue_per_user_50-
                                          eff_control_revenue_per_user_50, 
                                        fill='50% Treated Neighbors Threshold'), alpha = .25) + 
  geom_histogram(data=treatment_20_cluster_drta, 
                 aes(x=eff_treatment_revenue_per_user_75-
                       eff_control_revenue_per_user_75, 
                     fill='75% Treated Neighbors Threshold'), alpha = .25) + 
  scale_x_continuous(labels=dollar) + xlab('Difference in Average Revenue Per Listing (USD)') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Average Revenue Per Listing ATE \n with Graph Cluster Randomization\n and FNTR Eff. Treat. (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_cluster_effective_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_cluster_effective_minus_20.png')  

### Compare the distributions of the Hajek estimator for -50% price change and booking rate
price_cluster_hajek_minus_20 <- ggplot(treatment_20_cluster_drta, 
                                       aes(x=price_hajek_estimator_95, fill='95% Treated Neighbors Threshold'),
                                       alpha = .25) + 
  geom_histogram() + geom_histogram(data=treatment_20_cluster_drta,
                                    aes(x=price_hajek_estimator_50, fill='50% Treated Neighbors Threshold'),
                                    alpha = .25) + 
  geom_histogram(data=treatment_20_cluster_drta, 
                 aes(x=price_hajek_estimator_75, fill='75% Treated Neighbors Threshold'),
                 alpha = .25) + scale_x_continuous(labels=percent) + 
  xlab('Difference in Average Revenue Per Listing (USD)') + 
  ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Average Revenue Per Listing ATE \n with Graph Cluster Randomization and\n FNTR Hajek (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(price_cluster_hajek_minus_20, 
       file='~/Dropbox (MIT)/MIT/Spring_2016/15.840/Final Paper/Paper/price_cluster_hajek_minus_20.png')  


mean(control_reality$control_revenue_per_user)
sd(control_reality$control_revenue_per_user)
mean(control_reality$control_booking_rate)
sd(control_reality$control_booking_rate)

mean(treatment_reality$treatment_revenue_per_user)
sd(treatment_reality$treatment_revenue_per_user)
mean(treatment_reality$treatment_booking_rate)
sd(treatment_reality$treatment_booking_rate)

mean(treatment_reality_20$treatment_revenue_per_user)
sd(treatment_reality_20$treatment_revenue_per_user)
mean(treatment_reality_20$treatment_booking_rate)
sd(treatment_reality_20$treatment_booking_rate)

mean(treatment_reality_20$treatment_revenue_per_user) - mean(control_reality$control_revenue_per_user)
mean(treatment_reality$treatment_revenue_per_user) - mean(control_reality$control_revenue_per_user)

### Calculate summary statistics for independent randomization
mean(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user)
var(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user)

sqrt((mean(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user) + 2.84)^2 + 
       var(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user))

mean(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user)
var(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user)

sqrt((mean(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user) + 6.84)^2 + 
        var(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user))

mean(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)
var(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)

sqrt(mean(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)^2 + 
       var(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate))

mean(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)
var(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)

sqrt(mean(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)^2 + 
  var(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate))

### Calculate summary statistics for GCR (diff in means)

mean(treatment_20_cluster_drta$treatment_revenue_per_user-
       treatment_20_cluster_drta$control_revenue_per_user)
var(treatment_20_cluster_drta$treatment_revenue_per_user-
      treatment_20_cluster_drta$control_revenue_per_user)

sqrt((mean(treatment_20_cluster_drta$treatment_revenue_per_user-
             treatment_20_cluster_drta$control_revenue_per_user) + 2.84)^2 + 
       var(treatment_20_cluster_drta$treatment_revenue_per_user-
             treatment_20_cluster_drta$control_revenue_per_user))

mean(treatment_50_cluster_drta$treatment_revenue_per_user-
       treatment_50_cluster_drta$control_revenue_per_user)
var(treatment_50_cluster_drta$treatment_revenue_per_user-
      treatment_50_cluster_drta$control_revenue_per_user)

sqrt((mean(treatment_50_cluster_drta$treatment_revenue_per_user-
             treatment_50_cluster_drta$control_revenue_per_user) + 6.84)^2 + 
       var(treatment_50_cluster_drta$treatment_revenue_per_user-
             treatment_50_cluster_drta$control_revenue_per_user))

mean(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)
var(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)

sqrt(mean(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)^2 + 
       var(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate))

mean(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)
var(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)

sqrt(mean(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)^2 + 
       var(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate))


### Calculate summary statistics for effective treatments

## lambda = .5

mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
       treatment_20_cluster_drta$eff_control_revenue_per_user_50)
var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
      treatment_20_cluster_drta$eff_control_revenue_per_user_50)

sqrt((mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_20_cluster_drta$eff_control_revenue_per_user_50) + 2.84)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_20_cluster_drta$eff_control_revenue_per_user_50))

mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
       treatment_50_cluster_drta$eff_control_revenue_per_user_50)
var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
      treatment_50_cluster_drta$eff_control_revenue_per_user_50)

sqrt((mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_50_cluster_drta$eff_control_revenue_per_user_50) + 6.84)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_50_cluster_drta$eff_control_revenue_per_user_50))

mean(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
       treatment_20_cluster_drta$eff_control_booking_rate_50)
var(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
      treatment_20_cluster_drta$eff_control_booking_rate_50)

sqrt(mean(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
            treatment_20_cluster_drta$eff_control_booking_rate_50)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
             treatment_20_cluster_drta$eff_control_booking_rate_50))

mean(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
       treatment_50_cluster_drta$eff_control_booking_rate_50)
var(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
      treatment_50_cluster_drta$eff_control_booking_rate_50)

sqrt(mean(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
            treatment_50_cluster_drta$eff_control_booking_rate_50)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
             treatment_50_cluster_drta$eff_control_booking_rate_50))

## lambda = .75

mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
       treatment_20_cluster_drta$eff_control_revenue_per_user_75)
var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
      treatment_20_cluster_drta$eff_control_revenue_per_user_75)

sqrt((mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_20_cluster_drta$eff_control_revenue_per_user_75) + 2.84)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_20_cluster_drta$eff_control_revenue_per_user_75))

mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
       treatment_50_cluster_drta$eff_control_revenue_per_user_75)
var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
      treatment_50_cluster_drta$eff_control_revenue_per_user_75)

sqrt((mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_50_cluster_drta$eff_control_revenue_per_user_75) + 6.84)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_50_cluster_drta$eff_control_revenue_per_user_75))

mean(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
       treatment_20_cluster_drta$eff_control_booking_rate_75)
var(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
      treatment_20_cluster_drta$eff_control_booking_rate_75)

sqrt(mean(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
            treatment_20_cluster_drta$eff_control_booking_rate_75)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
             treatment_20_cluster_drta$eff_control_booking_rate_75))

mean(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
       treatment_50_cluster_drta$eff_control_booking_rate_75)
var(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
      treatment_50_cluster_drta$eff_control_booking_rate_75)

sqrt(mean(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
            treatment_50_cluster_drta$eff_control_booking_rate_75)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
             treatment_50_cluster_drta$eff_control_booking_rate_75))

## lambda = .95

mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
       treatment_20_cluster_drta$eff_control_revenue_per_user_95)
var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
      treatment_20_cluster_drta$eff_control_revenue_per_user_95)

sqrt((mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_20_cluster_drta$eff_control_revenue_per_user_95) + 2.84)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_20_cluster_drta$eff_control_revenue_per_user_95))

mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
       treatment_50_cluster_drta$eff_control_revenue_per_user_95)
var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
      treatment_50_cluster_drta$eff_control_revenue_per_user_95)

sqrt((mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_50_cluster_drta$eff_control_revenue_per_user_95) + 6.84)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_50_cluster_drta$eff_control_revenue_per_user_95))

mean(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
       treatment_20_cluster_drta$eff_control_booking_rate_95)
var(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
      treatment_20_cluster_drta$eff_control_booking_rate_95)

sqrt(mean(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
            treatment_20_cluster_drta$eff_control_booking_rate_95)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
             treatment_20_cluster_drta$eff_control_booking_rate_95))

mean(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
       treatment_50_cluster_drta$eff_control_booking_rate_95)
var(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
      treatment_50_cluster_drta$eff_control_booking_rate_95)

sqrt(mean(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
            treatment_50_cluster_drta$eff_control_booking_rate_95)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
             treatment_50_cluster_drta$eff_control_booking_rate_95))

### Calculations for Hajek estimator

## lambda = .5

mean(treatment_20_cluster_drta$price_hajek_estimator_50)
var(treatment_20_cluster_drta$price_hajek_estimator_50)

sqrt((mean(treatment_20_cluster_drta$price_hajek_estimator_50) + 2.84)^2 + 
       var(treatment_20_cluster_drta$price_hajek_estimator_50))

mean(treatment_50_cluster_drta$price_hajek_estimator_50)
var(treatment_50_cluster_drta$price_hajek_estimator_50)

sqrt((mean(treatment_50_cluster_drta$price_hajek_estimator_50) + 6.84)^2 + 
       var(treatment_50_cluster_drta$price_hajek_estimator_50))

mean(treatment_20_cluster_drta$hajek_estimator_50)
var(treatment_20_cluster_drta$hajek_estimator_50)

sqrt(mean(treatment_20_cluster_drta$hajek_estimator_50)^2 + 
       var(treatment_20_cluster_drta$hajek_estimator_50))

mean(treatment_50_cluster_drta$hajek_estimator_50)
var(treatment_50_cluster_drta$hajek_estimator_50)

sqrt(mean(treatment_50_cluster_drta$hajek_estimator_50)^2 + 
       var(treatment_50_cluster_drta$hajek_estimator_50))

## lambda = .75

mean(treatment_20_cluster_drta$price_hajek_estimator_75)
var(treatment_20_cluster_drta$price_hajek_estimator_75)

sqrt((mean(treatment_20_cluster_drta$price_hajek_estimator_75) + 2.84)^2 + 
       var(treatment_20_cluster_drta$price_hajek_estimator_75))

mean(treatment_50_cluster_drta$price_hajek_estimator_75)
var(treatment_50_cluster_drta$price_hajek_estimator_75)

sqrt((mean(treatment_50_cluster_drta$price_hajek_estimator_75) + 6.84)^2 + 
       var(treatment_50_cluster_drta$price_hajek_estimator_75))

mean(treatment_20_cluster_drta$hajek_estimator_75)
var(treatment_20_cluster_drta$hajek_estimator_75)

sqrt(mean(treatment_20_cluster_drta$hajek_estimator_75)^2 + 
       var(treatment_20_cluster_drta$hajek_estimator_75))

mean(treatment_50_cluster_drta$hajek_estimator_75)
var(treatment_50_cluster_drta$hajek_estimator_75)

sqrt(mean(treatment_50_cluster_drta$hajek_estimator_75)^2 + 
       var(treatment_50_cluster_drta$hajek_estimator_75))

## lambda = .95

mean(treatment_20_cluster_drta$price_hajek_estimator_95)
var(treatment_20_cluster_drta$price_hajek_estimator_95)

sqrt((mean(treatment_20_cluster_drta$price_hajek_estimator_95) + 2.84)^2 + 
       var(treatment_20_cluster_drta$price_hajek_estimator_95))

mean(treatment_50_cluster_drta$price_hajek_estimator_95)
var(treatment_50_cluster_drta$price_hajek_estimator_95)

sqrt((mean(treatment_50_cluster_drta$price_hajek_estimator_95) + 6.84)^2 + 
       var(treatment_50_cluster_drta$price_hajek_estimator_95))

mean(treatment_20_cluster_drta$hajek_estimator_95)
var(treatment_20_cluster_drta$hajek_estimator_95)

sqrt(mean(treatment_20_cluster_drta$hajek_estimator_95)^2 + 
       var(treatment_20_cluster_drta$hajek_estimator_95))

mean(treatment_50_cluster_drta$hajek_estimator_95)
var(treatment_50_cluster_drta$hajek_estimator_95)

sqrt(mean(treatment_50_cluster_drta$hajek_estimator_95)^2 + 
       var(treatment_50_cluster_drta$hajek_estimator_95))

### 50% Ind

sum(ifelse(treatment_50_ind_drta$p_value <= .05, 1, 0))/nrow(treatment_50_ind_drta)
1-sum(ifelse(treatment_50_ind_drta$price_p_value <= .05, 1, 0))/nrow(treatment_50_ind_drta)

sum(filter(treatment_50_ind_drta, p_value <= .05)$treatment_booking_rate - 
      filter(treatment_50_ind_drta, p_value <= .05)$control_booking_rate)/
  nrow(filter(treatment_50_ind_drta, p_value <= .05))

sum(filter(treatment_50_ind_drta, price_p_value <= .05)$treatment_revenue_per_user - 
      filter(treatment_50_ind_drta, price_p_value <= .05)$control_revenue_per_user)/
  nrow(filter(treatment_50_ind_drta, price_p_value <= .05))
  
### 20% Ind

sum(ifelse(treatment_20_ind_drta$p_value <= .05, 1, 0))/nrow(treatment_20_ind_drta)
1-sum(ifelse(treatment_20_ind_drta$price_p_value <= .05, 1, 0))/nrow(treatment_20_ind_drta)

sum(filter(treatment_20_ind_drta, p_value <= .05)$treatment_booking_rate - 
      filter(treatment_20_ind_drta, p_value <= .05)$control_booking_rate)/
  nrow(filter(treatment_20_ind_drta, p_value <= .05))

sum(filter(treatment_20_ind_drta, price_p_value <= .05)$treatment_revenue_per_user - 
      filter(treatment_20_ind_drta, price_p_value <= .05)$control_revenue_per_user)/
  nrow(filter(treatment_20_ind_drta, price_p_value <= .05))

### 50% GCR

sum(ifelse(treatment_50_cluster_drta$p_value <= .05, 1, 0))/nrow(treatment_50_cluster_drta)
1-sum(ifelse(treatment_50_cluster_drta$price_p_value <= .05, 1, 0))/nrow(treatment_50_cluster_drta)

sum(filter(treatment_50_cluster_drta, p_value <= .05)$treatment_booking_rate - 
      filter(treatment_50_cluster_drta, p_value <= .05)$control_booking_rate)/
  nrow(filter(treatment_50_cluster_drta, p_value <= .05))

sum(filter(treatment_50_cluster_drta, price_p_value <= .05)$treatment_revenue_per_user - 
      filter(treatment_50_cluster_drta, price_p_value <= .05)$control_revenue_per_user)/
  nrow(filter(treatment_50_cluster_drta, price_p_value <= .05))

### 20% GCR

sum(ifelse(treatment_20_cluster_drta$p_value <= .05, 1, 0))/nrow(treatment_20_cluster_drta)
1-sum(ifelse(treatment_20_cluster_drta$price_p_value <= .05, 1, 0))/nrow(treatment_20_cluster_drta)

sum(filter(treatment_20_cluster_drta, p_value <= .05)$treatment_booking_rate - 
      filter(treatment_20_cluster_drta, p_value <= .05)$control_booking_rate)/
  nrow(filter(treatment_20_cluster_drta, p_value <= .05))

sum(filter(treatment_20_cluster_drta, price_p_value <= .05)$treatment_revenue_per_user - 
      filter(treatment_20_cluster_drta, price_p_value <= .05)$control_revenue_per_user)/
  nrow(filter(treatment_20_cluster_drta, price_p_value <= .05))

### 50% GCR, Eff Lambda = .5

sum(ifelse(treatment_50_cluster_drta$eff_p_value_50 <= .05, 1, 0))/nrow(treatment_50_cluster_drta)
1-sum(ifelse(treatment_50_cluster_drta$eff_price_p_value_50 <= .05, 1, 0))/nrow(treatment_50_cluster_drta)

sum(filter(treatment_50_cluster_drta, eff_p_value_50 <= .05)$eff_treatment_booking_rate_50 - 
      filter(treatment_50_cluster_drta, eff_p_value_50 <= .05)$eff_control_booking_rate_50)/
  nrow(filter(treatment_50_cluster_drta, eff_p_value_50 <= .05))

sum(filter(treatment_50_cluster_drta, eff_price_p_value_50 <= .05)$eff_treatment_revenue_per_user_50 - 
      filter(treatment_50_cluster_drta, eff_price_p_value_50 <= .05)$eff_control_revenue_per_user_50)/
  nrow(filter(treatment_50_cluster_drta, eff_price_p_value_50 <= .05))

### 20% GCR, Eff Lambda = .5

sum(ifelse(treatment_20_cluster_drta$eff_p_value_50 <= .05, 1, 0))/nrow(treatment_20_cluster_drta)
1-sum(ifelse(treatment_20_cluster_drta$eff_price_p_value_50 <= .05, 1, 0))/nrow(treatment_20_cluster_drta)

sum(filter(treatment_20_cluster_drta, eff_p_value_50 <= .05)$eff_treatment_booking_rate_50 - 
      filter(treatment_20_cluster_drta, eff_p_value_50 <= .05)$eff_control_booking_rate_50)/
  nrow(filter(treatment_20_cluster_drta, eff_p_value_50 <= .05))

sum(filter(treatment_20_cluster_drta, eff_price_p_value_50 <= .05)$eff_treatment_revenue_per_user_50 - 
      filter(treatment_20_cluster_drta, eff_price_p_value_50 <= .05)$eff_control_revenue_per_user_50)/
  nrow(filter(treatment_20_cluster_drta, eff_price_p_value_50 <= .05))

### 50% GCR, Eff Lambda = .75

sum(ifelse(treatment_50_cluster_drta$eff_p_value_75 <= .05, 1, 0))/nrow(treatment_50_cluster_drta)
1-sum(ifelse(treatment_50_cluster_drta$eff_price_p_value_75 <= .05, 1, 0))/nrow(treatment_50_cluster_drta)

sum(filter(treatment_50_cluster_drta, eff_p_value_75 <= .05)$eff_treatment_booking_rate_75 - 
      filter(treatment_50_cluster_drta, eff_p_value_75 <= .05)$eff_control_booking_rate_75)/
  nrow(filter(treatment_50_cluster_drta, eff_p_value_75 <= .05))

sum(filter(treatment_50_cluster_drta, eff_price_p_value_75 <= .05)$eff_treatment_revenue_per_user_75 - 
      filter(treatment_50_cluster_drta, eff_price_p_value_75 <= .05)$eff_control_revenue_per_user_75)/
  nrow(filter(treatment_50_cluster_drta, eff_price_p_value_75 <= .05))

### 20% GCR, Eff Lambda = .75

sum(ifelse(treatment_20_cluster_drta$eff_p_value_75 <= .05, 1, 0))/nrow(treatment_20_cluster_drta)
1-sum(ifelse(treatment_20_cluster_drta$eff_price_p_value_75 <= .05, 1, 0))/nrow(treatment_20_cluster_drta)

sum(filter(treatment_20_cluster_drta, eff_p_value_75 <= .05)$eff_treatment_booking_rate_75 - 
      filter(treatment_20_cluster_drta, eff_p_value_75 <= .05)$eff_control_booking_rate_75)/
  nrow(filter(treatment_20_cluster_drta, eff_p_value_75 <= .05))

sum(filter(treatment_20_cluster_drta, eff_price_p_value_75 <= .05)$eff_treatment_revenue_per_user_75 - 
      filter(treatment_20_cluster_drta, eff_price_p_value_75 <= .05)$eff_control_revenue_per_user_75)/
  nrow(filter(treatment_20_cluster_drta, eff_price_p_value_75 <= .05))

### 50% GCR, Eff Lambda = .95

sum(ifelse(treatment_50_cluster_drta$eff_p_value_95 <= .05, 1, 0))/nrow(treatment_50_cluster_drta)
1-sum(ifelse(treatment_50_cluster_drta$eff_price_p_value_95 <= .05, 1, 0))/nrow(treatment_50_cluster_drta)

sum(filter(treatment_50_cluster_drta, eff_p_value_95 <= .05)$eff_treatment_booking_rate_95 - 
      filter(treatment_50_cluster_drta, eff_p_value_95 <= .05)$eff_control_booking_rate_95)/
  nrow(filter(treatment_50_cluster_drta, eff_p_value_95 <= .05))

sum(filter(treatment_50_cluster_drta, eff_price_p_value_95 <= .05)$eff_treatment_revenue_per_user_95 - 
      filter(treatment_50_cluster_drta, eff_price_p_value_95 <= .05)$eff_control_revenue_per_user_95)/
  nrow(filter(treatment_50_cluster_drta, eff_price_p_value_95 <= .05))

### 20% GCR, Eff Lambda = .95

sum(ifelse(treatment_20_cluster_drta$eff_p_value_95 <= .05, 1, 0))/nrow(treatment_20_cluster_drta)
1-sum(ifelse(treatment_20_cluster_drta$eff_price_p_value_95 <= .05, 1, 0))/nrow(treatment_20_cluster_drta)

sum(filter(treatment_20_cluster_drta, eff_p_value_95 <= .05)$eff_treatment_booking_rate_95 - 
      filter(treatment_20_cluster_drta, eff_p_value_95 <= .05)$eff_control_booking_rate_95)/
  nrow(filter(treatment_20_cluster_drta, eff_p_value_95 <= .05))

sum(filter(treatment_20_cluster_drta, eff_price_p_value_95 <= .05)$eff_treatment_revenue_per_user_95 - 
      filter(treatment_20_cluster_drta, eff_price_p_value_95 <= .05)$eff_control_revenue_per_user_95)/
  nrow(filter(treatment_20_cluster_drta, eff_price_p_value_95 <= .05))
