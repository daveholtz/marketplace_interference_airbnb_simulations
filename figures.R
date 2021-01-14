library(ggplot2)
library(ggmap)
library(dplyr)
library(knitr)
library(scales)
setwd('~/Dropbox (MIT)/Publications in Progress/Reducing Marketplace Interference through GCR - Job Market Slash ISR/Analysis Code/')

load('data_for_simulation_blocked.Rdata')
load('baseline_distributions_blocked.Rdata')
load('cluster_performance_50_pct_negative_50_effect_blocked.Rdata')
load('ind_performance_50_pct_negative_50_effect_blocked.Rdata')
load('treatment_minus_20_distribution_blocked.Rdata')
load('ind_performance_50_pct_negative_20_effect_blocked.Rdata')
load('cluster_performance_50_pct_negative_20_effect_blocked.Rdata')

airbnb_listings_for_raw <- read.csv('Airbnb_Listings_Miami.csv', row.names=1)

airbnb_listings_for_raw %>%
  mutate(`Room Type` = room_type) -> airbnb_listings_for_map

register_google(key = 'AIzaSyDkN-Wn_920KL53DZ2aXVGe3f5Yz3eXAVU')
Miami <- get_map('Miami, FL', zoom=10)

miami_map <- ggmap(Miami)
miami_map <- miami_map + geom_point(data = airbnb_listings_for_map, aes(longitude, latitude,
                                                                        color=`Room Type`), 
                                    size=.3, alpha=.7) + 
  xlab('Longitude') + ylab('Latitude') + 
  theme(plot.title = element_text(size = rel(1.25)))
ggsave(miami_map, file='../figures/listings_map.png', height=6, width=6, unit='in')

airbnb_listings_for_raw %>% 
  dplyr::select(-collected, -room_id, -host_id, -borough, -neighborhood) -> airbnb_data_for_table

transitivity(graph_distance_room_type_accom)

counteractual_minus50 <-ggplot(control_reality, aes(x=control_booking_rate, fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality, aes(x=treatment_booking_rate, fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=percent) + xlab('Booking Rate') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Booking Rate (Price change = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, size=rel(1.5)))

ggsave(counteractual_minus50, 
       file='../figures/counterfactual_minus50_block.png', height=7, width=7, units='in')

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
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(ind_minus_50, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/ind_minus50_block.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

clust_minus_50 <- ggplot(treatment_50_cluster_drta, aes(x=treatment_booking_rate-control_booking_rate)) + 
  geom_histogram() + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Booking Rate ATE with Graph Cluster Randomization\n and Diff. in Means (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(clust_minus_50, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/clust_minus_50.png')

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/cluster_effective_minus_50.png')  

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
  ggtitle('Booking Rate ATE with Graph Cluster Randomization\n and FNTR Hajek (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(cluster_hajek_minus_50, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/cluster_hajek_minus_50.png')  

price_counteractual_minus50 <- ggplot(control_reality, aes(x=control_revenue_per_user, 
                                                          fill='Control'), alpha=.25) + 
  geom_histogram() + 
  geom_histogram(data=treatment_reality, aes(x=treatment_revenue_per_user, 
                                             fill='Treatment'), alpha=.25) + 
  scale_x_continuous(labels=dollar) + xlab('Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='Counterfactual State') + 
  ggtitle('Average Revenue Per Listing (Price change = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, size = rel(1.5)))

ggsave(price_counteractual_minus50, 
       file='../figures/price_counterfactual_minus50_block.png', height=7, width=7, units='in')

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
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(price_ind_minus_50, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_ind_minus50_block.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

price_clust_minus_50 <- ggplot(treatment_50_cluster_drta, 
                               aes(x=treatment_revenue_per_user-control_revenue_per_user)) + 
  geom_histogram() + 
  scale_x_continuous(labels=dollar) + xlab('Difference in Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Average Revenue Per Listing ATE with Graph Cluster\n Randomization and Diff. in Means (Effect Size = -50%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(price_clust_minus_50, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_clust_minus_50.png')

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_cluster_effective_minus_50.png')  

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_cluster_hajek_minus_50.png')  

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/counterfactual_minus20_block.png')

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
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(ind_minus_20, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/ind_minus20.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

clust_minus_20 <- ggplot(treatment_20_cluster_drta, aes(x=treatment_booking_rate-control_booking_rate)) + 
  geom_histogram() + 
  scale_x_continuous(labels=percent) + xlab('Difference in Booking Rate') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Booking Rate ATE with Graph Cluster Randomization\n and Diff. in Means (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(clust_minus_20, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/clust_minus_20.png')

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/cluster_effective_minus_20.png')  

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
  ggtitle('Booking Rate ATE with Graph Cluster Randomization\n and FNTR Hajek (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)))

ggsave(cluster_hajek_minus_20, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/cluster_hajek_minus_20.png')  

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_counterfactual_minus20_block.png')

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
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(price_ind_minus_20, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_ind_minus20.png')

### Compare the distributions of diff in means with graph cluster randomization
### for -50% price change and booking rate

price_clust_minus_20 <- ggplot(treatment_20_cluster_drta, 
                               aes(x=treatment_revenue_per_user-control_revenue_per_user)) + 
  geom_histogram() + 
  scale_x_continuous(labels=dollar) + xlab('Difference in Average Revenue Per Listing (USD)') + ylab('# of Simulations') + 
  labs(fill='') + 
  ggtitle('Average Revenue Per Listing ATE with Graph Cluster\n Randomization and Diff. in Means (Effect Size = -20%)') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = rel(1)),
        text = element_text(size = 15))

ggsave(price_clust_minus_20, 
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_clust_minus_20.png')

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_cluster_effective_minus_20.png')  

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
       file='~/Dropbox (MIT)/Talks/WISE 2016/Manuscript/price_cluster_hajek_minus_20.png')  


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

true_effect_20 <- mean(treatment_reality_20$treatment_revenue_per_user) - mean(control_reality$control_revenue_per_user)
true_effect_50 <- mean(treatment_reality$treatment_revenue_per_user) - mean(control_reality$control_revenue_per_user)

### Calculate summary statistics for independent randomization

mean(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user)
var(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user)
sd(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user)

sqrt((mean(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user) - true_effect_20)^2 + 
       var(treatment_20_ind_drta$treatment_revenue_per_user-treatment_20_ind_drta$control_revenue_per_user))

mean(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user)
var(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user)
sd(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user)

sqrt((mean(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user) - true_effect_50)^2 + 
        var(treatment_50_ind_drta$treatment_revenue_per_user-treatment_50_ind_drta$control_revenue_per_user))

mean(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)
var(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)
sd(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)

sqrt(mean(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate)^2 + 
       var(treatment_20_ind_drta$treatment_booking_rate-treatment_20_ind_drta$control_booking_rate))

mean(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)
var(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)
sd(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)

sqrt(mean(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate)^2 + 
  var(treatment_50_ind_drta$treatment_booking_rate-treatment_50_ind_drta$control_booking_rate))

### Calculate summary statistics for GCR (diff in means)

mean(treatment_20_cluster_drta$treatment_revenue_per_user-
       treatment_20_cluster_drta$control_revenue_per_user)
var(treatment_20_cluster_drta$treatment_revenue_per_user-
      treatment_20_cluster_drta$control_revenue_per_user)
sd(treatment_20_cluster_drta$treatment_revenue_per_user-
      treatment_20_cluster_drta$control_revenue_per_user)

sqrt((mean(treatment_20_cluster_drta$treatment_revenue_per_user-
             treatment_20_cluster_drta$control_revenue_per_user) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$treatment_revenue_per_user-
             treatment_20_cluster_drta$control_revenue_per_user))

mean(treatment_50_cluster_drta$treatment_revenue_per_user-
       treatment_50_cluster_drta$control_revenue_per_user)
var(treatment_50_cluster_drta$treatment_revenue_per_user-
      treatment_50_cluster_drta$control_revenue_per_user)
sd(treatment_50_cluster_drta$treatment_revenue_per_user-
      treatment_50_cluster_drta$control_revenue_per_user)

sqrt((mean(treatment_50_cluster_drta$treatment_revenue_per_user-
             treatment_50_cluster_drta$control_revenue_per_user) - true_effect_50)^2 + 
       var(treatment_50_cluster_drta$treatment_revenue_per_user-
             treatment_50_cluster_drta$control_revenue_per_user))

mean(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)
var(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)
sd(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)

sqrt(mean(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate)^2 + 
       var(treatment_20_cluster_drta$treatment_booking_rate-treatment_20_cluster_drta$control_booking_rate))

mean(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)
var(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)
sd(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)

sqrt(mean(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate)^2 + 
       var(treatment_50_cluster_drta$treatment_booking_rate-treatment_50_cluster_drta$control_booking_rate))


### Calculate summary statistics for effective treatments

## lambda = .5

mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
       treatment_20_cluster_drta$eff_control_revenue_per_user_50)
var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
      treatment_20_cluster_drta$eff_control_revenue_per_user_50)
sd(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
      treatment_20_cluster_drta$eff_control_revenue_per_user_50)

sqrt((mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_20_cluster_drta$eff_control_revenue_per_user_50) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_20_cluster_drta$eff_control_revenue_per_user_50))

mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
       treatment_50_cluster_drta$eff_control_revenue_per_user_50)
var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
      treatment_50_cluster_drta$eff_control_revenue_per_user_50)
sd(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
      treatment_50_cluster_drta$eff_control_revenue_per_user_50)

sqrt((mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_50_cluster_drta$eff_control_revenue_per_user_50) -true_effect_50)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_50-
             treatment_50_cluster_drta$eff_control_revenue_per_user_50))

mean(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
       treatment_20_cluster_drta$eff_control_booking_rate_50)
var(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
      treatment_20_cluster_drta$eff_control_booking_rate_50)
sd(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
      treatment_20_cluster_drta$eff_control_booking_rate_50)

sqrt(mean(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
            treatment_20_cluster_drta$eff_control_booking_rate_50)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_booking_rate_50-
             treatment_20_cluster_drta$eff_control_booking_rate_50))

mean(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
       treatment_50_cluster_drta$eff_control_booking_rate_50)
var(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
      treatment_50_cluster_drta$eff_control_booking_rate_50)
sd(treatment_50_cluster_drta$eff_treatment_booking_rate_50-
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
sd(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
      treatment_20_cluster_drta$eff_control_revenue_per_user_75)

sqrt((mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_20_cluster_drta$eff_control_revenue_per_user_75) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_20_cluster_drta$eff_control_revenue_per_user_75))

mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
       treatment_50_cluster_drta$eff_control_revenue_per_user_75)
var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
      treatment_50_cluster_drta$eff_control_revenue_per_user_75)
sd(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
      treatment_50_cluster_drta$eff_control_revenue_per_user_75)

sqrt((mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_50_cluster_drta$eff_control_revenue_per_user_75) - true_effect_50)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_75-
             treatment_50_cluster_drta$eff_control_revenue_per_user_75))

mean(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
       treatment_20_cluster_drta$eff_control_booking_rate_75)
var(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
      treatment_20_cluster_drta$eff_control_booking_rate_75)
sd(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
      treatment_20_cluster_drta$eff_control_booking_rate_75)

sqrt(mean(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
            treatment_20_cluster_drta$eff_control_booking_rate_75)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_booking_rate_75-
             treatment_20_cluster_drta$eff_control_booking_rate_75))

mean(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
       treatment_50_cluster_drta$eff_control_booking_rate_75)
var(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
      treatment_50_cluster_drta$eff_control_booking_rate_75)
sd(treatment_50_cluster_drta$eff_treatment_booking_rate_75-
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
sd(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
      treatment_20_cluster_drta$eff_control_revenue_per_user_95)

sqrt((mean(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_20_cluster_drta$eff_control_revenue_per_user_95) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_20_cluster_drta$eff_control_revenue_per_user_95))

mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
       treatment_50_cluster_drta$eff_control_revenue_per_user_95)
var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
      treatment_50_cluster_drta$eff_control_revenue_per_user_95)
sd(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
      treatment_50_cluster_drta$eff_control_revenue_per_user_95)

sqrt((mean(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_50_cluster_drta$eff_control_revenue_per_user_95) - true_effect_50)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_revenue_per_user_95-
             treatment_50_cluster_drta$eff_control_revenue_per_user_95))

mean(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
       treatment_20_cluster_drta$eff_control_booking_rate_95)
var(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
      treatment_20_cluster_drta$eff_control_booking_rate_95)
sd(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
      treatment_20_cluster_drta$eff_control_booking_rate_95)

sqrt(mean(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
            treatment_20_cluster_drta$eff_control_booking_rate_95)^2 + 
       var(treatment_20_cluster_drta$eff_treatment_booking_rate_95-
             treatment_20_cluster_drta$eff_control_booking_rate_95))

mean(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
       treatment_50_cluster_drta$eff_control_booking_rate_95)
var(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
      treatment_50_cluster_drta$eff_control_booking_rate_95)
sd(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
      treatment_50_cluster_drta$eff_control_booking_rate_95)

sqrt(mean(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
            treatment_50_cluster_drta$eff_control_booking_rate_95)^2 + 
       var(treatment_50_cluster_drta$eff_treatment_booking_rate_95-
             treatment_50_cluster_drta$eff_control_booking_rate_95))

### Calculations for Hajek estimator

## lambda = .5

mean(treatment_20_cluster_drta$price_hajek_estimator_50)
var(treatment_20_cluster_drta$price_hajek_estimator_50)
sd(treatment_20_cluster_drta$price_hajek_estimator_50)

sqrt((mean(treatment_20_cluster_drta$price_hajek_estimator_50) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$price_hajek_estimator_50))

mean(treatment_50_cluster_drta$price_hajek_estimator_50)
var(treatment_50_cluster_drta$price_hajek_estimator_50)
sd(treatment_50_cluster_drta$price_hajek_estimator_50)

sqrt((mean(treatment_50_cluster_drta$price_hajek_estimator_50) - true_effect_50)^2 + 
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
sd(treatment_20_cluster_drta$price_hajek_estimator_75)

sqrt((mean(treatment_20_cluster_drta$price_hajek_estimator_75) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$price_hajek_estimator_75))

mean(treatment_50_cluster_drta$price_hajek_estimator_75)
var(treatment_50_cluster_drta$price_hajek_estimator_75)
sd(treatment_50_cluster_drta$price_hajek_estimator_75)

sqrt((mean(treatment_50_cluster_drta$price_hajek_estimator_75) - true_effect_50)^2 + 
       var(treatment_50_cluster_drta$price_hajek_estimator_75))

mean(treatment_20_cluster_drta$hajek_estimator_75)
var(treatment_20_cluster_drta$hajek_estimator_75)

sqrt(mean(treatment_20_cluster_drta$hajek_estimator_75)^2 + 
       var(treatment_20_cluster_drta$hajek_estimator_75))

mean(treatment_50_cluster_drta$hajek_estimator_75)
var(treatment_50_cluster_drta$hajek_estimator_75)
sd(treatment_50_cluster_drta$hajek_estimator_75)

sqrt(mean(treatment_50_cluster_drta$hajek_estimator_75)^2 + 
       var(treatment_50_cluster_drta$hajek_estimator_75))

## lambda = .95

mean(treatment_20_cluster_drta$price_hajek_estimator_95)
var(treatment_20_cluster_drta$price_hajek_estimator_95)
sd(treatment_20_cluster_drta$price_hajek_estimator_95)

sqrt((mean(treatment_20_cluster_drta$price_hajek_estimator_95) - true_effect_20)^2 + 
       var(treatment_20_cluster_drta$price_hajek_estimator_95))

mean(treatment_50_cluster_drta$price_hajek_estimator_95)
var(treatment_50_cluster_drta$price_hajek_estimator_95)
sd(treatment_50_cluster_drta$price_hajek_estimator_95)

sqrt((mean(treatment_50_cluster_drta$price_hajek_estimator_95) - true_effect_50)^2 + 
       var(treatment_50_cluster_drta$price_hajek_estimator_95))

mean(treatment_20_cluster_drta$hajek_estimator_95)
var(treatment_20_cluster_drta$hajek_estimator_95)
sd(treatment_20_cluster_drta$hajek_estimator_95)

sqrt(mean(treatment_20_cluster_drta$hajek_estimator_95)^2 + 
       var(treatment_20_cluster_drta$hajek_estimator_95))

mean(treatment_50_cluster_drta$hajek_estimator_95)
var(treatment_50_cluster_drta$hajek_estimator_95)
sd(treatment_50_cluster_drta$hajek_estimator_95)

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



biases <- c(2.74, .005, 1.02, .002, 2.13, .003, 0.65, .001, 2.11, .003, 0.64, .001, 2.04,
                        .003, 0.61, .001, 2.00, .003, 0.59, .001, 2.41, .004, 1.00, .001,
                        2.13, .003, 0.70, .001, 2.19, .003, 0.81, .002)

rmses <- c(3.37, .007, 2.41, .004, 3.65, .008, 3.51, .007, 3.61, .008, 3.47, .007, 3.57, .008,
           3.47, .008, 3.65, .008, 3.59, .008, 3.81, .008, 3.60, .007, 3.60, .01, 3.52, .009, 
           3.69, 0.01, 3.60, .009)

treatment <- rep(c('-50% Price (Revenue)', '-50% Price (Bookings)', '-20% Price (Revenue)', '-20% Price (Bookings)'), 
                   16)

Quantity <- c(rep('Bias', 32), rep('RMSE', 32))

Analysis_Technique <- factor(rep(c(rep('Ind., Naive', 4), rep('GCR, Naive', 4), rep('GCR, FNTR (lambda = .5)', 4),
                        rep('GCR, FNTR (lambda = .75)', 4), rep('GCR, FNTR (lambda = .95)', 4),
                        rep('GCR, FNTR, Hajek (lambda = .5)', 4), rep('GCR, FNTR, Hajek (lambda = .75)', 4),
                        rep('GCR, FNTR, Hajek (lambda = .95)', 4)), 2),
                        levels = c('Ind., Naive', 'GCR, Naive', 'GCR, FNTR (lambda = .5)',
                                   'GCR, FNTR (lambda = .75)', 'GCR, FNTR (lambda = .95)',
                                   'GCR, FNTR, Hajek (lambda = .5)', 'GCR, FNTR, Hajek (lambda = .75)',
                                   'GCR, FNTR, Hajek (lambda = .95)'))

bias_variance_df <- data.frame(Value = c(biases, rmses), 
                               Treatment = treatment,
                               Quantity = Quantity,
                               `Analysis Technique` = Analysis_Technique)

effects_plot <- bias_variance_df %>%
  mutate(`Analysis Technique` = Analysis.Technique) %>%
  ggplot(., aes(x=`Analysis Technique`, 
                y=Value, fill=Quantity)) +
  geom_bar(position='dodge', stat='identity') +
  facet_wrap(~Treatment, ncol=2, nrow=2, scales='free') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, size=rel(1.5)),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(effects_plot, file='../figures/overall_effects_plot.png', height=6, width=5, units='in')

rbind(treatment_20_ind_drta %>% mutate(method = 'independent', price_change = '-20%'),
      treatment_50_ind_drta %>% mutate(method = 'independent', price_change = '-50%'),
      treatment_50_cluster_drta[, names(treatment_20_ind_drta)] %>% mutate(method = 'cluster', price_change = '-50%'),
      treatment_20_cluster_drta[, names(treatment_20_ind_drta)] %>% mutate(method = 'cluster', price_change = '-20%')) %>%
  mutate(arpu_ate = treatment_revenue_per_user - control_revenue_per_user,
         br_ate = treatment_booking_rate - control_booking_rate) %>%
  dplyr::select(method, price_change, arpu_ate, br_ate) %>%
  tidyr::gather(., metric, diff, arpu_ate:br_ate) -> histogram_df_whole

histogram_df_20 <- histogram_df_whole %>% 
  mutate(`Treatment effect` = diff,
         `Price change` = price_change,
         `Assignemnt method` = method,
         `Outcome` = metric) %>% 
  filter(price_change == '-20%') %>% 
  mutate(method = ifelse(method == 'independent', 'Independent assignment', 'Graph cluster randomization'),
         metric = ifelse(metric == 'arpu_ate', 'Average revenue per listing', 'Booking rate')) %>%
  ggplot(., aes(x=`Treatment effect`)) + geom_histogram() + 
  facet_grid(method ~ metric, scales='free') + ylab('# of simulations') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, size=rel(1.5)))
ggsave(histogram_df_20, file='../figures/histogram_df_20.jpg', height=4, width=5, units='in')

histogram_df_50 <- histogram_df_whole %>% 
  mutate(`Treatment effect` = diff,
         `Price change` = price_change,
         `Assignemnt method` = method,
         `Outcome` = metric) %>% 
  filter(price_change == '-50%') %>% 
  mutate(method = ifelse(method == 'independent', 'Independent assignment', 'Graph cluster randomization'),
         metric = ifelse(metric == 'arpu_ate', 'Average revenue per listing', 'Booking rate')) %>%
  ggplot(., aes(x=`Treatment effect`)) + geom_histogram() + 
  facet_grid(method ~ metric, scales='free') + ylab('# of simulations') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, size=rel(1.5)))
ggsave(histogram_df_50, file='../figures/histogram_df_50.jpg', height=4, width=5, units='in')

rbind(treatment_20_cluster_drta %>% mutate(tate = eff_treatment_booking_rate_95 - eff_control_booking_rate_95,
                                     metric = 'booking_rate',
                                     threshold = '.95',
                                     treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = eff_treatment_booking_rate_75 - eff_control_booking_rate_75,
                                           metric = 'booking_rate',
                                           threshold = '.75',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = eff_treatment_booking_rate_50 - eff_control_booking_rate_50,
                                           metric = 'booking_rate',
                                           threshold = '.50',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = eff_treatment_booking_rate_95 - eff_control_booking_rate_95,
                                           metric = 'booking_rate',
                                           threshold = '.95',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = eff_treatment_booking_rate_75 - eff_control_booking_rate_75,
                                           metric = 'booking_rate',
                                           threshold = '.75',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = eff_treatment_booking_rate_50 - eff_control_booking_rate_50,
                                           metric = 'booking_rate',
                                           threshold = '.50',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = eff_treatment_revenue_per_user_95 - eff_control_revenue_per_user_95,
                                           metric = 'arpu',
                                           threshold = '.95',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = eff_treatment_revenue_per_user_75 - eff_control_revenue_per_user_75,
                                           metric = 'arpu',
                                           threshold = '.75',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = eff_treatment_revenue_per_user_50 - eff_control_revenue_per_user_50,
                                           metric = 'arpu',
                                           threshold = '.50',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = eff_treatment_revenue_per_user_95 - eff_control_revenue_per_user_95,
                                           metric = 'arpu',
                                           threshold = '.95',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = eff_treatment_revenue_per_user_75 - eff_control_revenue_per_user_75,
                                           metric = 'arpu',
                                           threshold = '.75',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = eff_treatment_revenue_per_user_50 - eff_control_revenue_per_user_50,
                                           metric = 'arpu',
                                           threshold = '.50',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment)) -> fntr_data

fntr_compare <- fntr_data %>% 
  mutate(metric = ifelse(metric == 'arpu', 'Average revenue \nper listing', 'Booking rate'),
         threshold = ifelse(threshold == '.50', 'FNTR threshold = .5', 
                            ifelse(threshold == '.75', 'FNTR threshold = .75', 'FNTR threshold = .95'))) %>% 
  ggplot(., aes(x=tate, fill=threshold)) + geom_histogram(alpha=.33, position='identity') + facet_grid(treatment ~ metric, scales='free') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.text=element_text(size = rel(.4)),
        legend.title = element_blank(),
        axis.text=element_text(size=rel(.5)),
        strip.text.x = element_text(size=rel(.75)),
        strip.text.y = element_text(size=rel(.75)),        
        plot.title = element_text(hjust=0.5, size=rel(1.5))) +
  xlab('Treatment effect') + ylab('# of simulations')
ggsave(fntr_compare, file='../figures/fntr_compare.jpg', height=4, width=5, units='in')
      
rbind(treatment_20_cluster_drta %>% mutate(tate = hajek_estimator_95,
                                           metric = 'booking_rate',
                                           threshold = '.95',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = hajek_estimator_75,
                                           metric = 'booking_rate',
                                           threshold = '.75',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = hajek_estimator_50,
                                           metric = 'booking_rate',
                                           threshold = '.50',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = hajek_estimator_95,
                                           metric = 'booking_rate',
                                           threshold = '.95',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = hajek_estimator_75,
                                           metric = 'booking_rate',
                                           threshold = '.75',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = hajek_estimator_50,
                                           metric = 'booking_rate',
                                           threshold = '.50',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = price_hajek_estimator_95,
                                           metric = 'arpu',
                                           threshold = '.95',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = price_hajek_estimator_75,
                                           metric = 'arpu',
                                           threshold = '.75',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_20_cluster_drta %>% mutate(tate = price_hajek_estimator_50,
                                           metric = 'arpu',
                                           threshold = '.50',
                                           treatment = '- 20% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = price_hajek_estimator_95,
                                           metric = 'arpu',
                                           threshold = '.95',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = price_hajek_estimator_75,
                                           metric = 'arpu',
                                           threshold = '.75',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment),
      treatment_50_cluster_drta %>% mutate(tate = price_hajek_estimator_50,
                                           metric = 'arpu',
                                           threshold = '.50',
                                           treatment = '- 50% price') %>% dplyr::select(tate, metric, threshold, treatment)) -> hajek_data   

hajek_compare <- hajek_data %>% 
  mutate(metric = ifelse(metric == 'arpu', 'Average revenue \nper listing', 'Booking rate'),
         threshold = ifelse(threshold == '.50', 'FNTR threshold = .5', 
                            ifelse(threshold == '.75', 'FNTR threshold = .75', 'FNTR threshold = .95'))) %>% 
  ggplot(., aes(x=tate, fill=threshold)) + geom_histogram(alpha=.33, position='identity') + facet_grid(treatment ~ metric, scales='free') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.text=element_text(size = rel(.4)),
        legend.title = element_blank(),
        axis.text=element_text(size=rel(.5)),
        strip.text.x = element_text(size=rel(.75)),
        strip.text.y = element_text(size=rel(.75)),        
        plot.title = element_text(hjust=0.5, size=rel(1.5))) +
  xlab('Treatment effect') + ylab('# of simulations')
ggsave(hajek_compare, file='../figures/hajek_compare.jpg', height=4, width=5, units='in')

counterfactual_hists <- rbind(
  control_reality %>% mutate(tate = control_booking_rate,
                             metric = 'booking_rate',
                             treatment = '-50% price',
                             group = 'Control') %>% dplyr::select(tate, metric, treatment, group),
  control_reality %>% mutate(tate = control_revenue_per_user,
                             metric = 'arpu',
                             treatment = '-50% price',
                             group = 'Control') %>% dplyr::select(tate, metric, treatment, group),
  control_reality %>% mutate(tate = control_booking_rate,
                             metric = 'booking_rate',
                             treatment = '-20% price',
                             group = 'Control') %>% dplyr::select(tate, metric, treatment, group),
  control_reality %>% mutate(tate = control_revenue_per_user,
                             metric = 'arpu',
                             treatment = '-20% price',
                             group = 'Control') %>% dplyr::select(tate, metric, treatment, group),
  treatment_reality %>% mutate(tate = treatment_booking_rate,
                               metric = 'booking_rate',
                               treatment = '-50% price',
                               group = 'Treatment') %>% dplyr::select(tate, metric, treatment, group),
  treatment_reality %>% mutate(tate = treatment_revenue_per_user,
                               metric = 'arpu',
                               treatment = '-50% price',
                               group = 'Treatment') %>% dplyr::select(tate, metric, treatment, group),
  treatment_reality_20 %>% mutate(tate = treatment_revenue_per_user,
                                  metric = 'arpu',
                                  treatment = '-20% price',
                                  group = 'Treatment') %>% dplyr::select(tate, metric, treatment, group),
  treatment_reality_20 %>% mutate(tate = treatment_booking_rate,
                                  metric = 'booking_rate',
                                  treatment = '-20% price',
                                  group = 'Treatment') %>% dplyr::select(tate, metric, treatment, group)
)

counterfactual_compare <- counterfactual_hists %>% 
  mutate(metric = ifelse(metric == 'arpu', 'Average revenue \nper listing', 'Booking rate'),
         `Counterfactual state` = group) %>% 
  ggplot(., aes(x=tate, fill=`Counterfactual state`)) + geom_histogram(alpha=.5, position='identity') + 
  facet_grid(treatment ~ metric, scales='free_x') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.text=element_text(size = rel(.4)),
        axis.text=element_text(size=rel(.5)),
        legend.title=element_text(size=rel(.75)),
        strip.text.x = element_text(size=rel(.75)),
        strip.text.y = element_text(size=rel(.75)),        
        plot.title = element_text(hjust=0.5, size=rel(1.5))) +
  xlab('Average outcome value') + ylab('# of simulations')
ggsave(counterfactual_compare, file='../figures/counterfactual_compare.jpg', height=4, width=5, units='in')

degree_plot <- data.frame(degree = degree(graph_distance_room_type_accom)) %>% 
  ggplot(., aes(x=degree)) + geom_histogram() + xlab('Degree') + ylab('Number of listings') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.text=element_text(size = rel(.4)),
        axis.text=element_text(size=rel(.5)),
        legend.title=element_text(size=rel(.75)),
        strip.text.x = element_text(size=rel(.75)),
        strip.text.y = element_text(size=rel(.75)),        
        plot.title = element_text(hjust=0.5, size=rel(1.5))) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))
ggsave(degree_plot, file='../figures/degree_distribution.jpg', height=4, width=5, units='in')

cluster_size_plot <- data.frame(cluster_size = as.numeric(table(clusters_distance_room_type_accom$membership))) %>%
  ggplot(., aes(x=cluster_size)) + geom_histogram() + xlab('Cluster size') + ylab('Number of clusters') + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.text=element_text(size = rel(.4)),
        axis.text=element_text(size=rel(.5)),
        legend.title=element_text(size=rel(.75)),
        strip.text.x = element_text(size=rel(.75)),
        strip.text.y = element_text(size=rel(.75)),        
        plot.title = element_text(hjust=0.5, size=rel(1.5))) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))
ggsave(cluster_size_plot, file='../figures/cluster_size.jpg', height=4, width=5, units='in')