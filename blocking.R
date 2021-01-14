setwd('~/Desktop/mit_15840_paper/')

load('df_for_blocking.Rdata')

blockTools::block(df_for_blocking, id.vars="cluster_assignment", 
                  block.vars = c("avg_reviews_scaled", "avg_satisfaction_scaled",
                                 "avg_bed_scaled",
                                 "avg_bath_scaled",
                                 "avg_minstay_scaled",
                                 "avg_lat",
                                 "avg_lon"))
