library(TSrepr)
library(ggplot2)
library(data.table)
library(cluster)
library(clusterCrit)

library('dtwclust')

library('TSclust')

library('dplyr')

library('microbenchmark')


### FILTERING MOTION == 0 data ####

microbenchmark(Motion_182_days_w_0 <- hourly_data_182 %>% filter(motion_total == 0),
               times = 24)


#### FILTERING DAYS WITH CONTINUOUS MOTION 0 DATA #####

Motion_182_days_w_0$Hour <- as.numeric(Motion_182_days_w_0$Hour)

Motion_182_contzero <- Motion_182_days_w_0[(1+(s<-c(0,cumsum(1-(diff(Motion_182_days_w_0$Hour)==1)))))%in%which(table(s)>=23),]

Days_with_Zero_Motion_182 <- (unique(Motion_182_contzero$day_index_num))

### REMOVING CONTINUOUS ZERO MOTION DAYS WITH THE MAIN DATA i.e. hourly_data_112 ####

dates <- as.POSIXct(c("2016-01-05",  "2016-01-06",  "2016-01-07",  "2016-03-31", "2016-05-17", "2016-05-18", "2016-06-13", "2016-06-14",
                      "2016-06-15", "2016-06-16", "2016-06-20", "2016-07-12", "2016-07-13", "2016-07-14", "2016-07-15", "2016-07-18",
                      "2016-07-19" ,"2016-07-20", "2016-07-21", "2016-07-22", "2016-08-11", "2016-08-12"))


A_182 <- hourly_data_182

A_182$Day_index <- as.Date(A_182$Day_index, tz="Europe/Paris")

A_182_Nonzerodays <- A_182[!as.Date(A_182$day_index_num) %in% as.Date(c("2016-02-21", "2016-02-22", "2016-02-26", "2016-02-27", "2016-06-05", "2016-07-03", "2016-07-17", "2016-07-27",
                                                                        "2016-07-28", "2016-07-29", "2016-07-30", "2016-07-31", "2016-08-06", "2016-08-09", "2016-08-10", "2016-08-11",
                                                                        "2016-08-12", "2016-08-13", "2016-08-14", "2016-08-15", "2016-08-17", "2016-08-18", "2016-08-19", "2016-08-25",
                                                                        "2016-08-26", "2016-08-27", "2016-10-20", "2016-10-21", "2016-10-22", "2016-10-23", "2016-10-24", "2016-10-31", 
                                                                        "2016-12-20", "2016-12-21", "2016-12-22", "2016-12-23", "2016-12-24", "2016-12-25",  "2016-12-28","2016-12-29", 
                                                                        "2016-12-30")), ]

A_182_Nonzerodays$day_index_num <- as.character(A_182_Nonzerodays$day_index_num)

A_182_Nonzerodays$day_index_num <- as.POSIXct(A_182_Nonzerodays$day_index_num)

#df_112 <- df_112[format(df_112$Day_index) != "2016-08-12", ]

A_182_Nonzerodays_weekdays <- filter(A_182_Nonzerodays, Weekday_weekend == 'weekday')

A_182_Nonzerodays_weekdays$motion_total <- as.numeric(A_182_Nonzerodays_weekdays$motion_total)

A_182_Nonzerodays_weekend <- filter(A_182_Nonzerodays, Weekday_weekend == 'weekend')

A_182_weekday_cluster_data <- dcast(A_182_Nonzerodays_weekdays, day_index_num~Hour, value.var = 'motion_total')

A_182_weekend_cluster_data <- dcast(A_182_Nonzerodays_weekend, day_index_num~Hour, value.var = 'motion_total')


A_182_weekday_cluster_data_matrix <- as.matrix(A_182_weekday_cluster_data[c(2:25)])

clusterings_wd_A_182_c1 <- lapply(c(2:10), function(x)
  pam(A_182_weekday_cluster_data_matrix, x))

DB_values_wd_A_182_c1 <- sapply(seq_along(clusterings_wd_A_182_c1), function(x) 
  intCriteria(A_182_weekday_cluster_data_matrix, as.integer(clusterings_wd_A_182_c1[[x]]$clustering),
              c("Dunn")))



ggplot(data.table(Clusters = 2:10, DBindex = unlist(DB_values_wd_A_182_c1)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()


dtw_cluster_wd_182_c1 = tsclust(A_182_weekday_cluster_data[c(2:25)], type="partitional",k=2, preproc = zscore, seed=123,
                                distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)
plot(dtw_cluster_wd_182_c1)

plot(dtw_cluster_wd_182_c1, type = "series", clus = 4L)
plot(dtw_cluster_wd_182_c1, type = "centroids", clus = 4L)


centroids_wd_182_c1 <- dtw_cluster_wd_182_c1@centroids

#centroids <- as.data.frame(centroids)

plot(centroids_wd_182_c1[[1]], type='l', col='red', ylim=c(-2, 4), xaxt="n", xlab='Hour')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24))
plot(centroids_wd_182_c1[[2]], type='l', col='brown')
plot(centroids_wd_182_c1[[3]], type='l', col='orange')
plot(centroids_wd_182_c1[[4]], type='l', col='dodgerblue')
plot(centroids_wd_182_c1[[5]], type='l', col='black')
plot(centroids_wd_182_c1[[6]], type='l', col='pink')
plot(centroids_wd_182_c1[[7]], type='l', col='purple', ylim=c(-1, 5))
plot(centroids_wd_182_c1[[8]], type='l', col='cyan')
plot(centroids_wd_182_c1[[9]], type='l', col='black')
plot(centroids_wd_182_c1[[10]], type='l', col='green')


### Weekend

A_182_Nonzerodays_weekend$Motion_total <- as.numeric(A_182_Nonzerodays_weekend$Motion_total)

A_182_weekend_cluster_data <- dcast(A_182_Nonzerodays_weekend, Day_index~Hour, value.var = 'Motion_total')

A_182_weekend_cluster_data_matrix <- as.matrix(A_182_weekend_cluster_data[c(2:25)])

clusterings_we_A_182_c1 <- lapply(c(2:10), function(x)
  pam(A_182_weekend_cluster_data_matrix, x))

DB_values_we_A_182_c1 <- sapply(seq_along(clusterings_we_A_182_c1), function(x) 
  intCriteria(A_182_weekend_cluster_data_matrix, as.integer(clusterings_we_A_182_c1[[x]]$clustering),
              c("Dunn")))



ggplot(data.table(Clusters = 2:10, DBindex = unlist(DB_values_we_A_182_c1)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()


dtw_cluster_we_182_c1 = tsclust(A_182_weekend_cluster_data[c(2:25)], type="partitional",k=4, preproc = zscore, 
                                distance="sbd",centroid = "shape",trace=T, na.rm=TRUE)
plot(dtw_cluster_we_182_c1)
centroids_we_182_c1 <- dtw_cluster_we_182_c1@centroids

#centroids <- as.data.frame(centroids)

plot(centroids_we_182_c1[[1]], type='l', col='red', ylim=c(-2, 4), xaxt="n", xlab='Hour')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(centroids_we_182_c1[[2]], type='l', col='brown')
lines(centroids_we_182_c1[[3]], type='l', col='orange')
lines(centroids_we_182_c1[[4]], type='l', col='dodgerblue')
lines(centroids_wd_182_c1[[5]], type='l', col='black')
lines(centroids_wd_182_c1[[6]], type='l', col='pink')
lines(centroids_wd_182_c1[[7]], type='l', col='purple', ylim=c(-1, 5))
lines(centroids_wd_182_c1[[8]], type='l', col='cyan')
lines(centroids_wd_182_c1[[9]], type='l', col='black')
lines(centroids_wd_182_c1[[10]], type='l', col='green')


#M_112_Weekday analysis

proper_hourly_profile_weekdays_112 <- A_112_Nonzerodays_weekdays %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), Motion_total_wd = mean(Motion_total))


proper_hourly_profile_weekdays_112[c(2,3)] <- round(proper_hourly_profile_weekdays_112[c(2,3)], digits = 0)

par(mar=c(5, 4, 4, 6) + 0.1)

#hourly_profile_weekdays$Hour <- as.numeric(hourly_profile_weekdays$Hour)

with(hourly_profile_weekdays_112, plot(Hour, Motion_total_wd, type='l', col='red'))

par(new=TRUE)

with(hourly_profile_weekdays_112, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(Motion_total), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))


#M_112_Weekend analysis

proper_hourly_profile_weekend_112 <- A_112_Nonzerodays_weekend %>%
  
  group_by(Hour) %>%
  
  summarise(P_tot_wd = mean(P_tot), Motion_total_wd = mean(Motion_total), CO2_tot_wd=mean(((CO2_LR+CO2_BR1+CO2_BR2+CO2_BR3)/4)))

plot(proper_hourly_profile_weekend_112$CO2_tot_wd, type='l')
plot(proper_hourly_profile_weekend_112$Motion_total_wd, type='l', col='red')


proper_hourly_profile_weekend_112[c(2,3)] <- round(proper_hourly_profile_weekend_112[c(2,3)], digits = 0)

par(mar=c(5, 4, 4, 6) + 0.1)

#hourly_profile_weekdays$Hour <- as.numeric(hourly_profile_weekdays$Hour)

with(hourly_profile_112_weekend, plot(Hour, Motion_total_wd, type='l', col='red'))

par(new=TRUE)

with(hourly_profile_112_weekend, plot(Hour, P_tot_wd, type='l', pch=16, axes=F,xlab=NA, ylab=NA, cex=1.2))
axis(side=4)
mtext(side=4, line=3, 'P_Tot')
legend("topleft",
       legend=c(expression(Motion_total), expression(P_Tot)),
       lty=c(1,0), pch=c(NA, 16), col=c("red", "black"))

#### WEEKDAY ANALYSIS #####

Motion_112_Cluster_data$Day <- weekdays(Motion_112_Cluster_data$Day_index)

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')


Motion_112_Cluster_data$wDay <- factor((weekdays(Motion_112_Cluster_data$Day_index) %in% weekdays1),
                                       
                                       levels=c(FALSE,TRUE), labels=c('weekend','weekday'))


Motion_112_weekday_c1 <- df_112_weekdays[c(1,2,3,4,34)]

Motion_112_weekday_c1$Motion_total <- as.numeric(Motion_112_weekday_c1$Motion_total)

Motion_112__wd_Cluster_data_c1 <- dcast(Motion_112_weekday_c1, Day_index~Hour, value.var = 'Motion_total')

Motion_112_wd_Matrix_c1 <- as.matrix(Motion_112__wd_Cluster_data_c1[c(2:25)])

#Motion_112_Matrix <- as.numeric(Motion_112_Matrix)

clusterings_wd_112_c1 <- lapply(c(2:11), function(x)
  pam(Motion_112_wd_Matrix_c1, x))

DB_values_wd_112_c1 <- sapply(seq_along(clusterings_wd_112_c1), function(x) 
  intCriteria(Motion_112_wd_Matrix_c1, as.integer(clusterings_wd_112_c1[[x]]$clustering),
              c("Davies_Bouldin")))



ggplot(data.table(Clusters = 2:11, DBindex = unlist(DB_values_wd_112_c1)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()


dtw_cluster_wd_112_c1 = tsclust(Motion_112__wd_Cluster_data_c1[c(2:25)], type="partitional",k=9, preproc = zscore, 
                                distance="dtw",centroid = "shape",trace=T, na.rm=TRUE)
plot(dtw_cluster_wd_112_c1)
centroids_wd_112_c1 <- dtw_cluster_wd_112_c1@centroids

#centroids <- as.data.frame(centroids)

plot(centroids_wd_112_c1[[1]], type='l', col='red', ylim=c(-2, 4), xaxt="n", xlab='Hour')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(centroids_wd_112_c1[[2]], type='l', col='brown')
lines(centroids_wd_112_c1[[3]], type='l', col='orange')
lines(centroids_wd_112_c1[[4]], type='l', col='dodgerblue')
lines(centroids_wd_112_c1[[5]], type='l', col='black')
lines(centroids_wd_112_c1[[6]], type='l', col='pink')
lines(centroids_wd_112_c1[[7]], type='l', col='purple', ylim=c(-1, 5))
lines(centroids_wd_112_c1[[8]], type='l', col='cyan')
lines(centroids_wd_112_c1[[9]], type='l', col='black')
lines(centroids_wd_112_c1[[10]], type='l', col='green')


#### WEEKEND ANALYSIS ####

Motion_112_weekend_c1 <- df_112_weekend[c(1,2,3,4,34)]

Motion_112_weekend_c1$Motion_total <- as.numeric(Motion_112_weekend_c1$Motion_total)

Motion_112__we_Cluster_data_c1 <- dcast(Motion_112_weekend_c1, Day_index~Hour, value.var = 'Motion_total')

Motion_112_we_Matrix_c1 <- as.matrix(Motion_112__we_Cluster_data_c1[c(2:25)])

#Motion_112_Matrix <- as.numeric(Motion_112_Matrix)

clusterings_we_112_c1 <- lapply(c(2:15), function(x)
  pam(Motion_112_we_Matrix_c1, x))

DB_values_we_112_c1 <- sapply(seq_along(clusterings_we_112_c1), function(x) 
  intCriteria(Motion_112_we_Matrix_c1, as.integer(clusterings_we_112_c1[[x]]$clustering),
              c("Davies_Bouldin")))



ggplot(data.table(Clusters = 2:15, DBindex = unlist(DB_values_we_112_c1)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()


dtw_cluster_we_112_c1 = tsclust(Motion_112__we_Cluster_data_c1[c(2:25)], type="partitional",k=12, preproc = zscore, 
                                distance="dtw",centroid = "shape",trace=T, na.rm=TRUE)
plot(dtw_cluster_we_112_c1)
centroids_we_112_c1 <- dtw_cluster_we_112_c1@centroids


plot(centroids_we_112_c1[[1]], type='l', col='red', ylim=c(-2, 4), xaxt="n", xlab='Hour')
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24))
lines(centroids_we_112_c1[[2]], type='l', col='brown')
lines(centroids_we_112_c1[[3]], type='l', col='orange')
lines(centroids_we_112_c1[[4]], type='l', col='dodgerblue')
lines(centroids_we_112_c1[[5]], type='l', col='black')
lines(centroids_we_112_c1[[6]], type='l', col='pink')
lines(centroids_we_112_c1[[7]], type='l', col='purple', ylim=c(-1, 5))
lines(centroids_we_112_c1[[8]], type='l', col='cyan')
lines(centroids_we_112_c1[[9]], type='l', col='black')
lines(centroids_we_112_c1[[10]], type='l', col='green')
