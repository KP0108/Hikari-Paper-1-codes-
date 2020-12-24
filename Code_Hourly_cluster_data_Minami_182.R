#### Energy Season analysis###



Func.season <- function(month){
  
  Winter <- c("1", "2","12")
  
  Spring <- c("3", "4", "5")
  
  Summer <- c("6","7","8")
  
  Autumn <- c("9","10","11")
  
  season_division <- c("Winter","Spring", "Summer", "Autumn")
  
  x <- vector()
  
  
  
  for(i in 1:length(month)){
    
    if(month[i] %in% Winter){
      
      x[i] = season_division[1]
      
    }else if(month[i] %in% Spring){
      
      x[i] = season_division[2]
      
    }else if(month[i] %in% Summer){
      
      x[i] = season_division[3]
      
    }else{
      
      x[i] = season_division[4]
      
    }
    
  }
  
  
  
  return(x)
  
}

##Day is seperated based on the occupancy & energy CPA

Func.Day_Energy <- function(time){
  
  Morning <- c("8","9", "10")
  
  Noon <- c("11", "12")
  
  Afternoon <- c("13", "14", "15", "16", "17", "18")
  
  Evening <- c("19", "20", "21", "22")
  
  Night <- c("23", "0", "1", "2", "3", "4", "5", "6")
  
  day_division <- c("Morning","Noon", "Afternoon", "Evening", "Night")
  
  x <- vector()
  
  for(i in 1:length(time)){
    
    if(time[i] %in% Morning){
      
      x[i]= day_division[1]
      
    }else if(time[i] %in% Noon){
      
      x[i] = day_division[2]
      
    }else if(time[i] %in% Afternoon){
      
      x[i] = day_division[3]
      
    }else if(time[i] %in% Evening){
      
      x[i] = day_division[4]
      
    }else{
      
      x[i] = day_division[5]
      
    }
    
  }
  
  
  
  return(x)
  
}

remove(Minami182_10min_index)

Minami182_10min_index <- read.csv("K:/Hikari/S/T4_O/182/Minami182_10min_index.csv")

Minami182_10min_index$Time <- as.POSIXct(strptime(Minami182_10min_index$Time, format = "%Y-%m-%d %H:%M"))

Minami182_10min_index <- Minami182_10min_index[,-9]

Minami182_10min_index$Day.period <- Func.Day_Energy(Minami182_10min_index$Hour)


Minami182_10min_index <- Minami182_10min_index[c(1:8, 151, 9:150)]

## Cluster_Energy data

library(dplyr)

Cluster_data <- hourly_data_all %>%
  
  group_by(day_index_num, Day_period) %>%
  
  summarise(mean=mean(power_total_2),
            min=min(power_total_2),
            max=max(power_total_2),
            sd=sd(power_total_2))


df_cluster=Func.reshape(Cluster_data, num_period = 5, num_feature = 4)

names(df_cluster) <- c("date", "Period 3_mean", "Period 3_min", "Peroid 3_max", "Period 3_sd", "Period 4_mean", "Period 4_min", "Period 4_max", "Period 4_sd", 
                       "Period 1_mean", "Period 1_min", "Period 1_max", "Period 1_sd", "Period 5_mean", "Period 5_min", "Period 5_max", "Period 5_sd", "Period 2_mean", "Period 2_min", "Period 2_max", "Period 2_sd")


Func.transform <- function(df){
  num_row <- nrow(df)
  for(i in 2:ncol(df)){
    x_min <- min(df[i])
    x_max <- max(df[i])
    
    min_max <- x_max-x_min
    
    min_vec <- rep(x_min, num_row)
    
    df[i] <- (df[i]-min_vec)/min_max
    
  }
  
  return(df)
}

#df_cluster <- df_cluster[-332,]
df_cluster_transform <- Func.transform(df_cluster)

####### Motion_Cluster_data######

Func.Day_Motion <- function(time){
  
  Morning <- c("08","09", "10")
  
  Noon <- c("11", "12")
  
  Afternoon <- c("13", "14", "15", "16", "17", "18")
  
  Evening <- c("19", "20", "21", "22")
  
  Night <- c("23", "00", "01", "02", "03", "04", "05", "06", "07")
  
  day_division <- c("Morning","Noon", "Afternoon", "Evening", "Night")
  
  x <- vector()
  
  for(i in 1:length(time)){
    
    if(time[i] %in% Morning){
      
      x[i]= day_division[1]
      
    }else if(time[i] %in% Noon){
      
      x[i] = day_division[2]
      
    }else if(time[i] %in% Afternoon){
      
      x[i] = day_division[3]
      
    }else if(time[i] %in% Evening){
      
      x[i] = day_division[4]
      
    }else{
      
      x[i] = day_division[5]
      
    }
    
  }
  
  
  
  return(x)
  
}

library(dplyr)

minami182_motion_tot <- hourly_data_all %>%
  
  mutate(motion_tot = KMVL1201+KMVL1202+KMVL1203+KMVL1204+KMVL1205+KMVL1206+KMVL1207+KMVL1208+KMVL1209+KMVL120A+KMVL120B+KMVL120C+KMVL120D+KMVL120E)


occupancy_event <- hourly_data_all[c(1,2,17,19)]

occupancy_event$day_index_num <- format(occupancy_event$Time,"%Y-%m-%d")

occupancy_event$Day_period <- Func.Day_Motion(occupancy_event$Hour)

occupancy_event <- occupancy_event %>%
  group_by(day_index_num,Day_period) %>%
  summarise(num_event_total = sum(motion_total))

occupancy_event_rs <- Func.reshape(occupancy_event,num_period = 5, num_feature = 1)

#occupancy_event_rs <- occupancy_event_rs[-332,]

occupancy_event_transform <- Func.transform(occupancy_event_rs)

minami182_motion_tot$day_index_num <- format(minami182_motion_tot$Time,"%Y-%m-%d")

##Movements in August###

occupancy_event_2 <- minami182_motion_tot %>%
  group_by(day_index_num) %>%
  summarise(num_event_total = sum(motion_tot))
occupancy_event_2 <- occupancy_event_2[-332,]
occupancy_event_2$day_index_num <- as.POSIXct(occupancy_event_2$day_index_num )

plot(x=occupancy_event_2$day_index_num[183:211],y=occupancy_event_2$num_event_total[183:211], type="l", xlab = "Date", ylab = "Total no.of movements", main ="Movements in August - Minami 182")


#  period 1:Morning

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[4]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movement_Morning period")
box()

set.seed(123)
occ_kmeans_results <- kmeans(occupancy_event_transform[4],centers = 4, nstart = 50)


occ_kmeans_center <- occ_kmeans_results[["centers"]]

occ_kmeans_center <- t(occ_kmeans_center)
occ_kmeans_center <- as.data.frame(occ_kmeans_center)
x_1 <- occ_kmeans_center$`1`
x_2 <- occ_kmeans_center$`2`
x_3 <- occ_kmeans_center$`3`
x_4 <- occ_kmeans_center$`4`

x_period <- rep("Period 1",4)

occ_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3, x_4))
colnames(occ_kmeans_center_new) <- c("Period", "Center")
occ_kmeans_center_new$cluster.ID <- c(1:4)


occupancy_event_rs$cluster <- occ_kmeans_results[["cluster"]]

library(ggplot2)
ggplot(occ_kmeans_center_new, aes(cluster.ID,Period)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") + theme(text=element_text(size=15))+
  xlab("Cluster ID")+ ggtitle("Occupancy Cluster_Period 1")+geom_text(aes(label = round(Center, 2)), cex=5.0)

b_1 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==1)]
b_2 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==2)]
b_3 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==3)]
b_4 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==4)]


#  Energy


#Elbow Method
k.max <- 15
data <- df_cluster_transform[10:13]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0, 50),
     main = "Power consumption_Morning period")
box()

set.seed(123)
power_kmeans_results <- kmeans(df_cluster_transform[10:13],centers = 4, nstart = 50)

power_kmeans_center <- power_kmeans_results[["centers"]]

power_kmeans_center <- t(power_kmeans_center)
power_kmeans_center <- as.data.frame(power_kmeans_center)


x_1 <- power_kmeans_center$`1`
x_2 <- power_kmeans_center$`2`
x_3 <- power_kmeans_center$`3`
x_4 <- power_kmeans_center$`4`


x_period <- rownames(power_kmeans_center)
x_period <- rep(x_period,4)


power_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3, x_4))
colnames(power_kmeans_center_new) <- c("Features","Center")
power_kmeans_center_new$cluster.ID <- rep(c(1:4),each=4)


ggplot(power_kmeans_center_new, aes(cluster.ID,Features)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +theme(text=element_text(size=15))+
  xlab("Cluster ID")+ggtitle("Energy consumption Cluster_Period 1")+geom_text(aes(label = round(Center, 2)), cex=5.0)

df_cluster_transform$cluster <- power_kmeans_results[["cluster"]]

df_cluster_transform$date <- as.character(df_cluster_transform$date)

a_1 <- df_cluster_transform$date[which(df_cluster_transform$cluster==1)]
a_2 <- df_cluster_transform$date[which(df_cluster_transform$cluster==2)]
a_3 <- df_cluster_transform$date[which(df_cluster_transform$cluster==3)]
a_4 <- df_cluster_transform$date[which(df_cluster_transform$cluster==4)]


length(which(b_1 %in% a_1))
length(which(b_1 %in% a_2))
length(which(b_1 %in% a_3))
length(which(b_1 %in% a_4))

length(which(b_2 %in% a_1))
length(which(b_2 %in% a_2))
length(which(b_2 %in% a_3))
length(which(b_2 %in% a_4))



length(which(b_3 %in% a_1))
length(which(b_3 %in% a_2))
length(which(b_3 %in% a_3))
length(which(b_3 %in% a_4))


length(which(b_4 %in% a_1))
length(which(b_4 %in% a_2))
length(which(b_4 %in% a_3))
length(which(b_4 %in% a_4))


#  period 2:Noon (11, 12)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[6]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 50 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movement_Noon period")
box()

set.seed(123)
occ_kmeans_results <- kmeans(occupancy_event_transform[6],centers = 4, nstart = 50)


occ_kmeans_center <- occ_kmeans_results[["centers"]]

occ_kmeans_center <- t(occ_kmeans_center)
occ_kmeans_center <- as.data.frame(occ_kmeans_center)
x_1 <- occ_kmeans_center$`1`
x_2 <- occ_kmeans_center$`2`
x_3 <- occ_kmeans_center$`3`
x_4 <- occ_kmeans_center$`4`

x_period <- rep("Noon",4)

occ_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3, x_4))
colnames(occ_kmeans_center_new) <- c("Period","Center")
occ_kmeans_center_new$cluster.ID <- c(1:4)


occupancy_event_rs$cluster <- occ_kmeans_results[["cluster"]]


library(ggplot2)
ggplot(occ_kmeans_center_new, aes(cluster.ID,Period)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") + theme(text=element_text(size=15))+
  xlab("Cluster ID")+ ggtitle("Occupancy Cluster_Period 2")+geom_text(aes(label = round(Center, 2)), cex=5.0)


b_1 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==1)]
b_2 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==2)]
b_3 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==3)]
b_4 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==4)]


#  Energy


#Elbow Method
k.max <- 15
data <- df_cluster_transform[18:21]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,50),
     main = "Power consumption_Noon period")
box()

set.seed(123)
power_kmeans_results <- kmeans(df_cluster_transform[18:21],centers = 4, nstart = 50)

power_kmeans_center <- power_kmeans_results[["centers"]]

power_kmeans_center <- t(power_kmeans_center)
power_kmeans_center <- as.data.frame(power_kmeans_center)


x_1 <- power_kmeans_center$`1`
x_2 <- power_kmeans_center$`2`
x_3 <- power_kmeans_center$`3`
x_4 <- power_kmeans_center$`4`


x_period <- rownames(power_kmeans_center)
x_period <- rep(x_period,4)


power_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3, x_4))
colnames(power_kmeans_center_new) <- c("Features","Center")
power_kmeans_center_new$cluster.ID <- rep(c(1:4),each=4)


ggplot(power_kmeans_center_new, aes(cluster.ID,Features)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +theme(text=element_text(size=15))+
  xlab("Cluster ID")+ggtitle("Energy consumption Cluster_Period 2")+geom_text(aes(label = round(Center, 2)), cex=5.0)


df_cluster_transform$cluster <- power_kmeans_results[["cluster"]]

a_1 <- df_cluster_transform$date[which(df_cluster_transform$cluster==1)]
a_2 <- df_cluster_transform$date[which(df_cluster_transform$cluster==2)]
a_3 <- df_cluster_transform$date[which(df_cluster_transform$cluster==3)]
a_4 <- df_cluster_transform$date[which(df_cluster_transform$cluster==4)]


length(which(b_1 %in% a_1))
length(which(b_1 %in% a_2))
length(which(b_1 %in% a_3))
length(which(b_1 %in% a_4))

length(which(b_2 %in% a_1))
length(which(b_2 %in% a_2))
length(which(b_2 %in% a_3))
length(which(b_2 %in% a_4))

length(which(b_3 %in% a_1))
length(which(b_3 %in% a_2))
length(which(b_3 %in% a_3))
length(which(b_3 %in% a_4))


length(which(b_4 %in% a_1))
length(which(b_4 %in% a_2))
length(which(b_4 %in% a_3))
length(which(b_4 %in% a_4))


#  period 3:Afternoon (13, 14, 15, 16, 17, 18)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[2]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 30 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,25),
     main = "Occupancy movement_Afternoon period")
box()

set.seed(123)
occ_kmeans_results <- kmeans(occupancy_event_transform[2],centers = 4, nstart = 50)


occ_kmeans_center <- occ_kmeans_results[["centers"]]

occ_kmeans_center <- t(occ_kmeans_center)
occ_kmeans_center <- as.data.frame(occ_kmeans_center)

x_1 <- occ_kmeans_center$`1`
x_2 <- occ_kmeans_center$`2`
x_3 <- occ_kmeans_center$`3`
x_4 <- occ_kmeans_center$`4`

x_period <- rep("Afternoon",4)

occ_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3, x_4))
colnames(occ_kmeans_center_new) <- c("Period","Center")
occ_kmeans_center_new$cluster.ID <- c(1:4)


occupancy_event_rs$cluster <- occ_kmeans_results[["cluster"]]


library(ggplot2)
ggplot(occ_kmeans_center_new, aes(cluster.ID,Period)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") + theme(text=element_text(size=15))+
  xlab("Cluster ID")+ ggtitle("Occupancy Cluster_Period 3")+geom_text(aes(label = round(Center, 2)), cex=5.0)

b_1 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==1)]
b_2 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==2)]
b_3 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==3)]
b_4 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==4)]


#  Energy


#Elbow Method
k.max <- 15
data <- df_cluster_transform[2:5]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0, 65),
     main = "Power consumption_Afternoon period")
box()

set.seed(123)
power_kmeans_results <- kmeans(df_cluster_transform[2:5],centers = 4, nstart = 50)

power_kmeans_center <- power_kmeans_results[["centers"]]

power_kmeans_center <- t(power_kmeans_center)
power_kmeans_center <- as.data.frame(power_kmeans_center)


x_1 <- power_kmeans_center$`1`
x_2 <- power_kmeans_center$`2`
x_3 <- power_kmeans_center$`3`
x_4 <- power_kmeans_center$`4`


x_period <- rownames(power_kmeans_center)
x_period <- rep(x_period,4)


power_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3,x_4))
colnames(power_kmeans_center_new) <- c("Features","Center")
power_kmeans_center_new$cluster.ID <- rep(c(1:4),each=4)

library(ggplot2)
ggplot(power_kmeans_center_new, aes(cluster.ID,Features)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +theme(text=element_text(size=15))+
  xlab("Cluster ID")+ggtitle("Energy consumption Cluster_Period 3")+geom_text(aes(label = round(Center, 2)), cex=5.0)

df_cluster_transform$cluster <- power_kmeans_results[["cluster"]]

a_1 <- df_cluster_transform$date[which(df_cluster_transform$cluster==1)]
a_2 <- df_cluster_transform$date[which(df_cluster_transform$cluster==2)]
a_3 <- df_cluster_transform$date[which(df_cluster_transform$cluster==3)]
a_4 <- df_cluster_transform$date[which(df_cluster_transform$cluster==4)]


length(which(b_1 %in% a_1))
length(which(b_1 %in% a_2))
length(which(b_1 %in% a_3))
length(which(b_1 %in% a_4))

length(which(b_2 %in% a_1))
length(which(b_2 %in% a_2))
length(which(b_2 %in% a_3))
length(which(b_2 %in% a_4))



length(which(b_3 %in% a_1))
length(which(b_3 %in% a_2))
length(which(b_3 %in% a_3))
length(which(b_3 %in% a_4))


length(which(b_4 %in% a_1))
length(which(b_4 %in% a_2))
length(which(b_4 %in% a_3))
length(which(b_4 %in% a_4))

#  period 4: Evening (19, 20, 21, 22)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[3]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0, 10),
     main = "Occupancy movement_Evening period")

box()
set.seed(123)
occ_kmeans_results <- kmeans(occupancy_event_transform[3],centers = 4, nstart = 50)


occ_kmeans_center <- occ_kmeans_results[["centers"]]

occ_kmeans_center <- t(occ_kmeans_center)
occ_kmeans_center <- as.data.frame(occ_kmeans_center)
x_1 <- occ_kmeans_center$`1`
x_2 <- occ_kmeans_center$`2`
x_3 <- occ_kmeans_center$`3`
x_4 <- occ_kmeans_center$`4`

x_period <- rep("Evening",4)

occ_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3, x_4))
colnames(occ_kmeans_center_new) <- c("Period","Center")
occ_kmeans_center_new$cluster.ID <- c(1:4)


occupancy_event_rs$cluster <- occ_kmeans_results[["cluster"]]


library(ggplot2)
ggplot(occ_kmeans_center_new, aes(cluster.ID,Period)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +
  xlab("Cluster ID")+ggtitle("Occupancy movemenmt_Cluster_Evening period")+geom_text(aes(label=round(Center,2)))

b_1 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==1)]
b_2 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==2)]
b_3 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==3)]
b_4 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==4)]

#  Energy


#Elbow Method
k.max <- 15
data <- df_cluster_transform[6:9]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,50),
     main="Power consumption_Evening period")
box()

set.seed(123)
power_kmeans_results <- kmeans(df_cluster_transform[6:9],centers = 4, nstart = 50)

power_kmeans_center <- power_kmeans_results[["centers"]]

power_kmeans_center <- t(power_kmeans_center)
power_kmeans_center <- as.data.frame(power_kmeans_center)


x_1 <- power_kmeans_center$`1`
x_2 <- power_kmeans_center$`2`
x_3 <- power_kmeans_center$`3`
x_4 <- power_kmeans_center$`4`


x_period <- rownames(power_kmeans_center)
x_period <- rep(x_period,4)


power_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3,x_4))
colnames(power_kmeans_center_new) <- c("Features","Center")
power_kmeans_center_new$cluster.ID <- rep(c(1:4),each=4)


ggplot(power_kmeans_center_new, aes(cluster.ID,Features)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +
  xlab("Cluster ID")+ggtitle("Power consumption_Cluster_Evening period")+geom_text(aes(label=round(Center,2)))

df_cluster_transform$cluster <- power_kmeans_results[["cluster"]]

a_1 <- df_cluster_transform$date[which(df_cluster_transform$cluster==1)]
a_2 <- df_cluster_transform$date[which(df_cluster_transform$cluster==2)]
a_3 <- df_cluster_transform$date[which(df_cluster_transform$cluster==3)]
a_4 <- df_cluster_transform$date[which(df_cluster_transform$cluster==4)]


length(which(b_1 %in% a_1))
length(which(b_1 %in% a_2))
length(which(b_1 %in% a_3))
length(which(b_1 %in% a_4))

length(which(b_2 %in% a_1))
length(which(b_2 %in% a_2))
length(which(b_2 %in% a_3))
length(which(b_2 %in% a_4))



length(which(b_3 %in% a_1))
length(which(b_3 %in% a_2))
length(which(b_3 %in% a_3))
length(which(b_3 %in% a_4))


length(which(b_4 %in% a_1))
length(which(b_4 %in% a_2))
length(which(b_4 %in% a_3))
length(which(b_4 %in% a_4))


#  period 5: Night (23, 24, 01, 02, 03, 04, 05, 06, 07)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[5]
wss <- sapply(1:k.max, 
               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movemenmt_Night period")
box()

set.seed(123)
occ_kmeans_results <- kmeans(occupancy_event_transform[5],centers = 3, nstart = 50)


occ_kmeans_center <- occ_kmeans_results[["centers"]]

occ_kmeans_center <- t(occ_kmeans_center)
occ_kmeans_center <- as.data.frame(occ_kmeans_center)
x_1 <- occ_kmeans_center$`1`
x_2 <- occ_kmeans_center$`2`
x_3 <- occ_kmeans_center$`3`
x_4 <- occ_kmeans_center$`4`


x_period <- rep("LateNight",3)

occ_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3))
colnames(occ_kmeans_center_new) <- c("Period","Center")
occ_kmeans_center_new$cluster.ID <- c(1:3)


occupancy_event_rs$cluster <- occ_kmeans_results[["cluster"]]


library(ggplot2)
ggplot(occ_kmeans_center_new, aes(cluster.ID,Period)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +
  xlab("Cluster ID")+ggtitle("Occupancy movement_Cluster_Night period")+geom_text(aes(label=round(Center,2)))

b_1 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==1)]
b_2 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==2)]
b_3 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==3)]
b_4 <- occupancy_event_rs$day_index_num[which(occupancy_event_rs$cluster==4)]

#  Energy


#Elbow Method
k.max <- 15
data <- df_cluster_transform[14:17]
wss <- sapply(1:k.max, 
                function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,60),
     main="Power consumption_Night period")
box()

set.seed(123)
power_kmeans_results <- kmeans(df_cluster_transform[14:17],centers = 3, nstart = 50)

power_kmeans_center <- power_kmeans_results[["centers"]]

power_kmeans_center <- t(power_kmeans_center)
power_kmeans_center <- as.data.frame(power_kmeans_center)


x_1 <- power_kmeans_center$`1`
x_2 <- power_kmeans_center$`2`
x_3 <- power_kmeans_center$`3`
x_4 <- power_kmeans_center$`4`


x_period <- rownames(power_kmeans_center)
x_period <- rep(x_period,3)


power_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3))
colnames(power_kmeans_center_new) <- c("Features","Center")
power_kmeans_center_new$cluster.ID <- rep(c(1:3),each=4)


ggplot(power_kmeans_center_new, aes(cluster.ID,Features)) + 
  geom_tile(aes(fill=Center), colour="white")+
  scale_fill_gradient(low="white", high = "Orange") +
  xlab("Cluster ID")+ggtitle("Power consumption_Cluster_Night period")+geom_text(aes(label=round(Center,2)))

df_cluster_transform$cluster <- power_kmeans_results[["cluster"]]

a_1 <- df_cluster_transform$date[which(df_cluster_transform$cluster==1)]
a_2 <- df_cluster_transform$date[which(df_cluster_transform$cluster==2)]
a_3 <- df_cluster_transform$date[which(df_cluster_transform$cluster==3)]
a_4 <- df_cluster_transform$date[which(df_cluster_transform$cluster==4)]


length(which(b_1 %in% a_1))
length(which(b_1 %in% a_2))
length(which(b_1 %in% a_3))
length(which(b_1 %in% a_4))

length(which(b_2 %in% a_1))
length(which(b_2 %in% a_2))
length(which(b_2 %in% a_3))
length(which(b_2 %in% a_4))


length(which(b_3 %in% a_1))
length(which(b_3 %in% a_2))
length(which(b_3 %in% a_3))
length(which(b_3 %in% a_4))


# Validation
#Compute clValid

library(clValid)

data <- occupancy_event_transform[2]
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(data, nClust = 2:8,
                  clMethods = clmethods, validation = "internal")
summary(intern)

plot(intern)

# Validation
#K-means
library(clValid)

data <- df_cluster_transform[2:5]
clmethods <- c("kmeans")
intern <- clValid(data, nClust = 2:8,
                  clMethods = clmethods, validation = "internal")
summary(intern)

plot(intern, main="Evening period_Energy")


library(wskm)

ewkmcluster <- ewkm(df_cluster_transform[2:5], 4, 0.5, 100)
plot.ewkm()
levelplot(ewkmcluster)


#### ARM####

Minami_182_Motion$index_num <- rep(seq(1:47664),each=10)

library(dplyr)

Minami_182_Motion_10min <- Minami_182_Motion %>%
  group_by(index_num) %>%
  summarise(motion_LR1 = sum(KMVL1201),
            motion_LR2 = sum(KMVL1202),
            motion_Kit = sum(KMVL1203),
            motion_COR1 = sum(KMVL1204),
            motion_COR2 = sum(KMVL1205),
            motion_COR3 = sum(KMVL1206),
            motion_COR4 = sum(KMVL1207),
            motion_COR5 = sum(KMVL1208),
            motion_Bed1 = sum(KMVL1209),
            motion_Bed2 = sum(KMVL120A),
            motion_Bed3 = sum(KMVL120B),
            motion_Bath1 = sum(KMVL120C),
            motion_Bath2 = sum(KMVL120D),
            motion_Toilet = sum(KMVL120E))

Minami_182_Motion_10min$Time <- Minami182_10min_index$Time
Minami_182_Motion_10min <- Minami_182_Motion_10min[-1]
Minami_182_Motion_10min <- Minami_182_Motion_10min[c(15,1:14)]

Minami_182_Motion_10min$motion_LR1[which(Minami_182_Motion_10min$motion_LR1>1)] <- 1
Minami_182_Motion_10min$motion_LR2[which(Minami_182_Motion_10min$motion_LR2>1)] <- 1
Minami_182_Motion_10min$motion_Kit[which(Minami_182_Motion_10min$motion_Kit>1)] <- 1
Minami_182_Motion_10min$motion_COR1[which(Minami_182_Motion_10min$motion_COR1>1)] <- 1
Minami_182_Motion_10min$motion_COR2[which(Minami_182_Motion_10min$motion_COR2>1)] <- 1
Minami_182_Motion_10min$motion_COR3[which(Minami_182_Motion_10min$motion_COR3>1)] <- 1
Minami_182_Motion_10min$motion_COR4[which(Minami_182_Motion_10min$motion_COR4>1)] <- 1
Minami_182_Motion_10min$motion_COR5[which(Minami_182_Motion_10min$motion_COR5>1)] <- 1
Minami_182_Motion_10min$motion_Bed1[which(Minami_182_Motion_10min$motion_Bed1>1)] <- 1
Minami_182_Motion_10min$motion_Bed2[which(Minami_182_Motion_10min$motion_Bed2>1)] <- 1
Minami_182_Motion_10min$motion_Bed3[which(Minami_182_Motion_10min$motion_Bed3>1)] <- 1
Minami_182_Motion_10min$motion_Bath1[which(Minami_182_Motion_10min$motion_Bath1>1)] <- 1
Minami_182_Motion_10min$motion_Bath2[which(Minami_182_Motion_10min$motion_Bath2>1)] <- 1
Minami_182_Motion_10min$motion_Toilet[which(Minami_182_Motion_10min$motion_Toilet>1)] <- 1


Minami_182_Motion_10min$Hour <- Minami182_10min_index$Hour
Minami_182_Motion_10min$Day.period <- Func.Day_Energy(Minami_182_Motion_10min$Hour)


Minami_182_Motion_10min <- Minami_182_Motion_10min %>%
  mutate(motion_total= motion_LR1+motion_LR2++motion_Kit+motion_COR1+motion_COR2+motion_COR3+motion_COR4+motion_COR5+motion_Bed1+motion_Bed2+motion_Bed3+motion_Bath1+motion_Bath2+motion_Toilet)

minami_182_Light_processed <- Minami182_10min_index[c(3:11,99)]

minami_182_indoor_processed <- Minami182_10min_index[c(3:23)]

minami_182_power_processed <- Minami182_10min_index[c(3:11,132)]

minami_182_indoor_processed <- minami_182_indoor_processed %>%
  mutate(averageCO2 = (KTCO1144+KTCO1145+KTCO1146+KTCO1147)/4)


#################### MORNING PERIOD############

data_motion_Morning <- hourly_data_all[which(hourly_data_all$Day_period=="Morning"),c(1:2, 20, 93, 94, 96, 97, 3:17)]

data_power_Morning <- hourly_data_all[which(hourly_data_all$Day_period=="Morning"),c(21:40, 56:73)]

#data_CO2_Morning <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Morning"),22]


data_light_Morning <- hourly_data_all[which(hourly_data_all$Day_period=="Morning"),c(41:55)]


data_morning <- cbind(data_motion_Morning,data_light_Morning,data_power_Morning)


#data_morning$day_index_num <- format(data_morning$Time,"%Y-%m-%d")


#data_morning$season <- Func.season(data_morning$Month)


data_morning_H <- data_morning[which(data_morning$day_index_num %in% b_2),]
data_morning_L <- data_morning[which(data_morning$day_index_num %in% b_3),]
data_morning_VL <- data_morning[which(data_morning$day_index_num %in% b_1),]
data_morning_M <- data_morning[which(data_morning$day_index_num %in% b_4),]



############## High activity################

library("arc")


Morn_L_bins_Power <- discretizeUnsupervised(data_morning_H$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Frequency")

Morn_L_bins_Light <- discretizeUnsupervised(data_morning_H$light_total, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Frequency")

#Morn_L_bins_CO2 <- discretizeUnsupervised(data_morning_H$data_CO2_Morning, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          #categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_H$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_morning_H$motion_label <- 0

data_morning_H$motion_label[which(data_morning_H$motion_total==0)] <- "no movement"

#data_morning_M$motion_label[which(data_morning_M$motion_total>=1)] <- " more than movement"

data_morning_H$motion_label[which(data_morning_H$motion_total >=1 & data_morning_H$motion_total <=106)] <- "Less movement"

data_morning_H$motion_label[which(data_morning_H$motion_total >=107)] <- "frequent movement"



data_morning_H$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_morning_H$power_label <- Morn_L_bins_Power[["Disc.data"]]


#data_morning_H$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_H$motion_label <- Morn_L_bins_motion[["Disc.data"]]


#data_morning_H <- data_morning[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_morning_H, file = "data_morning_High.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_2[which(b_2 %in% a_1)]

ind_morn_2 <- b_2[which(b_2 %in% a_2)]

ind_morn_3 <- b_2[which(b_2 %in% a_3)]

ind_morn_4 <- b_2[which(b_2 %in% a_4)]



data_morning_H_1 <- data_morning_H[which(data_morning_H$day_index_num %in% ind_morn_1),]

data_morning_H_2 <- data_morning_H[which(data_morning_H$day_index_num %in% ind_morn_2),]

data_morning_H_3 <- data_morning_H[which(data_morning_H$day_index_num %in% ind_morn_3),]

data_morning_H_4 <- data_morning_H[which(data_morning_H$day_index_num %in% ind_morn_4),]




write.csv(data_morning_H_1,"data_morning_H_1.csv")

write.csv(data_morning_H_2,"data_morning_H_2.csv")

write.csv(data_morning_H_3,"data_morning_H_3.csv")

write.csv(data_morning_H_4,"data_morning_H_4.csv")


############## Low activity################

Morn_L_bins_Power <- discretizeUnsupervised(data_morning_L$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Frequency")

Morn_L_bins_Light <- discretizeUnsupervised(data_morning_L$light_total, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Frequency")

#Morn_L_bins_CO2 <- discretizeUnsupervised(data_morning_L$data_CO2_Morning, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          #categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_morning_L$motion_label <- 0

data_morning_L$motion_label[which(data_morning_L$motion_total==0)] <- "no movement"

#data_morning_L$motion_label[which(data_morning_L$motion_total==1)] <- " one movement"

data_morning_L$motion_label[which(data_morning_L$motion_total >=1 & data_morning_L$motion_total <=48)] <- "Less movement"

data_morning_L$motion_label[which(data_morning_L$motion_total >=49)] <- "frequent movement"



data_morning_L$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_morning_L$power_label <- Morn_L_bins_Power[["Disc.data"]]


#data_morning_L$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_L$motion_label <- Morn_L_bins_motion[["Disc.data"]]


#data_morning <- data_morning[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_morning_L, file = "data_morning_Low.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_3[which(b_3 %in% a_1)]

ind_morn_2 <- b_3[which(b_3 %in% a_2)]

ind_morn_3 <- b_3[which(b_3 %in% a_3)]

ind_morn_4 <- b_3[which(b_3 %in% a_4)]



data_morning_L_1 <- data_morning_L[which(data_morning_L$day_index_num %in% ind_morn_1),]

data_morning_L_2 <- data_morning_L[which(data_morning_L$day_index_num %in% ind_morn_2),]

data_morning_L_3 <- data_morning_L[which(data_morning_L$day_index_num %in% ind_morn_3),]

data_morning_L_4 <- data_morning_L[which(data_morning_L$day_index_num %in% ind_morn_4),]




write.csv(data_morning_L_1,"data_morning_L_1.csv")

write.csv(data_morning_L_2,"data_morning_L_2.csv")

write.csv(data_morning_L_3,"data_morning_L_3.csv")

write.csv(data_morning_L_4,"data_morning_L_4.csv")


############## Medium activity################

library("arc")


Morn_L_bins_Power <- discretizeUnsupervised(data_morning_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Frequency")

Morn_L_bins_Light <- discretizeUnsupervised(data_morning_M$data_light_Morning, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Frequency")

Morn_L_bins_CO2 <- discretizeUnsupervised(data_morning_M$data_CO2_Morning, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_M$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")


data_morning_M$motion_label <- 0

data_morning_M$motion_label[which(data_morning_M$motion_total==0)] <- "no movement"

#data_morning_M$motion_label[which(data_morning_M$motion_total>=1)] <- " more than movement"

data_morning_M$motion_label[which(data_morning_M$motion_total >=1 & data_morning_M$motion_total <=5)] <- "Less movement"

data_morning_M$motion_label[which(data_morning_M$motion_total >=6)] <- "frequent movement"



data_morning_M$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_morning_M$power_label <- Morn_L_bins_Power[["Disc.data"]]


data_morning_M$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_M$motion_label <- Morn_L_bins_motion[["Disc.data"]]


data_morning <- data_morning[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_morning_M, file = "data_morning_Medium.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_4[which(b_4 %in% a_1)]

ind_morn_2 <- b_4[which(b_4 %in% a_2)]

ind_morn_3 <- b_4[which(b_4 %in% a_3)]

ind_morn_4 <- b_4[which(b_4 %in% a_4)]



data_morning_M_1 <- data_morning_M[which(data_morning_M$day_index_num %in% ind_morn_1),]

data_morning_M_2 <- data_morning_M[which(data_morning_M$day_index_num %in% ind_morn_2),]

data_morning_M_3 <- data_morning_M[which(data_morning_M$day_index_num %in% ind_morn_3),]

data_morning_M_4 <- data_morning_M[which(data_morning_M$day_index_num %in% ind_morn_4),]




write.csv(data_morning_M_1,"data_morning_M_1.csv")

write.csv(data_morning_M_2,"data_morning_M_2.csv")

write.csv(data_morning_M_3,"data_morning_M_3.csv")

write.csv(data_morning_M_4,"data_morning_M_4.csv")


############## VeryLow activity################

library("arc")


Morn_L_bins_Power <- discretizeUnsupervised(data_morning_VL$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Morn_L_bins_Light <- discretizeUnsupervised(data_morning_VL$light_total, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

#Morn_L_bins_CO2 <- discretizeUnsupervised(data_morning_VL$data_CO2_Morning, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          #categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_VL$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

#data_morning_VL$motion_label <- 0

data_morning_VL$motion_label[which(data_morning_VL$motion_total==0)] <- "no movement"

data_morning_VL$motion_label[which(data_morning_VL$motion_total==1)] <- "one movement"

#data_morning_VL$motion_label[which(data_morning_VL$motion_total >=1 & data_morning_VL$motion_total <=4)] <- "Less movement"

data_morning_VL$motion_label[which(data_morning_VL$motion_total >1)] <- "frequent movement"



data_morning_VL$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_morning_VL$power_label <- Morn_L_bins_Power[["Disc.data"]]


#data_morning_VL$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_VL$motion_label <- Morn_L_bins_motion[["Disc.data"]]


#data_morning <- data_morning[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_morning_VL, file = "data_morning_VLow.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_1[which(b_1 %in% a_1)]

ind_morn_2 <- b_1[which(b_1 %in% a_2)]

ind_morn_3 <- b_1[which(b_1 %in% a_3)]

ind_morn_4 <- b_1[which(b_1 %in% a_4)]



data_morning_VL_1 <- data_morning_VL[which(data_morning_VL$day_index_num %in% ind_morn_1),]

data_morning_VL_2 <- data_morning_VL[which(data_morning_VL$day_index_num %in% ind_morn_2),]

data_morning_VL_3 <- data_morning_VL[which(data_morning_VL$day_index_num %in% ind_morn_3),]

data_morning_VL_4 <- data_morning_VL[which(data_morning_VL$day_index_num %in% ind_morn_4),]




write.csv(data_morning_VL_1,"data_morning_VL_1.csv")

write.csv(data_morning_VL_2,"data_morning_VL_2.csv")

write.csv(data_morning_VL_3,"data_morning_VL_3.csv")

write.csv(data_morning_VL_4,"data_morning_VL_4.csv")


#################### NOON PERIOD############

data_motion_Noon <- hourly_data_all[which(hourly_data_all$Day_period=="Noon"),c(1:2, 20, 93, 94, 96, 97, 3:17)]

data_power_Noon <- hourly_data_all[which(hourly_data_all$Day_period=="Noon"),c(21:40, 56:73)]

#data_CO2_Morning <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Morning"),22]


data_light_Noon <- hourly_data_all[which(hourly_data_all$Day_period=="Noon"),c(41:55)]


data_Noon <- cbind(data_motion_Noon,data_light_Noon,data_power_Noon)


#data_Noon$day_index_num <- format(data_Noon$Time,"%Y-%m-%d")


#data_Noon$season <- Func.season(data_Noon$Month)


data_Noon_M <- data_Noon[which(data_Noon$day_index_num %in% b_4),]
data_Noon_L <- data_Noon[which(data_Noon$day_index_num %in% b_2),]
data_Noon_H <- data_Noon[which(data_Noon$day_index_num %in% b_1),]
data_Noon_VL <- data_Noon[which(data_Noon$day_index_num %in% b_3),]


############## Low activity################

library("arc")

Noon_L_bins_Power <- discretizeUnsupervised(data_Noon_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Frequency")

Noon_L_bins_Light <- discretizeUnsupervised(data_Noon_L$data_light_Noon, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Noon_L_bins_CO2 <- discretizeUnsupervised(data_Noon_L$data_CO2_Noon, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Noon_L_bins_motion <- discretizeUnsupervised(data_Noon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Noon_L$motion_label <- 0

data_Noon_L$motion_label[which(data_Noon_L$motion_total==0)] <- "no movement"

#data_Noon_L$motion_label[which(data_Noon_L$motion_total==1)] <- " one movement"

data_Noon_L$motion_label[which(data_Noon_L$motion_total >=1 & data_Noon_L$motion_total <=3)] <- "Less movement"

data_Noon_L$motion_label[which(data_Noon_L$motion_total >=4)] <- "frequent movement"



data_Noon_L$light_label <- Noon_L_bins_Light[["Disc.data"]]

data_Noon_L$power_label <- Noon_L_bins_Power[["Disc.data"]]


data_Noon_L$co2_label <- Noon_L_bins_CO2[["Disc.data"]]

#data_Noon_L$motion_label <- Noon_L_bins_motion[["Disc.data"]]


data_Noon <- data_Noon[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Noon_L, file = "data_Noon_Low.csv")

### Seperating days with respect to different clusters##########

ind_Noon_1 <- b_2[which(b_2 %in% a_1)]

ind_Noon_2 <- b_2[which(b_2 %in% a_2)]

ind_Noon_3 <- b_2[which(b_2 %in% a_3)]

ind_Noon_4 <- b_2[which(b_2 %in% a_4)]



data_Noon_L_1 <- data_Noon_L[which(data_Noon_L$day_index_num %in% ind_Noon_1),]

data_Noon_L_2 <- data_Noon_L[which(data_Noon_L$day_index_num %in% ind_Noon_2),]

data_Noon_L_3 <- data_Noon_L[which(data_Noon_L$day_index_num %in% ind_Noon_3),]

data_Noon_L_4 <- data_Noon_L[which(data_Noon_L$day_index_num %in% ind_Noon_4),]




write.csv(data_Noon_L_1,"data_Noon_L_1.csv")

write.csv(data_Noon_L_2,"data_Noon_L_2.csv")

write.csv(data_Noon_L_3,"data_Noon_L_3.csv")

write.csv(data_Noon_L_4,"data_Noon_L_4.csv")


############## Very Low activity################
install.packages("arc")

library("arc")

Noon_VL_bins_Power <- discretizeUnsupervised(data_Noon_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Noon_VL_bins_Light <- discretizeUnsupervised(data_Noon_VL$data_light_Noon, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 3, method = "Cluster")

Noon_VL_bins_CO2 <- discretizeUnsupervised(data_Noon_VL$data_CO2_Noon, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           categories = 4, method = "Cluster")

#Noon_L_bins_motion <- discretizeUnsupervised(data_Noon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Noon_VL$motion_label <- 0

data_Noon_VL$motion_label[which(data_Noon_VL$motion_total==0)] <- "no movement"

data_Noon_VL$motion_label[which(data_Noon_VL$motion_total==1)] <- " one movement"

#data_Noon_VL$motion_label[which(data_Noon_VL$motion_total >=1 & data_Noon_L$motion_total <=3)] <- "Less movement"

data_Noon_VL$motion_label[which(data_Noon_VL$motion_total >=2)] <- "frequent movement"



data_Noon_VL$light_label <- Noon_VL_bins_Light[["Disc.data"]]

data_Noon_VL$power_label <- Noon_VL_bins_Power[["Disc.data"]]


data_Noon_VL$co2_label <- Noon_VL_bins_CO2[["Disc.data"]]

#data_Noon_L$motion_label <- Noon_L_bins_motion[["Disc.data"]]


data_Noon <- data_Noon[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Noon_VL, file = "data_Noon_VLow.csv")

### Seperating days with respect to different clusters##########

ind_Noon_1 <- b_4[which(b_4 %in% a_1)]

ind_Noon_2 <- b_4[which(b_4 %in% a_2)]

ind_Noon_3 <- b_4[which(b_4 %in% a_3)]

ind_Noon_4 <- b_4[which(b_4 %in% a_4)]



data_Noon_VL_1 <- data_Noon_VL[which(data_Noon_VL$day_index_num %in% ind_Noon_1),]

data_Noon_VL_2 <- data_Noon_VL[which(data_Noon_VL$day_index_num %in% ind_Noon_2),]

data_Noon_VL_3 <- data_Noon_VL[which(data_Noon_VL$day_index_num %in% ind_Noon_3),]

data_Noon_VL_4 <- data_Noon_VL[which(data_Noon_VL$day_index_num %in% ind_Noon_4),]




write.csv(data_Noon_VL_1,"data_Noon_VL_1.csv")

write.csv(data_Noon_VL_2,"data_Noon_VL_2.csv")

write.csv(data_Noon_VL_3,"data_Noon_VL_3.csv")

write.csv(data_Noon_VL_4,"data_Noon_VL_4.csv")



#################### AFTERNOON PERIOD############

data_motion_Afternoon <- hourly_data_all[which(hourly_data_all$Day_period=="Afternoon"),c(1:2, 20, 93, 94, 96, 97, 3:17)]

data_power_Afternoon <- hourly_data_all[which(hourly_data_all$Day_period=="Afternoon"),c(21:40, 56:73)]

#data_CO2_Afternoon <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Afternoon"),22]


data_light_Afternoon <- hourly_data_all[which(hourly_data_all$Day_period=="Afternoon"),c(41:55)]


data_Afternoon <- cbind(data_motion_Afternoon,data_light_Afternoon,data_power_Afternoon)


#data_Afternoon$day_index_num <- format(data_Afternoon$Time,"%Y-%m-%d")


#data_Afternoon$season <- Func.season(data_Afternoon$Month)


data_Afternoon_VL <- data_Afternoon[which(data_Afternoon$day_index_num %in% b_3),]
data_Afternoon_L <- data_Afternoon[which(data_Afternoon$day_index_num %in% b_1),]
data_Afternoon_M <- data_Afternoon[which(data_Afternoon$day_index_num %in% b_2),]
data_Afternoon_H <- data_Afternoon[which(data_Afternoon$day_index_num %in% b_4),]




############## Low period################

library("arc")


Afnoon_L_bins_Power <- discretizeUnsupervised(data_Afternoon_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Frequency")

Afnoon_L_bins_Light <- discretizeUnsupervised(data_Afternoon_L$data_light_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 3, method = "Cluster")

Afnoon_L_bins_CO2 <- discretizeUnsupervised(data_Afternoon_L$data_CO2_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

#Afnoon_L_bins_motion <- discretizeUnsupervised(data_Afternoon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Afternoon_L$motion_label <- 0

data_Afternoon_L$motion_label[which(data_Afternoon_L$motion_total==0)] <- "no movement"

#data_Afternoon_M$motion_label[which(data_Afternoon_M$motion_total>=1)] <- " more than movement"

data_Afternoon_L$motion_label[which(data_Afternoon_L$motion_total >=1 & data_Afternoon_L$motion_total <=3)] <- "Less movement"

data_Afternoon_L$motion_label[which(data_Afternoon_L$motion_total >=4)] <- "frequent movement"



data_Afternoon_L$light_label <- Afnoon_L_bins_Light[["Disc.data"]]

data_Afternoon_L$power_label <- Afnoon_L_bins_Power[["Disc.data"]]


data_Afternoon_L$co2_label <- Afnoon_L_bins_CO2[["Disc.data"]]

#data_Afternoon_L$motion_label <- Afnoon_L_bins_motion[["Disc.data"]]


data_Afternoon <- data_Afternoon[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Afternoon_L, file = "data_Afternoon_Low.csv")

### Seperating days with respect to different clusters##########

###Low activity####

ind_Afnoon_1 <- b_2[which(b_2 %in% a_1)]

ind_Afnoon_2 <- b_2[which(b_2 %in% a_2)]

ind_Afnoon_3 <- b_2[which(b_2 %in% a_3)]

ind_Afnoon_4 <- b_2[which(b_2 %in% a_4)]



data_Afternoon_L_1 <- data_Afternoon_L[which(data_Afternoon_L$day_index_num %in% ind_Afnoon_1),]

data_Afternoon_L_2 <- data_Afternoon_L[which(data_Afternoon_L$day_index_num %in% ind_Afnoon_2),]

data_Afternoon_L_3 <- data_Afternoon_L[which(data_Afternoon_L$day_index_num %in% ind_Afnoon_3),]

data_Afternoon_L_4 <- data_Afternoon_L[which(data_Afternoon_L$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Afternoon_L_1,"data_Afternoon_L_1.csv")

write.csv(data_Afternoon_L_2,"data_Afternoon_L_2.csv")

write.csv(data_Afternoon_L_3,"data_Afternoon_L_3.csv")

write.csv(data_Afternoon_L_4,"data_Afternoon_L_4.csv")

############## Very Low period################

library("arc")


Afnoon_VL_bins_Power <- discretizeUnsupervised(data_Afternoon_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                               
                                               categories = 4, method = "Cluster")

Afnoon_VL_bins_Light <- discretizeUnsupervised(data_Afternoon_VL$data_light_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                               
                                               categories = 3, method = "Cluster")

Afnoon_VL_bins_CO2 <- discretizeUnsupervised(data_Afternoon_VL$data_CO2_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

#Afnoon_VL_bins_motion <- discretizeUnsupervised(data_Afternoon_VL$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Afternoon_VL$motion_Label <- 0

data_Afternoon_VL$motion_Label[which(data_Afternoon_VL$motion_total==0)] <- "no movement"

data_Afternoon_VL$motion_Label[which(data_Afternoon_VL$motion_total==1)] <- " one movement"

#data_Afternoon_VL$motion_Label[which(data_Afternoon_VL$motion_total >=1 & data_Afternoon_VL$motion_total <=3)] <- "Less movement"

data_Afternoon_VL$motion_Label[which(data_Afternoon_VL$motion_total >=2)] <- "frequent movement"



data_Afternoon_VL$light_Label <- Afnoon_VL_bins_Light[["Disc.data"]]

data_Afternoon_VL$power_Label <- Afnoon_VL_bins_Power[["Disc.data"]]


data_Afternoon_VL$co2_Label <- Afnoon_VL_bins_CO2[["Disc.data"]]

#data_Afternoon_VL$motion_VLabel <- Afnoon_VL_bins_motion[["Disc.data"]]


data_Afternoon <- data_Afternoon[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Afternoon_VL, file = "data_Afternoon_VLow.csv")

### Seperating days with respect to different clusters##########

###Very Low activity####

ind_Afnoon_1 <- b_1[which(b_1 %in% a_1)]

ind_Afnoon_2 <- b_1[which(b_1 %in% a_2)]

ind_Afnoon_3 <- b_1[which(b_1 %in% a_3)]

ind_Afnoon_4 <- b_1[which(b_1 %in% a_4)]



data_Afternoon_VL_1 <- data_Afternoon_VL[which(data_Afternoon_VL$day_index_num %in% ind_Afnoon_1),]

data_Afternoon_VL_2 <- data_Afternoon_VL[which(data_Afternoon_VL$day_index_num %in% ind_Afnoon_2),]

data_Afternoon_VL_3 <- data_Afternoon_VL[which(data_Afternoon_VL$day_index_num %in% ind_Afnoon_3),]

data_Afternoon_VL_4 <- data_Afternoon_VL[which(data_Afternoon_VL$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Afternoon_VL_1,"data_Afternoon_VL_1.csv")

write.csv(data_Afternoon_VL_2,"data_Afternoon_VL_2.csv")

write.csv(data_Afternoon_VL_3,"data_Afternoon_VL_3.csv")

write.csv(data_Afternoon_VL_4,"data_Afternoon_VL_4.csv")


############## Medium period################

library("arc")


Afnoon_M_bins_Power <- discretizeUnsupervised(data_Afternoon_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Frequency")

Afnoon_M_bins_Light <- discretizeUnsupervised(data_Afternoon_M$data_light_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Cluster")

Afnoon_M_bins_CO2 <- discretizeUnsupervised(data_Afternoon_M$data_CO2_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

#Afnoon_M_bins_motion <- discretizeUnsupervised(data_Afternoon_M$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Afternoon_M$motion_Label <- 0

data_Afternoon_M$motion_Label[which(data_Afternoon_M$motion_total==0)] <- "no movement"

#data_Afternoon_M$motion_Label[which(data_Afternoon_M$motion_total==1)] <- " one movement"

data_Afternoon_M$motion_Label[which(data_Afternoon_M$motion_total >=1 & data_Afternoon_M$motion_total <=2)] <- "Less movement"

data_Afternoon_M$motion_Label[which(data_Afternoon_M$motion_total >=2)] <- "frequent movement"



data_Afternoon_M$light_Label <- Afnoon_M_bins_Light[["Disc.data"]]

data_Afternoon_M$power_Label <- Afnoon_M_bins_Power[["Disc.data"]]


data_Afternoon_M$co2_Label <- Afnoon_M_bins_CO2[["Disc.data"]]

#data_Afternoon_M$motion_Mabel <- Afnoon_M_bins_motion[["Disc.data"]]


data_Afternoon <- data_Afternoon[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Afternoon_M, file = "data_Afternoon_Medium.csv")

### Seperating days with respect to different clusters##########

###Medium activity####

ind_Afnoon_1 <- b_3[which(b_3 %in% a_1)]

ind_Afnoon_2 <- b_3[which(b_3 %in% a_2)]

ind_Afnoon_3 <- b_3[which(b_3 %in% a_3)]

ind_Afnoon_4 <- b_3[which(b_3 %in% a_4)]



data_Afternoon_M_1 <- data_Afternoon_M[which(data_Afternoon_M$day_index_num %in% ind_Afnoon_1),]

data_Afternoon_M_2 <- data_Afternoon_M[which(data_Afternoon_M$day_index_num %in% ind_Afnoon_2),]

data_Afternoon_M_3 <- data_Afternoon_M[which(data_Afternoon_M$day_index_num %in% ind_Afnoon_3),]

data_Afternoon_M_4 <- data_Afternoon_M[which(data_Afternoon_M$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Afternoon_M_1,"data_Afternoon_M_1.csv")

write.csv(data_Afternoon_M_2,"data_Afternoon_M_2.csv")

write.csv(data_Afternoon_M_3,"data_Afternoon_M_3.csv")

write.csv(data_Afternoon_M_4,"data_Afternoon_M_4.csv")


############# High activity period################

library("arc")


Afnoon_H_bins_Power <- discretizeUnsupervised(data_Afternoon_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Frequency")

Afnoon_H_bins_Light <- discretizeUnsupervised(data_Afternoon_H$data_light_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Cluster")

Afnoon_H_bins_CO2 <- discretizeUnsupervised(data_Afternoon_H$data_CO2_Afternoon, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

#Afnoon_H_bins_motion <- discretizeUnsupervised(data_Afternoon_H$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Afternoon_H$motion_Label <- 0

data_Afternoon_H$motion_Label[which(data_Afternoon_H$motion_total==0)] <- "no movement"

#data_Afternoon_H$motion_Label[which(data_Afternoon_H$motion_total==1)] <- " one movement"

data_Afternoon_H$motion_Label[which(data_Afternoon_H$motion_total >=1 & data_Afternoon_H$motion_total <=3)] <- "Less movement"

data_Afternoon_H$motion_Label[which(data_Afternoon_H$motion_total >=4)] <- "frequent movement"



data_Afternoon_H$light_Label <- Afnoon_H_bins_Light[["Disc.data"]]

data_Afternoon_H$power_Label <- Afnoon_H_bins_Power[["Disc.data"]]


data_Afternoon_H$co2_Label <- Afnoon_H_bins_CO2[["Disc.data"]]

#data_Afternoon_H$motion_Habel <- Afnoon_H_bins_motion[["Disc.data"]]


data_Afternoon <- data_Afternoon[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Afternoon_H, file = "data_Afternoon_High.csv")

### Seperating days with respect to different clusters##########

###High activity####

ind_Afnoon_1 <- b_4[which(b_4 %in% a_1)]

ind_Afnoon_2 <- b_4[which(b_4 %in% a_2)]

ind_Afnoon_3 <- b_4[which(b_4 %in% a_3)]

ind_Afnoon_4 <- b_4[which(b_4 %in% a_4)]



data_Afternoon_H_1 <- data_Afternoon_H[which(data_Afternoon_H$day_index_num %in% ind_Afnoon_1),]

data_Afternoon_H_2 <- data_Afternoon_H[which(data_Afternoon_H$day_index_num %in% ind_Afnoon_2),]

data_Afternoon_H_3 <- data_Afternoon_H[which(data_Afternoon_H$day_index_num %in% ind_Afnoon_3),]

data_Afternoon_H_4 <- data_Afternoon_H[which(data_Afternoon_H$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Afternoon_H_1,"data_Afternoon_H_1.csv")

write.csv(data_Afternoon_H_2,"data_Afternoon_H_2.csv")

write.csv(data_Afternoon_H_3,"data_Afternoon_H_3.csv")

write.csv(data_Afternoon_H_4,"data_Afternoon_H_4.csv")


######### EVENING DATA#################

data_motion_Evening <- hourly_data_all[which(hourly_data_all$Day_period=="Evening"),c(1:2, 20, 93, 94, 96, 97, 3:17)]

data_power_Evening <- hourly_data_all[which(hourly_data_all$Day_period=="Evening"),c(21:40, 56:73)]

data_CO2_Evening <- hourly_data_all[which(hourly_data_all$Day_period=="Evening"),c(74:77)]


data_light_Evening <- hourly_data_all[which(hourly_data_all$Day_period=="Evening"),(c(41:55))]


data_evening <- cbind(data_motion_Evening,data_CO2_Evening, data_light_Evening,data_power_Evening)

#data_evening$day_index_num <- format(data_evening$Time,"%Y-%m-%d")


#data_evening$season <- Func.season(data_evening$Month)


data_evening_M <- data_evening[which(data_evening$day_index_num %in% b_4),]
data_evening_VL <- data_evening[which(data_evening$day_index_num %in% b_1),]
data_evening_H <- data_evening[which(data_evening$day_index_num %in% b_3),]
data_evening_L <- data_evening[which(data_evening$day_index_num %in% b_2),]

install.packages("arc")

library(arc)

############ For low activity############

Eve_LowActivity_bins_Power <- discretizeUnsupervised(data_evening_L$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                                     
                                                     categories = 4, method = "Frequency")

Eve_LowActivity_bins_Light <- discretizeUnsupervised(data_evening_L$light_total, labels = TRUE, infinite_bounds = FALSE,
                                                     
                                                     categories = 3, method = "Frequency")

#Eve_LowActivity_bins_CO2 <- discretizeUnsupervised(data_evening_L$data_CO2, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   #categories = 4, method = "Cluster")


data_evening_L$motion_label <- 0

data_evening_L$motion_label[which(data_evening_L$motion_total==0)] <- "no movement"

data_evening_L$motion_label[which(data_evening_L$motion_total >=1 & data_evening_L$motion_total <=29)] <- "Less movement"

data_evening_L$motion_label[which(data_evening_L$motion_total >=30)] <- "frequent movement"



data_evening_L$light_label <- Eve_LowActivity_bins_Light[["Disc.data"]]

data_evening_L$power_label <- Eve_LowActivity_bins_Power[["Disc.data"]]


#data_evening_L$co2_label <- Eve_LowActivity_bins_CO2[["Disc.data"]]

write.csv(data_evening_L, "data_evening_low.csv")

### Seperating days with respect to different clusters##########

###Low activity####

ind_eve_1 <- b_2[which(b_2 %in% a_1)]

ind_eve_2 <- b_2[which(b_2 %in% a_2)]

ind_eve_3 <- b_2[which(b_2 %in% a_3)]

ind_eve_4 <- b_2[which(b_2 %in% a_4)]




data_evening_L_1 <- data_evening_VL[which(data_evening_L$day_index_num %in% ind_eve_1),]

data_evening_L_2 <- data_evening_VL[which(data_evening_L$day_index_num %in% ind_eve_2),]

data_evening_L_3 <- data_evening_VL[which(data_evening_L$day_index_num %in% ind_eve_3),]

data_evening_L_4 <- data_evening_VL[which(data_evening_L$day_index_num %in% ind_eve_4),]


write.csv(data_evening_L_1,"data_evening_L_1.csv")

write.csv(data_evening_L_2,"data_evening_L_2.csv")

write.csv(data_evening_L_3,"data_evening_L_3.csv")

write.csv(data_evening_L_4,"data_evening_L_4.csv")


### APriori##########

install.packages("arulesViz")

library("arulesViz")

data_evening_ARM <- data_evening_L[c(27,29,30)]

rules <- apriori(data_evening_ARM, parameter = list(support=0.1, confidence=0.4))

data_evening_ARM$motion_label=as.factor(data_evening_ARM$motion_label)

data_evening_L <- data_evening_L[c(1,19:23,25,26,2:18,24,27:30)]


write.csv(data_evening_L,"data_evening_L.csv")



############ For Evening - Verylow activity############

Eve_VeryLowActivity_bins_Power <- discretizeUnsupervised(data_evening_VL$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                                         
                                                         categories = 4, method = "Frequency")

Eve_VeryLowActivity_bins_Light <- discretizeUnsupervised(data_evening_VL$light_total, labels = TRUE, infinite_bounds = FALSE,
                                                         
                                                         categories = 2, method = "Cluster")

#Eve_VeryLowActivity_bins_CO2 <- discretizeUnsupervised(data_evening_VL$data_CO2, labels = TRUE, infinite_bounds = FALSE,
                                                       
                                                       #categories = 4, method = "Cluster")


data_evening_VL$motion_label <- 0

data_evening_VL$motion_label[which(data_evening_VL$motion_total==0)] <- "no movement"

data_evening_VL$motion_label[which(data_evening_VL$motion_total >=1 & data_evening_VL$motion_total <=2)] <- "Less movement"

data_evening_VL$motion_label[which(data_evening_VL$motion_total >2)] <- "frequent movement"



data_evening_VL$light_label <- Eve_VeryLowActivity_bins_Light[["Disc.data"]]

data_evening_VL$power_label <- Eve_VeryLowActivity_bins_Power[["Disc.data"]]


#data_evening_VL$co2_label <- Eve_VeryLowActivity_bins_CO2[["Disc.data"]]


#data_evening_VL <- data_evening_VL[c(1,19:23,25,26,2:18,24,27:30)]


write.csv(data_evening_VL,"data_evening_verylow.csv")

##Very Low activity####

ind_eve_1 <- b_1[which(b_1 %in% a_1)]

ind_eve_2 <- b_1[which(b_1 %in% a_2)]

ind_eve_3 <- b_1[which(b_1 %in% a_3)]

ind_eve_4 <- b_1[which(b_1 %in% a_4)]




data_evening_VL_1 <- data_evening_VL[which(data_evening_VL$day_index_num %in% ind_eve_1),]

data_evening_VL_2 <- data_evening_VL[which(data_evening_VL$day_index_num %in% ind_eve_2),]

data_evening_VL_3 <- data_evening_VL[which(data_evening_VL$day_index_num %in% ind_eve_3),]

data_evening_VL_4 <- data_evening_VL[which(data_evening_VL$day_index_num %in% ind_eve_4),]


write.csv(data_evening_VL_1,"data_evening_VL_1.csv")

write.csv(data_evening_VL_2,"data_evening_VL_2.csv")

write.csv(data_evening_VL_3,"data_evening_VL_3.csv")

write.csv(data_evening_VL_4,"data_evening_VL_4.csv")


############ FOr Evening - medium activity################

Eve_MActivity_bins_Power <- discretizeUnsupervised(data_evening_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   categories = 4, method = "Frequency")

Eve_MActivity_bins_Light <- discretizeUnsupervised(data_evening_M$data_light, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   categories = 2, method = "Cluster")

Eve_MActivity_bins_CO2 <- discretizeUnsupervised(data_evening_M$data_CO2, labels = TRUE, infinite_bounds = FALSE,
                                                 
                                                 categories = 4, method = "Cluster")


data_evening_M$motion_label <- 0

data_evening_M$motion_label[which(data_evening_M$motion_total==0)] <- "no movement"

data_evening_M$motion_label[which(data_evening_M$motion_total >=1 & data_evening_M$motion_total <=4)] <- "Less movement"

data_evening_M$motion_label[which(data_evening_M$motion_total >=5)] <- "frequent movement"



data_evening_M$light_label <- Eve_MActivity_bins_Light[["Disc.data"]]

data_evening_M$power_label <- Eve_MActivity_bins_Power[["Disc.data"]]


data_evening_M$co2_label <- Eve_MActivity_bins_CO2[["Disc.data"]]


data_evening_M <- data_evening_VL[c(1,19:23,25,26,2:18,24,27:30)]


write.csv(data_evening_M,"data_evening_M.csv")


### Seperating days with respect to different clusters##########

###Very Medium activity####

ind_eve_1 <- b_1[which(b_1 %in% a_1)]

ind_eve_2 <- b_1[which(b_1 %in% a_2)]

ind_eve_3 <- b_1[which(b_1 %in% a_3)]

ind_eve_4 <- b_1[which(b_1 %in% a_4)]




data_evening_M_1 <- data_evening_M[which(data_evening_M$day_index_num %in% ind_eve_1),]

data_evening_M_2 <- data_evening_M[which(data_evening_M$day_index_num %in% ind_eve_2),]

data_evening_M_3 <- data_evening_M[which(data_evening_M$day_index_num %in% ind_eve_3),]

data_evening_M_4 <- data_evening_M[which(data_evening_M$day_index_num %in% ind_eve_4),]


write.csv(data_evening_M_1,"data_evening_M_1.csv")

write.csv(data_evening_M_2,"data_evening_M_2.csv")

write.csv(data_evening_M_3,"data_evening_M_3.csv")

write.csv(data_evening_M_4,"data_evening_M_4.csv")



############ For Evening - High activity############

Eve_HighActivity_bins_Power <- discretizeUnsupervised(data_evening_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                                      
                                                      categories = 4, method = "Frequency")

Eve_HighActivity_bins_Light <- discretizeUnsupervised(data_evening_H$data_light, labels = TRUE, infinite_bounds = FALSE,
                                                      
                                                      categories = 4, method = "Cluster")

Eve_HighActivity_bins_CO2 <- discretizeUnsupervised(data_evening_H$data_CO2, labels = TRUE, infinite_bounds = FALSE,
                                                    
                                                    categories = 4, method = "Cluster")


data_evening_H$motion_label <- 0

data_evening_H$motion_label[which(data_evening_H$motion_total==0)] <- "no movement"

data_evening_H$motion_label[which(data_evening_H$motion_total >=1 & data_evening_H$motion_total <=8)] <- "Less movement"

data_evening_H$motion_label[which(data_evening_H$motion_total >=9)] <- "frequent movement"



data_evening_H$light_label <- Eve_HighActivity_bins_Light[["Disc.data"]]

data_evening_H$power_label <- Eve_HighActivity_bins_Power[["Disc.data"]]


data_evening_H$co2_label <- Eve_HighActivity_bins_CO2[["Disc.data"]]


data_evening_H <- data_evening_H[c(1,19:23,25,26,2:18,24,27:30)]


write.csv(data_evening_H,"data_evening_High.csv")

### Seperating days with respect to different clusters##########

###High activity####

ind_eve_1 <- b_3[which(b_3 %in% a_1)]

ind_eve_2 <- b_3[which(b_3 %in% a_2)]

ind_eve_3 <- b_3[which(b_3 %in% a_3)]

ind_eve_4 <- b_3[which(b_3 %in% a_4)]


data_evening_H_1 <- data_evening_H[which(data_evening_H$day_index_num %in% ind_eve_1),]

data_evening_H_2 <- data_evening_H[which(data_evening_H$day_index_num %in% ind_eve_2),]

data_evening_H_3 <- data_evening_H[which(data_evening_H$day_index_num %in% ind_eve_3),]

data_evening_H_4 <- data_evening_H[which(data_evening_H$day_index_num %in% ind_eve_4),]


write.csv(data_evening_H_1,"data_evening_H_1.csv")

write.csv(data_evening_H_2,"data_evening_H_2.csv")

write.csv(data_evening_H_3,"data_evening_H_3.csv")

write.csv(data_evening_H_4,"data_evening_H_4.csv")



#################### NIGHT PERIOD############

data_motion_Night <- hourly_data_all[which(hourly_data_all$Day_period=="Night"),c(1:2, 20, 93, 94, 96, 97, 3:17)]

data_power_Night <- hourly_data_all[which(hourly_data_all$Day_period=="Night"),c(21:40, 56:73)]

data_CO2_Night <- hourly_data_all[which(hourly_data_all$Day_period=="Night"),c(74:77)]


data_light_Night <- hourly_data_all[which(hourly_data_all$Day_period=="Night"),c(41:55)]


data_Night <- cbind(data_motion_Night,data_CO2_Night,data_light_Night,data_power_Night)


#data_Night$day_index_num <- format(data_Night$Time,"%Y-%m-%d")


#data_Night$season <- Func.season(data_Night$Month)


data_Night_H <- data_Night[which(data_Night$day_index_num %in% b_1),]
data_Night_L <- data_Night[which(data_Night$day_index_num %in% b_3),]
data_Night_M <- data_Night[which(data_Night$day_index_num %in% b_2),]




############## Low period################

library("arc")


Night_L_bins_Power <- discretizeUnsupervised(data_Night_L$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Frequency")

Night_L_bins_Light <- discretizeUnsupervised(data_Night_L$light_total, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Frequency")

#Night_L_bins_CO2 <- discretizeUnsupervised(data_Night_L$CO2, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           #categories = 4, method = "Frequency")


data_Night_L$motion_label <- 0

data_Night_L$motion_label[which(data_Night_L$motion_total==0)] <- "no movement"

data_Night_L$motion_label[which(data_Night_L$motion_total==1)] <- " One movement"

data_Night_L$motion_label[which(data_Night_L$motion_total >=1 & data_Night_L$motion_total <=3)] <- "Less movement"

data_Night_L$motion_label[which(data_Night_L$motion_total >3)] <- "frequent movement"



data_Night_L$light_label <- Night_L_bins_Light[["Disc.data"]]

data_Night_L$power_label <- Night_L_bins_Power[["Disc.data"]]


#data_Night_L$co2_label <- Night_L_bins_CO2[["Disc.data"]]


#data_Night <- data_Night[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Night_L, file = "data_Night_Low.csv")


### Seperating days with respect to different clusters##########

###Low activity####

ind_Night_1 <- b_3[which(b_3 %in% a_1)]

ind_Night_2 <- b_3[which(b_3 %in% a_2)]

ind_Night_3 <- b_3[which(b_3 %in% a_3)]




data_Night_L_1 <- data_Night_L[which(data_Night_L$day_index_num %in% ind_Night_1),]

data_Night_L_2 <- data_Night_L[which(data_Night_L$day_index_num %in% ind_Night_2),]

data_Night_L_3 <- data_Night_L[which(data_Night_L$day_index_num %in% ind_Night_3),]


write.csv(data_Night_L_1,"data_Night_L_1.csv")

write.csv(data_Night_L_2,"data_Night_L_2.csv")

write.csv(data_Night_L_3,"data_Night_L_3.csv")


##############High period################

library("arc")


Night_H_bins_Power <- discretizeUnsupervised(data_Night_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Frequency")

Night_H_bins_Light <- discretizeUnsupervised(data_Night_H$data_light_Night, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_H_bins_CO2 <- discretizeUnsupervised(data_Night_H$data_CO2_Night, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           categories = 4, method = "Cluster")


data_Night_H$motion_Label <- 0

data_Night_H$motion_Label[which(data_Night_H$motion_total==0)] <- "no movement"

#data_Night_H$motion_Label[which(data_Night_H$motion_total==1)] <- " One movement"

data_Night_H$motion_Label[which(data_Night_H$motion_total >=1 & data_Night_H$motion_total <=3)] <- "Less movement"

data_Night_H$motion_Label[which(data_Night_H$motion_total >3)] <- "frequent movement"



data_Night_H$light_Label <- Night_H_bins_Light[["Disc.data"]]

data_Night_H$power_Label <- Night_H_bins_Power[["Disc.data"]]


data_Night_H$co2_Label <- Night_H_bins_CO2[["Disc.data"]]


data_Night <- data_Night[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Night_H, file = "data_Night_High.csv")


### Seperating days with respect to different clusters##########

###High activity####

ind_Night_1 <- b_1[which(b_1 %in% a_1)]

ind_Night_2 <- b_1[which(b_1 %in% a_2)]

ind_Night_3 <- b_1[which(b_1 %in% a_3)]




data_Night_H_1 <- data_Night_H[which(data_Night_H$day_index_num %in% ind_Night_1),]

data_Night_H_2 <- data_Night_H[which(data_Night_H$day_index_num %in% ind_Night_2),]

data_Night_H_3 <- data_Night_H[which(data_Night_H$day_index_num %in% ind_Night_3),]


write.csv(data_Night_H_1,"data_Night_H_1.csv")

write.csv(data_Night_H_2,"data_Night_H_2.csv")

write.csv(data_Night_H_3,"data_Night_H_3.csv")


##############MEdium period################

library("arc")


Night_M_bins_Power <- discretizeUnsupervised(data_Night_M$power_total_2, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Frequency")

Night_M_bins_Light <- discretizeUnsupervised(data_Night_M$light_total, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Frequency")

#Night_M_bins_CO2 <- discretizeUnsupervised(data_Night_M$data_CO2_Night, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           #categories = 4, method = "Cluster")


data_Night_M$motion_Label <- 0

data_Night_M$motion_Label[which(data_Night_M$motion_total==0)] <- "no movement"

#data_Night_M$motion_Label[which(data_Night_M$motion_total==1)] <- " One movement"

data_Night_M$motion_Label[which(data_Night_M$motion_total >=1 & data_Night_M$motion_total <=13)] <- "Less movement"

data_Night_M$motion_Label[which(data_Night_M$motion_total >=14)] <- "frequent movement"



data_Night_M$light_Label <- Night_M_bins_Light[["Disc.data"]]

data_Night_M$power_Label <- Night_M_bins_Power[["Disc.data"]]


#data_Night_M$co2_Label <- Night_M_bins_CO2[["Disc.data"]]


#data_Night <- data_Night[c(1,19:23,25,26,2:18,24,27:30)]



write.csv(data_Night_M, file = "data_Night_Medium.csv")


### Seperating days with respect to different clusters##########

###High activity####

ind_Night_1 <- b_2[which(b_2 %in% a_1)]

ind_Night_2 <- b_2[which(b_2 %in% a_2)]

ind_Night_3 <- b_2[which(b_2 %in% a_3)]




data_Night_M_1 <- data_Night_M[which(data_Night_M$day_index_num %in% ind_Night_1),]

data_Night_M_2 <- data_Night_M[which(data_Night_M$day_index_num %in% ind_Night_2),]

data_Night_M_3 <- data_Night_M[which(data_Night_M$day_index_num %in% ind_Night_3),]


write.csv(data_Night_M_1,"data_Night_M_1.csv")

write.csv(data_Night_M_2,"data_Night_M_2.csv")

write.csv(data_Night_M_3,"data_Night_M_3.csv")

write.csv(df_cluster_transform, "df_cluster_transfer.csv")



##Finding the frequency_energy

rel_freq_energy_P1 <- table(data_morning_L_2$Total.KPWR)

freq_energy_P1 <- table(data_morning_L_2$Total.KPWR)/length(data_morning_L_2$Total.KPWR)

barplot(freq_energy_P1, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow")
box()


matrix <- cbind(prop.table(table(data_morning_L_2$Total.KPWR)))
barplot(matrix)

