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
  
  Period1 <- c("7","8","9", "10", "11", "12")
  
  Period2 <- c("13", "14", "15","16","17")
  
  Period3 <- c("18", "19")
  
  Period4 <- c("20", "21", "22", "23")
  
  Period5 <- c("0", "1", "2", "3", "4", "5", "6")
  
  day_division <- c("Period1","Period2", "Period3", "Period4", "Period5")
  
  x <- vector()
  
  for(i in 1:length(time)){
    
    if(time[i] %in% Period1){
      
      x[i]= day_division[1]
      
    }else if(time[i] %in% Period2){
      
      x[i] = day_division[2]
      
    }else if(time[i] %in% Period3){
      
      x[i] = day_division[3]
      
    }else if(time[i] %in% Period4){
      
      x[i] = day_division[4]
      
    }else{
      
      x[i] = day_division[5]
      
    }
    
  }
  
  
  
  return(x)
  
}


Func.reshape <- function(df, num_period, num_feature){
  
  attr(df, "names")[1] <- "day_index_num"
  
  attr(df, "names")[2] <- "Day.period"
  
  
  
  names_vec <- attr(df, "names")
  
  period_vec <- df$Day.period[1:num_period]
  
  col_ind <- 5+num_feature-1
  
  feature_vec <- names_vec[5:col_ind]
  
  
  
  day_unique <- as.vector(unique(df$day_index_num))
  
  df_new <- data.frame()
  
  len_day <- length(day_unique)
  
  
  
  for(i in 1:len_day){
    
    
    
    ind_1 <- which(df$day_index_num==day_unique[i])
    
    df_sub1 <- df[ind_1[1],-2]
    
    df_sub2 <- df[ind_1[2],-c(1,2)]
    
    df_sub3 <- df[ind_1[3],-c(1,2)]
    
    df_sub4 <- df[ind_1[4],-c(1,2)]
    
    df_sub5 <- df[ind_1[5],-c(1,2)]
    
    
    
    df_created <- dplyr::bind_cols(df_sub1,df_sub2,df_sub3, df_sub4, df_sub5)
    
    
    
    df_new <- dplyr::bind_rows(df_new,df_created)
    
    
    
  }
  
  
  
  new_period_vec <- rep(period_vec,each=num_feature)
  
  new_feature_vec <- rep(feature_vec,num_period)
  
  
  
  new_names_vec <- paste(new_period_vec,new_feature_vec,sep="_")
  
  new_names_vec <- c(names_vec[1],new_names_vec)
  
  
  
  attr(df_new, "names") <- new_names_vec
  
  
  
  return(df_new)
  
  
  
}


#remove(Minami182_10min_index)

Minami182_10min_index <- read.csv("K:/Hikari/S/T4_O/182/Minami182_10min_index.csv")

Minami182_10min_index$Time <- as.POSIXct(strptime(Minami182_10min_index$Time, format = "%Y-%m-%d %H:%M"))

Minami182_10min_index <- Minami182_10min_index[,-9]

Minami182_10min_index$Day.period <- Func.Day_Energy(Minami182_10min_index$Hour)


Minami182_10min_index <- Minami182_10min_index[c(1:8, 151, 9:150)]

## Cluster_Energy data

library(dplyr)

Cluster_data <- Minami182_10min_index %>%
  
  group_by(day_index, Day.period) %>%
  
  summarise(mean=mean(Total.KPWR),
            min=min(Total.KPWR),
            max=max(Total.KPWR),
            sd=sd(Total.KPWR))


df_cluster=Func.reshape(Cluster_data, num_period = 5, num_feature = 4)
            
names(df_cluster) <- c("date", "Period 1_mean", "Period 1_min", "Peroid 1_max", "Period 1_sd", "Period 2_mean", "Period 2_min", "Period 2_max", "Period 2_sd", 
                       "Period 3_mean", "Period 3_min", "Period 3_max", "Period 3_sd", "Period 4_mean", "Period 4_min", "Period 4_max", "Period 4_sd", "Period 5_mean", "Period 5_min", "Period 5_max", "Period 5_sd")


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

df_cluster <- df_cluster[-332,]
df_cluster_transform <- Func.transform(df_cluster)

####### Motion_Cluster_data######

Func.motionseason <- function(month){
  
  Winter <- c("01", "02","12")
  
  Spring <- c("03", "04", "05")
  
  Summer <- c("06","07","08")
  
  Autumn <- c("09","10","11")
  
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

Func.Day_Motion <- function(time){
  
  Period1 <- c("07","08","09", "10", "11", "12")
  
  Period2 <- c("13", "14", "15","16","17")
  
  Period3 <- c("18", "19")
  
  Period4 <- c("20", "21", "22", "23")
  
  Period5 <- c("00","01", "02", "03", "04", "05", "06")
  
  day_division <- c("Period1","Period2", "Period3", "Period4", "Period5")
  
  
  x <- vector()
  
  for(i in 1:length(time)){
    
    if(time[i] %in% Period1){
      
      x[i]= day_division[1]
      
    }else if(time[i] %in% Period2){
      
      x[i] = day_division[2]
      
    }else if(time[i] %in% Period3){
      
      x[i] = day_division[3]
      
    }else if(time[i] %in% Period4){
      
      x[i] = day_division[4]
      
    }else{
      
      x[i] = day_division[5]
      
    }
    
  }
  
  
  
  return(x)
  
}


library(dplyr)


Minami_182_Motion <- read.csv("K:/Hikari/S/T4_O/182/MInami_182_Motion.csv")

Minami_182_Motion$Time <- as.POSIXct(strptime(Minami_182_Motion$Time, format = "%m/%d/%Y %H:%M"))

weekday1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')



Minami_182_Motion <- Minami_182_Motion %>%
  
  mutate(Hour= format(Minami_182_Motion$Time,"%H"),
         
         Day = as.numeric(format(Minami_182_Motion$Time,"%d")),
         
         DayOfWeek = weekdays(Minami_182_Motion$Time,abbreviate = FALSE),
         
         Weekday_weekend=factor((DayOfWeek %in% weekday1),
                                
                                levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))

Minami_182_Motion <- Minami_182_Motion %>%
  mutate(Month = format(Minami_182_Motion$Time, "%m"))

Minami_182_Motion <- Minami_182_Motion[c(1,16:20,2:15)]

Minami_182_Motion$Day.period <- Func.Day_Motion(Minami_182_Motion$Hour)

Minami_182_Motion$Season <- Func.motionseason(Minami_182_Motion$Month)


minami182_motion_tot <- Minami_182_Motion %>%
  
  mutate(motion_tot = KMVL1201+KMVL1202+KMVL1203+KMVL1204+KMVL1205+KMVL1206+KMVL1207+KMVL1208+KMVL1209+KMVL120A+KMVL120B+KMVL120C+KMVL120D+KMVL120E)


occupancy_event <- minami182_motion_tot[c(1,2,6,23)]

occupancy_event$day_index_num <- format(occupancy_event$Time,"%Y-%m-%d")

occupancy_event$Day_period <- Func.Day_Motion(occupancy_event$Hour)

occupancy_event <- occupancy_event %>%
  group_by(day_index_num,Day_period) %>%
  summarise(num_event_total = sum(motion_tot))

occupancy_event_rs <- Func.reshape(occupancy_event,num_period = 5, num_feature = 1)

occupancy_event_rs <- occupancy_event_rs[-332,]

occupancy_event_transform <- Func.transform(occupancy_event_rs)

minami182_motion_tot$day_index_num <- format(minami182_motion_tot$Time,"%Y-%m-%d")

##Movements in August###

occupancy_event_2 <- minami182_motion_tot %>%
  group_by(day_index_num) %>%
  summarise(num_event_total = sum(motion_tot))
occupancy_event_2 <- occupancy_event_2[-332,]
occupancy_event_2$day_index_num <- as.POSIXct(occupancy_event_2$day_index_num )

plot(x=occupancy_event_2$day_index_num[183:211],y=occupancy_event_2$num_event_total[183:211], type="l", xlab = "Date", ylab = "Total no.of movements", main ="Movements in August - Minami 182")


#  period 1

#  Occupancy
#Elbow Method

k.max <- 15
data <- occupancy_event_transform[2]
wss1 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss1
plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movement_Perio1")
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
data <- df_cluster_transform[2:5]
wss2 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss2
plot(1:k.max, wss2,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0, 50),
     main = "Power consumption_Period1")
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


#  period 2:(13, 14, 15, 16, 17)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[3]
wss3 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 50 )$tot.withinss})
wss3
plot(1:k.max, wss3,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movement_Noon period")
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

x_period <- rep("Period2",4)

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
data <- df_cluster_transform[6:9]
wss4 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss4
plot(1:k.max, wss4,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,50),
     main = "Power consumption_Noon period")
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


#  period 3:(18, 19)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[4]
wss5 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 30 )$tot.withinss})
wss5
plot(1:k.max, wss5,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,25),
     main = "Occupancy movement_Afternoon period")
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

x_period <- rep("Period3",4)

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
data <- df_cluster_transform[10:13]
wss6 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15)$tot.withinss})
wss6
plot(1:k.max, wss6,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0, 65),
     main = "Power consumption_Afternoon period")
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

#  period 4: (20, 21, 22, 23)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[5]
wss7 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss7
plot(1:k.max, wss7,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0, 10),
     main = "Occupancy movement_Evening period")

box()
set.seed(123)
occ_kmeans_results <- kmeans(occupancy_event_transform[5],centers = 4, nstart = 50)


occ_kmeans_center <- occ_kmeans_results[["centers"]]

occ_kmeans_center <- t(occ_kmeans_center)
occ_kmeans_center <- as.data.frame(occ_kmeans_center)
x_1 <- occ_kmeans_center$`1`
x_2 <- occ_kmeans_center$`2`
x_3 <- occ_kmeans_center$`3`
x_4 <- occ_kmeans_center$`4`

x_period <- rep("Period4",4)

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
data <- df_cluster_transform[14:17]
wss8 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss8
plot(1:k.max, wss8,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,50),
     main="Power consumption_Evening period")
box()

set.seed(123)
power_kmeans_results <- kmeans(df_cluster_transform[14:17],centers = 4, nstart = 50)

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


#  period 5: (00, 01, 02, 03, 04, 05, 06)

#  Occupancy
#Elbow Method
k.max <- 15
data <- occupancy_event_transform[6]
wss9 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss9
plot(1:k.max, wss9,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Occupancy movemenmt_Night period")
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


x_period <- rep("Period5",4)

occ_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3,x_4))
colnames(occ_kmeans_center_new) <- c("Period","Center")
occ_kmeans_center_new$cluster.ID <- c(1:4)


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
data <- df_cluster_transform[18:21]
wss10 <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss10
plot(1:k.max, wss10,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     ylim=c(0,60),
     main="Power consumption_Night period")
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


power_kmeans_center_new <- data.frame(x_period,c(x_1,x_2,x_3,x_4))
colnames(power_kmeans_center_new) <- c("Features","Center")
power_kmeans_center_new$cluster.ID <- rep(c(1:4),each=4)


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


length(which(b_4 %in% a_1))
length(which(b_4 %in% a_2))
length(which(b_4 %in% a_3))
length(which(b_4 %in% a_4))


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

minami_182_Light_processed <- Minami182_10min_index[c(3:10,98)]

minami_182_indoor_processed <- Minami182_10min_index[c(3:23)]

minami_182_power_processed <- Minami182_10min_index[c(3:10,131)]

minami_182_indoor_processed <- minami_182_indoor_processed %>%
  mutate(averageCO2 = (KTCO1144+KTCO1145+KTCO1146+KTCO1147)/4)


#################### PERIOD 1############

data_motion_Period1 <- Minami_182_Motion_10min[which(Minami_182_Motion_10min$Day.period=="Period1"),c(1:15,18)]

data_power_Period1 <- minami_182_power_processed[which(minami_182_power_processed$Day.period=="Period1"),c(4:9)]

data_CO2_Period1 <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Period1"),22]


data_light_Period1 <- minami_182_Light_processed[which(minami_182_Light_processed$Day.period=="Period1"),9]


data_Period1 <- cbind(data_motion_Period1,data_CO2_Period1,data_light_Period1,data_power_Period1)


data_Period1$day_index_num <- format(data_Period1$Time,"%Y-%m-%d")


data_Period1$season <- Func.season(data_Period1$Month)

data_Period1 <- data_Period1[, -23]


data_Period1_H <- data_Period1[which(data_Period1$day_index_num %in% b_1),]
data_Period1_M <- data_Period1[which(data_Period1$day_index_num %in% b_2),]
data_Period1_L <- data_Period1[which(data_Period1$day_index_num %in% b_3),]
data_Period1_VL <- data_Period1[which(data_Period1$day_index_num %in% b_4),]



############## High activity################

library("arc")


Per1_L_bins_Power <- discretizeUnsupervised(data_Period1_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

Per1_L_bins_Light <- discretizeUnsupervised(data_Period1_H$data_light_Period1, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 3, method = "Cluster")

Per1_L_bins_CO2 <- discretizeUnsupervised(data_Period1_H$data_CO2_Period1, labels = TRUE, infinite_bounds = FALSE,
                                        
                                        categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_H$motion_total, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          #categories = 4, method = "Cluster")

data_Period1_H$motion_label <- 0

data_Period1_H$motion_label[which(data_Period1_H$motion_total==0)] <- "no movement"

#data_morning_M$motion_label[which(data_morning_M$motion_total>=1)] <- " more than movement"

data_Period1_H$motion_label[which(data_Period1_H$motion_total >=1 & data_Period1_H$motion_total <=5)] <- "Less movement"

data_Period1_H$motion_label[which(data_Period1_H$motion_total >=5)] <- "frequent movement"



data_Period1_H$light_label <- Per1_L_bins_Light[["Disc.data"]]

data_Period1_H$power_label <- Per1_L_bins_Power[["Disc.data"]]


data_Period1_H$co2_label <- Per1_L_bins_CO2[["Disc.data"]]

#data_morning_H$motion_label <- Morn_L_bins_motion[["Disc.data"]]


data_Period1_H <- data_Period1_H[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period1_H, file = "data_Period1_High.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_1[which(b_1 %in% a_1)]

ind_morn_2 <- b_1[which(b_1 %in% a_2)]

ind_morn_3 <- b_1[which(b_1 %in% a_3)]

ind_morn_4 <- b_1[which(b_1 %in% a_4)]



data_Period1_H_1 <- data_Period1_H[which(data_Period1_H$day_index_num %in% ind_morn_1),]

data_Period1_H_2 <- data_Period1_H[which(data_Period1_H$day_index_num %in% ind_morn_2),]

data_Period1_H_3 <- data_Period1_H[which(data_Period1_H$day_index_num %in% ind_morn_3),]

data_Period1_H_4 <- data_Period1_H[which(data_Period1_H$day_index_num %in% ind_morn_4),]




write.csv(data_Period1_H_1,"data_Period1_H_1.csv")

write.csv(data_Period1_H_2,"data_Period1_H_2.csv")

write.csv(data_Period1_H_3,"data_Period1_H_3.csv")

write.csv(data_Period1_H_4,"data_Period1_H_4.csv")


############## Medium activity################

Morn_L_bins_Power <- discretizeUnsupervised(data_Period1_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Morn_L_bins_Light <- discretizeUnsupervised(data_Period1_M$data_light_Period1, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Morn_L_bins_CO2 <- discretizeUnsupervised(data_Period1_M$data_CO2_Period1, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period1_M$motion_label <- 0

data_Period1_M$motion_label[which(data_Period1_M$motion_total==0)] <- "no movement"

#data_morning_L$motion_label[which(data_morning_L$motion_total==1)] <- " one movement"

data_Period1_M$motion_label[which(data_Period1_M$motion_total >=1 & data_Period1_M$motion_total <=3)] <- "Less movement"

data_Period1_M$motion_label[which(data_Period1_M$motion_total >=4)] <- "frequent movement"



data_Period1_M$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_Period1_M$power_label <- Morn_L_bins_Power[["Disc.data"]]


data_Period1_M$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_L$motion_label <- Morn_L_bins_motion[["Disc.data"]]


data_Period1_M <- data_Period1_M[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period1_M, file = "data_Period1_Medium.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_2[which(b_2 %in% a_1)]

ind_morn_2 <- b_2[which(b_2 %in% a_2)]

ind_morn_3 <- b_2[which(b_2 %in% a_3)]

ind_morn_4 <- b_2[which(b_2 %in% a_4)]



data_Period1_M_1 <- data_Period1_M[which(data_Period1_M$day_index_num %in% ind_morn_1),]

data_Period1_M_2 <- data_Period1_M[which(data_Period1_M$day_index_num %in% ind_morn_2),]

data_Period1_M_3 <- data_Period1_M[which(data_Period1_M$day_index_num %in% ind_morn_3),]

data_Period1_M_4 <- data_Period1_M[which(data_Period1_M$day_index_num %in% ind_morn_4),]




write.csv(data_Period1_M_1,"data_Period1_M_1.csv")

write.csv(data_Period1_M_2,"data_Period1_M_2.csv")

write.csv(data_Period1_M_3,"data_Period1_M_3.csv")

write.csv(data_Period1_M_4,"data_Period1_M_4.csv")


############## Low activity################

library("arc")


Morn_L_bins_Power <- discretizeUnsupervised(data_Period1_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Morn_L_bins_Light <- discretizeUnsupervised(data_Period1_L$data_light_Period1, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Morn_L_bins_CO2 <- discretizeUnsupervised(data_Period1_L$data_CO2_Period1, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_M$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")


data_Period1_L$motion_label <- 0

data_Period1_L$motion_label[which(data_Period1_L$motion_total==0)] <- "no movement"

#data_morning_M$motion_label[which(data_morning_M$motion_total>=1)] <- " more than movement"

data_Period1_L$motion_label[which(data_Period1_L$motion_total >=1 & data_Period1_L$motion_total <=3)] <- "Less movement"

data_Period1_L$motion_label[which(data_Period1_L$motion_total >3)] <- "frequent movement"



data_Period1_L$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_Period1_L$power_label <- Morn_L_bins_Power[["Disc.data"]]


data_Period1_L$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_M$motion_label <- Morn_L_bins_motion[["Disc.data"]]


data_Period1_L <- data_Period1_L[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period1_L, file = "data_morning_Low.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_3[which(b_3 %in% a_1)]

ind_morn_2 <- b_3[which(b_3 %in% a_2)]

ind_morn_3 <- b_3[which(b_3 %in% a_3)]

ind_morn_4 <- b_3[which(b_3 %in% a_4)]



data_Period1_L_1 <- data_Period1_L[which(data_Period1_L$day_index_num %in% ind_morn_1),]

data_Period1_L_2 <- data_Period1_L[which(data_Period1_L$day_index_num %in% ind_morn_2),]

data_Period1_L_3 <- data_Period1_L[which(data_Period1_L$day_index_num %in% ind_morn_3),]

data_Period1_L_4 <- data_Period1_L[which(data_Period1_L$day_index_num %in% ind_morn_4),]




write.csv(data_Period1_L_1,"data_Period1_L_1.csv")

write.csv(data_Period1_L_2,"data_Period1_L_2.csv")

write.csv(data_Period1_L_3,"ddata_Period1_L_3.csv")

write.csv(data_Period1_L_4,"data_Period1_L_4.csv")


############## VeryLow activity################

library("arc")


Morn_L_bins_Power <- discretizeUnsupervised(data_Period1_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Morn_L_bins_Light <- discretizeUnsupervised(data_Period1_VL$data_light_Period1, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Morn_L_bins_CO2 <- discretizeUnsupervised(data_Period1_VL$data_CO2_Period1, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Morn_L_bins_motion <- discretizeUnsupervised(data_morning_VL$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

#data_morning_VL$motion_label <- 0

data_Period1_VL$motion_label[which(data_Period1_VL$motion_total==0)] <- "no movement"

data_Period1_VL$motion_label[which(data_Period1_VL$motion_total==1)] <- "one movement"

#data_morning_VL$motion_label[which(data_morning_VL$motion_total >=1 & data_morning_VL$motion_total <=4)] <- "Less movement"

data_Period1_VL$motion_label[which(data_Period1_VL$motion_total >1)] <- "frequent movement"



data_Period1_VL$light_label <- Morn_L_bins_Light[["Disc.data"]]

data_Period1_VL$power_label <- Morn_L_bins_Power[["Disc.data"]]


data_Period1_VL$co2_label <- Morn_L_bins_CO2[["Disc.data"]]

#data_morning_VL$motion_label <- Morn_L_bins_motion[["Disc.data"]]


data_Period1_VL <- data_Period1_VL[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period1_VL, file = "data_Period1_VLow.csv")

### Seperating days with respect to different clusters##########

ind_morn_1 <- b_4[which(b_4 %in% a_1)]

ind_morn_2 <- b_4[which(b_4 %in% a_2)]

ind_morn_3 <- b_4[which(b_4 %in% a_3)]

ind_morn_4 <- b_4[which(b_4 %in% a_4)]



data_Period1_VL_1 <- data_Period1_VL[which(data_Period1_VL$day_index_num %in% ind_morn_1),]

data_Period1_VL_2 <- data_Period1_VL[which(data_Period1_VL$day_index_num %in% ind_morn_2),]

data_Period1_VL_3 <- data_Period1_VL[which(data_Period1_VL$day_index_num %in% ind_morn_3),]

data_Period1_VL_4 <- data_Period1_VL[which(data_Period1_VL$day_index_num %in% ind_morn_4),]




write.csv(data_Period1_VL_1,"data_Period1_VL_1.csv")

write.csv(data_Period1_VL_2,"data_Period1_VL_2.csv")

write.csv(data_Period1_VL_3,"data_Period1_VL_3.csv")

write.csv(data_Period1_VL_4,"data_Period1_VL_4.csv")


#################### PERIOD2############

data_motion_Period2 <- Minami_182_Motion_10min[which(Minami_182_Motion_10min$Day.period=="Period2"),c(1:15,18)]

data_power_Period2 <- minami_182_power_processed[which(minami_182_power_processed$Day.period=="Period2"),c(4:9)]

data_CO2_Period2 <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Period2"),22]


data_light_Period2 <- minami_182_Light_processed[which(minami_182_Light_processed$Day.period=="Period2"),9]


data_Period2 <- cbind(data_motion_Period2,data_CO2_Period2,data_light_Period2,data_power_Period2)


data_Period2$day_index_num <- format(data_Period2$Time,"%Y-%m-%d")


data_Period2$season <- Func.season(data_Period2$Month)


data_Period2_L <- data_Period2[which(data_Period2$day_index_num %in% b_1),]
data_Period2_VL <- data_Period2[which(data_Period2$day_index_num %in% b_2),]
data_Period2_H <- data_Period2[which(data_Period2$day_index_num %in% b_3),]
data_Period2_M <- data_Period2[which(data_Period2$day_index_num %in% b_4),]


############## Low activity################

library("arc")

Noon_L_bins_Power <- discretizeUnsupervised(data_Period2_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Noon_L_bins_Light <- discretizeUnsupervised(data_Period2_L$data_light_Period2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Noon_L_bins_CO2 <- discretizeUnsupervised(data_Period2_L$data_CO2_Period2, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Noon_L_bins_motion <- discretizeUnsupervised(data_Noon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period2_L$motion_label <- 0

data_Period2_L$motion_label[which(data_Period2_L$motion_total==0)] <- "no movement"

#data_Noon_L$motion_label[which(data_Noon_L$motion_total==1)] <- " one movement"

data_Period2_L$motion_label[which(data_Period2_L$motion_total >=1 & data_Period2_L$motion_total <=3)] <- "Less movement"

data_Period2_L$motion_label[which(data_Period2_L$motion_total >=4)] <- "frequent movement"



data_Period2_L$light_label <- Noon_L_bins_Light[["Disc.data"]]

data_Period2_L$power_label <- Noon_L_bins_Power[["Disc.data"]]


data_Period2_L$co2_label <- Noon_L_bins_CO2[["Disc.data"]]

#data_Noon_L$motion_label <- Noon_L_bins_motion[["Disc.data"]]

data_Period2_L <- data_Period2_L[, -23]


data_Period2_L <- data_Period2_L[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period2_L, file = "data_Period2_L.csv")

### Seperating days with respect to different clusters##########

ind_Noon_1 <- b_1[which(b_1 %in% a_1)]

ind_Noon_2 <- b_1[which(b_1 %in% a_2)]

ind_Noon_3 <- b_1[which(b_1 %in% a_3)]

ind_Noon_4 <- b_1[which(b_1 %in% a_4)]



data_Period2_L_1 <- data_Period2_L[which(data_Period2_L$day_index_num %in% ind_Noon_1),]

data_Period2_L_2 <- data_Period2_L[which(data_Period2_L$day_index_num %in% ind_Noon_2),]

data_Period2_L_3 <- data_Period2_L[which(data_Period2_L$day_index_num %in% ind_Noon_3),]

data_Period2_L_4 <- data_Period2_L[which(data_Period2_L$day_index_num %in% ind_Noon_4),]




write.csv(data_Period2_L_1,"data_Period2_L_1.csv")

write.csv(data_Period2_L_2,"data_Period2_L_2.csv")

write.csv(data_Period2_L_3,"data_Period2_L_3.csv")

write.csv(data_Period2_L_4,"data_Period2_L_4.csv")


############## Very Low activity################
install.packages("arc")

library("arc")

Noon_VL_bins_Power <- discretizeUnsupervised(data_Period2_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Noon_VL_bins_Light <- discretizeUnsupervised(data_Period2_VL$data_light_Period2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Noon_VL_bins_CO2 <- discretizeUnsupervised(data_Period2_VL$data_CO2_Period2, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Noon_L_bins_motion <- discretizeUnsupervised(data_Noon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period2_VL$motion_label <- 0

data_Period2_VL$motion_label[which(data_Period2_VL$motion_total==0)] <- "no movement"

data_Period2_VL$motion_label[which(data_Period2_VL$motion_total==1)] <- " one movement"

#data_Noon_VL$motion_label[which(data_Noon_VL$motion_total >=1 & data_Noon_L$motion_total <=3)] <- "Less movement"

data_Period2_VL$motion_label[which(data_Period2_VL$motion_total >=2)] <- "frequent movement"



data_Period2_VL$light_label <- Noon_VL_bins_Light[["Disc.data"]]

data_Period2_VL$power_label <- Noon_VL_bins_Power[["Disc.data"]]


data_Period2_VL$co2_label <- Noon_VL_bins_CO2[["Disc.data"]]

#data_Noon_L$motion_label <- Noon_L_bins_motion[["Disc.data"]]

data_Period2_VL <- data_Period2_VL[, -23]


data_Period2_VL <- data_Period2_VL[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period2_VL, file = "data_Period2_VLow.csv")

### Seperating days with respect to different clusters##########

ind_Noon_1 <- b_2[which(b_2 %in% a_1)]

ind_Noon_2 <- b_2[which(b_2 %in% a_2)]

ind_Noon_3 <- b_2[which(b_2 %in% a_3)]

ind_Noon_4 <- b_2[which(b_2 %in% a_4)]



data_Period2_VL_1 <- data_Period2_VL[which(data_Period2_VL$day_index_num %in% ind_Noon_1),]

data_Period2_VL_2 <- data_Period2_VL[which(data_Period2_VL$day_index_num %in% ind_Noon_2),]

data_Period2_VL_3 <- data_Period2_VL[which(data_Period2_VL$day_index_num %in% ind_Noon_3),]

data_Period2_VL_4 <- data_Period2_VL[which(data_Period2_VL$day_index_num %in% ind_Noon_4),]




write.csv(data_Period2_VL_1,"data_Period2_VL_1.csv")

write.csv(data_Period2_VL_2,"data_Period2_VL_2.csv")

write.csv(data_Period2_VL_3,"data_Period2_VL_3.csv")

write.csv(data_Period2_VL_4,"data_Period2_VL_4.csv")


############## High activity################

library("arc")

Noon_L_bins_Power <- discretizeUnsupervised(data_Period2_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Noon_L_bins_Light <- discretizeUnsupervised(data_Period2_H$data_light_Period2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Noon_L_bins_CO2 <- discretizeUnsupervised(data_Period2_H$data_CO2_Period2, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Noon_L_bins_motion <- discretizeUnsupervised(data_Noon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period2_H$motion_label <- 0

data_Period2_H$motion_label[which(data_Period2_H$motion_total==0)] <- "no movement"

#data_Noon_L$motion_label[which(data_Noon_L$motion_total==1)] <- " one movement"

data_Period2_H$motion_label[which(data_Period2_H$motion_total >=1 & data_Period2_H$motion_total <=5)] <- "Less movement"

data_Period2_H$motion_label[which(data_Period2_H$motion_total >=6)] <- "frequent movement"



data_Period2_H$light_label <- Noon_L_bins_Light[["Disc.data"]]

data_Period2_H$power_label <- Noon_L_bins_Power[["Disc.data"]]


data_Period2_H$co2_label <- Noon_L_bins_CO2[["Disc.data"]]

#data_Noon_L$motion_label <- Noon_L_bins_motion[["Disc.data"]]

data_Period2_H <- data_Period2_H[, -23]


data_Period2_H <- data_Period2_H[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period2_H, file = "data_Period2_H.csv")

### Seperating days with respect to different clusters##########

ind_Noon_1 <- b_3[which(b_3 %in% a_1)]

ind_Noon_2 <- b_3[which(b_3 %in% a_2)]

ind_Noon_3 <- b_3[which(b_3 %in% a_3)]

ind_Noon_4 <- b_3[which(b_3 %in% a_4)]



data_Period2_H_1 <- data_Period2_H[which(data_Period2_H$day_index_num %in% ind_Noon_1),]

data_Period2_H_2 <- data_Period2_H[which(data_Period2_H$day_index_num %in% ind_Noon_2),]

data_Period2_H_3 <- data_Period2_H[which(data_Period2_H$day_index_num %in% ind_Noon_3),]

data_Period2_H_4 <- data_Period2_H[which(data_Period2_H$day_index_num %in% ind_Noon_4),]




write.csv(data_Period2_H_1,"data_Period2_H_1.csv")

write.csv(data_Period2_H_2,"data_Period2_H_2.csv")

write.csv(data_Period2_H_3,"data_Period2_H_3.csv")

write.csv(data_Period2_H_4,"data_Period2_H_4.csv")


############## Medium activity################

library("arc")

Noon_L_bins_Power <- discretizeUnsupervised(data_Period2_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Noon_L_bins_Light <- discretizeUnsupervised(data_Period2_M$data_light_Period2, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 3, method = "Cluster")

Noon_L_bins_CO2 <- discretizeUnsupervised(data_Period2_M$data_CO2_Period2, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")

#Noon_L_bins_motion <- discretizeUnsupervised(data_Noon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period2_M$motion_label <- 0

data_Period2_M$motion_label[which(data_Period2_M$motion_total==0)] <- "no movement"

#data_Noon_L$motion_label[which(data_Noon_L$motion_total==1)] <- " one movement"

data_Period2_M$motion_label[which(data_Period2_M$motion_total >=1 & data_Period2_M$motion_total <=4)] <- "Less movement"

data_Period2_M$motion_label[which(data_Period2_M$motion_total >=5)] <- "frequent movement"



data_Period2_M$light_label <- Noon_L_bins_Light[["Disc.data"]]

data_Period2_M$power_label <- Noon_L_bins_Power[["Disc.data"]]


data_Period2_M$co2_label <- Noon_L_bins_CO2[["Disc.data"]]

#data_Noon_L$motion_label <- Noon_L_bins_motion[["Disc.data"]]

data_Period2_M <- data_Period2_M[, -23]


data_Period2_M <- data_Period2_M[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period2_M, file = "data_Period2_M.csv")

### Seperating days with respect to different clusters##########

ind_Noon_1 <- b_4[which(b_4 %in% a_1)]

ind_Noon_2 <- b_4[which(b_4 %in% a_2)]

ind_Noon_3 <- b_4[which(b_4 %in% a_3)]

ind_Noon_4 <- b_4[which(b_4 %in% a_4)]



data_Period2_M_1 <- data_Period2_M[which(data_Period2_M$day_index_num %in% ind_Noon_1),]

data_Period2_M_2 <- data_Period2_M[which(data_Period2_M$day_index_num %in% ind_Noon_2),]

data_Period2_M_3 <- data_Period2_M[which(data_Period2_M$day_index_num %in% ind_Noon_3),]

data_Period2_M_4 <- data_Period2_M[which(data_Period2_M$day_index_num %in% ind_Noon_4),]




write.csv(data_Period2_M_1,"data_Period2_M_1.csv")

write.csv(data_Period2_M_2,"data_Period2_M_2.csv")

write.csv(data_Period2_M_3,"data_Period2_M_3.csv")

write.csv(data_Period2_M_4,"data_Period2_M_4.csv")



#################### PERIOD3############

data_motion_Period3 <- Minami_182_Motion_10min[which(Minami_182_Motion_10min$Day.period=="Period3"),c(1:15,18)]

data_power_Period3 <- minami_182_power_processed[which(minami_182_power_processed$Day.period=="Period3"),c(4:9)]

data_CO2_Period3 <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Period3"),22]

data_light_Period3 <- minami_182_Light_processed[which(minami_182_Light_processed$Day.period=="Period3"),9]


data_Period3 <- cbind(data_motion_Period3,data_CO2_Period3,data_light_Period3,data_power_Period3)


data_Period3$day_index_num <- format(data_Period3$Time,"%Y-%m-%d")


data_Period3$season <- Func.season(data_Period3$Month)


data_Period3_H <- data_Period3[which(data_Period3$day_index_num %in% b_1),]
data_Period3_L <- data_Period3[which(data_Period3$day_index_num %in% b_2),]
data_Period3_VL <- data_Period3[which(data_Period3$day_index_num %in% b_3),]
data_Period3_M <- data_Period3[which(data_Period3$day_index_num %in% b_4),]




############## Low period################

library("arc")


Afnoon_L_bins_Power <- discretizeUnsupervised(data_Period3_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Frequency")

Afnoon_L_bins_Light <- discretizeUnsupervised(data_Period3_L$data_light_Period3, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 3, method = "Cluster")

Afnoon_L_bins_CO2 <- discretizeUnsupervised(data_Period3_L$data_CO2_Period3, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

#Afnoon_L_bins_motion <- discretizeUnsupervised(data_Afternoon_L$motion_total, labels = TRUE, infinite_bounds = FALSE,
                                               
                                               #categories = 4, method = "Cluster")

data_Period3_L$motion_label <- 0

data_Period3_L$motion_label[which(data_Period3_L$motion_total==0)] <- "no movement"

#data_Afternoon_M$motion_label[which(data_Afternoon_M$motion_total>=1)] <- " more than movement"

data_Period3_L$motion_label[which(data_Period3_L$motion_total >=1 & data_Period3_L$motion_total <=4)] <- "Less movement"

data_Period3_L$motion_label[which(data_Period3_L$motion_total >=5)] <- "frequent movement"



data_Period3_L$light_label <- Afnoon_L_bins_Light[["Disc.data"]]

data_Period3_L$power_label <- Afnoon_L_bins_Power[["Disc.data"]]


data_Period3_L$co2_label <- Afnoon_L_bins_CO2[["Disc.data"]]

#data_Afternoon_L$motion_label <- Afnoon_L_bins_motion[["Disc.data"]]

data_Period3_L <- data_Period3_L[, -23]
data_Period3_L <- data_Period3_L[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period3_L, file = "data_Period3_Low.csv")

### Seperating days with respect to different clusters##########

###Low activity####

ind_Afnoon_1 <- b_2[which(b_2 %in% a_1)]

ind_Afnoon_2 <- b_2[which(b_2 %in% a_2)]

ind_Afnoon_3 <- b_2[which(b_2 %in% a_3)]

ind_Afnoon_4 <- b_2[which(b_2 %in% a_4)]



data_Period3_L_1 <- data_Period3_L[which(data_Period3_L$day_index_num %in% ind_Afnoon_1),]

data_Period3_L_2 <- data_Period3_L[which(data_Period3_L$day_index_num %in% ind_Afnoon_2),]

data_Period3_L_3 <- data_Period3_L[which(data_Period3_L$day_index_num %in% ind_Afnoon_3),]

data_Period3_L_4 <- data_Period3_L[which(data_Period3_L$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Period3_L_1,"data_Period3_L_1.csv")

write.csv(data_Period3_L_2,"data_Period3_L_2.csv")

write.csv(data_Period3_L_3,"data_Period3_L_3.csv")

write.csv(data_Period3_L_4,"data_Period3_L_4.csv")

############## Very Low period################

library("arc")


Afnoon_VL_bins_Power <- discretizeUnsupervised(data_Period3_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                               
                                               categories = 4, method = "Cluster")

Afnoon_VL_bins_Light <- discretizeUnsupervised(data_Period3_VL$data_light_Period3, labels = TRUE, infinite_bounds = FALSE,
                                                
                                                categories = 3, method = "Cluster")

Afnoon_VL_bins_CO2 <- discretizeUnsupervised(data_Period3_VL$data_CO2_Period3, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

#Afnoon_VL_bins_motion <- discretizeUnsupervised(data_Afternoon_VL$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period3_VL$motion_Label <- 0

data_Period3_VL$motion_Label[which(data_Period3_VL$motion_total==0)] <- "no movement"

data_Period3_VL$motion_Label[which(data_Period3_VL$motion_total==1)] <- " one movement"

#data_Afternoon_VL$motion_Label[which(data_Afternoon_VL$motion_total >=1 & data_Afternoon_VL$motion_total <=3)] <- "Less movement"

data_Period3_VL$motion_Label[which(data_Period3_VL$motion_total >1)] <- "frequent movement"



data_Period3_VL$light_Label <- Afnoon_VL_bins_Light[["Disc.data"]]

data_Period3_VL$power_Label <- Afnoon_VL_bins_Power[["Disc.data"]]


data_Period3_VL$co2_Label <- Afnoon_VL_bins_CO2[["Disc.data"]]

#data_Afternoon_VL$motion_VLabel <- Afnoon_VL_bins_motion[["Disc.data"]]

data_Period3_VL <- data_Period3_VL[, -23]
data_Period3_VL <- data_Period3_VL[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period3_VL, file = "data_Period3_VLow.csv")

### Seperating days with respect to different clusters##########

###Very Low activity####

ind_Afnoon_1 <- b_3[which(b_3 %in% a_1)]

ind_Afnoon_2 <- b_3[which(b_3 %in% a_2)]

ind_Afnoon_3 <- b_3[which(b_3 %in% a_3)]

ind_Afnoon_4 <- b_3[which(b_3 %in% a_4)]



data_Period3_VL_1 <- data_Period3_VL[which(data_Period3_VL$day_index_num %in% ind_Afnoon_1),]

data_Period3_VL_2 <- data_Period3_VL[which(data_Period3_VL$day_index_num %in% ind_Afnoon_2),]

data_Period3_VL_3 <- data_Period3_VL[which(data_Period3_VL$day_index_num %in% ind_Afnoon_3),]

data_Period3_VL_4 <- data_Period3_VL[which(data_Period3_VL$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Period3_VL_1,"data_Period3_VL_1.csv")

write.csv(data_Period3_VL_2,"data_Period3_VL_2.csv")

write.csv(data_Period3_VL_3,"data_Period3_VL_3.csv")

write.csv(data_Period3_VL_4,"data_Period3_VL_4.csv")


############## Medium period################

library("arc")


Afnoon_M_bins_Power <- discretizeUnsupervised(data_Period3_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Cluster")

Afnoon_M_bins_Light <- discretizeUnsupervised(data_Period3_M$data_light_Period3, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Cluster")

Afnoon_M_bins_CO2 <- discretizeUnsupervised(data_Period3_M$data_CO2_Period3, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

#Afnoon_M_bins_motion <- discretizeUnsupervised(data_Afternoon_M$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period3_M$motion_Label <- 0

data_Period3_M$motion_Label[which(data_Period3_M$motion_total==0)] <- "no movement"

#data_Afternoon_M$motion_Label[which(data_Afternoon_M$motion_total==1)] <- " one movement"

data_Period3_M$motion_Label[which(data_Period3_M$motion_total >=1 & data_Period3_M$motion_total <=6)] <- "Less movement"

data_Period3_M$motion_Label[which(data_Period3_M$motion_total >=7)] <- "frequent movement"



data_Period3_M$light_Label <- Afnoon_M_bins_Light[["Disc.data"]]

data_Period3_M$power_Label <- Afnoon_M_bins_Power[["Disc.data"]]


data_Period3_M$co2_Label <- Afnoon_M_bins_CO2[["Disc.data"]]

#data_Afternoon_M$motion_Mabel <- Afnoon_M_bins_motion[["Disc.data"]]

data_Period3_M <- data_Period3_M[, -23]
data_Period3_M <- data_Period3_M[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period3_M, file = "data_Period3_Medium.csv")

### Seperating days with respect to different clusters##########

###Medium activity####

ind_Afnoon_1 <- b_4[which(b_4 %in% a_1)]

ind_Afnoon_2 <- b_4[which(b_4 %in% a_2)]

ind_Afnoon_3 <- b_4[which(b_4 %in% a_3)]

ind_Afnoon_4 <- b_4[which(b_4 %in% a_4)]



data_Period3_M_1 <- data_Period3_M[which(data_Period3_M$day_index_num %in% ind_Afnoon_1),]

data_Period3_M_2 <- data_Period3_M[which(data_Period3_M$day_index_num %in% ind_Afnoon_2),]

data_Period3_M_3 <- data_Period3_M[which(data_Period3_M$day_index_num %in% ind_Afnoon_3),]

data_Period3_M_4 <- data_Period3_M[which(data_Period3_M$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Period3_M_1,"data_Period3_M_1.csv")

write.csv(data_Period3_M_2,"data_Period3_M_2.csv")

write.csv(data_Period3_M_3,"data_Period3_M_3.csv")

write.csv(data_Period3_M_4,"data_Period3_M_4.csv")


############# High activity period################

library("arc")


Afnoon_H_bins_Power <- discretizeUnsupervised(data_Period3_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Cluster")

Afnoon_H_bins_Light <- discretizeUnsupervised(data_Period3_H$data_light_Period3, labels = TRUE, infinite_bounds = FALSE,
                                              
                                              categories = 4, method = "Cluster")

Afnoon_H_bins_CO2 <- discretizeUnsupervised(data_Period3_H$data_CO2_Period3, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

#Afnoon_H_bins_motion <- discretizeUnsupervised(data_Afternoon_H$motion_total, labels = TRUE, infinite_bounds = FALSE,

#categories = 4, method = "Cluster")

data_Period3_H$motion_Label <- 0

data_Period3_H$motion_Label[which(data_Period3_H$motion_total==0)] <- "no movement"

#data_Afternoon_H$motion_Label[which(data_Afternoon_H$motion_total==1)] <- " one movement"

data_Period3_H$motion_Label[which(data_Period3_H$motion_total >=1 & data_Period3_H$motion_total <=7)] <- "Less movement"

data_Period3_H$motion_Label[which(data_Period3_H$motion_total >=8)] <- "frequent movement"



data_Period3_H$light_Label <- Afnoon_H_bins_Light[["Disc.data"]]

data_Period3_H$power_Label <- Afnoon_H_bins_Power[["Disc.data"]]


data_Period3_H$co2_Label <- Afnoon_H_bins_CO2[["Disc.data"]]

#data_Afternoon_H$motion_Habel <- Afnoon_H_bins_motion[["Disc.data"]]


data_Period3_H <- data_Period3_H[, -23]
data_Period3_H <- data_Period3_H[c(1,19:22,24,25,2:18,23,26:29)]


write.csv(data_Period3_H, file = "data_Period3_High.csv")

### Seperating days with respect to different clusters##########

###High activity####

ind_Afnoon_1 <- b_1[which(b_1 %in% a_1)]

ind_Afnoon_2 <- b_1[which(b_1 %in% a_2)]

ind_Afnoon_3 <- b_1[which(b_1 %in% a_3)]

ind_Afnoon_4 <- b_1[which(b_1 %in% a_4)]



data_Period3_H_1 <- data_Period3_H[which(data_Period3_H$day_index_num %in% ind_Afnoon_1),]

data_Period3_H_2 <- data_Period3_H[which(data_Period3_H$day_index_num %in% ind_Afnoon_2),]

data_Period3_H_3 <- data_Period3_H[which(data_Period3_H$day_index_num %in% ind_Afnoon_3),]

data_Period3_H_4 <- data_Period3_H[which(data_Period3_H$day_index_num %in% ind_Afnoon_4),]




write.csv(data_Period3_H_1,"data_Period3_H_1.csv")

write.csv(data_Period3_H_2,"data_Period3_H_2.csv")

write.csv(data_Period3_H_3,"data_Period3_H_3.csv")

write.csv(data_Period3_H_4,"data_Period3_H_4.csv")


######### PERIOD4#################

data_motion_Period4 <- Minami_182_Motion_10min[which(Minami_182_Motion_10min$Day.period=="Period4"),c(1:15,18)]

data_power_Period4 <- minami_182_power_processed[which(minami_182_power_processed$Day.period=="Period4"),c(4:9)]

data_CO2_Period4 <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Period4"),22]


data_light_Period4 <- minami_182_Light_processed[which(minami_182_Light_processed$Day.period=="Period4"),9]


data_Period4 <- cbind(data_motion_Period4,data_CO2_Period4,data_light_Period4,data_power_Period4)

data_Period4$day_index_num <- format(data_Period4$Time,"%Y-%m-%d")


data_Period4$season <- Func.season(data_Period4$Month)


data_Period4_H <- data_Period4[which(data_Period4$day_index_num %in% b_2),]
data_Period4_L <- data_Period4[which(data_Period4$day_index_num %in% b_3),]
data_Period4_M <- data_Period4[which(data_Period4$day_index_num %in% b_1),]
data_Period4_VL <- data_Period4[which(data_Period4$day_index_num %in% b_4),]

library(arc)

############ For low activity############

Eve_LowActivity_bins_Power <- discretizeUnsupervised(data_Period4_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                                     
                                                     categories = 4, method = "Cluster")

Eve_LowActivity_bins_Light <- discretizeUnsupervised(data_Period4_L$data_light_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                     
                                                     categories = 3, method = "Cluster")

Eve_LowActivity_bins_CO2 <- discretizeUnsupervised(data_Period4_L$data_CO2_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   categories = 4, method = "Cluster")


data_Period4_L$motion_label <- 0

data_Period4_L$motion_label[which(data_Period4_L$motion_total==0)] <- "no movement"

data_Period4_L$motion_label[which(data_Period4_L$motion_total >=1 & data_Period4_L$motion_total <=2)] <- "Less movement"

data_Period4_L$motion_label[which(data_Period4_L$motion_total >2)] <- "frequent movement"



data_Period4_L$light_label <- Eve_LowActivity_bins_Light[["Disc.data"]]

data_Period4_L$power_label <- Eve_LowActivity_bins_Power[["Disc.data"]]


data_Period4_L$co2_label <- Eve_LowActivity_bins_CO2[["Disc.data"]]


data_Period4_L <- data_Period4_L[, -23]
data_Period4_L <- data_Period4_L[c(1,19:22,24,25,2:18,23,26:29)]


write.csv(data_Period4_L, "data_Period4_low.csv")

### Seperating days with respect to different clusters##########

###Very Low activity####

ind_eve_1 <- b_3[which(b_3 %in% a_1)]

ind_eve_2 <- b_3[which(b_3 %in% a_2)]

ind_eve_3 <- b_3[which(b_3 %in% a_3)]

ind_eve_4 <- b_3[which(b_3 %in% a_4)]




data_Period4_L_1 <- data_Period4_L[which(data_Period4_L$day_index_num %in% ind_eve_1),]

data_Period4_L_2 <- data_Period4_L[which(data_Period4_L$day_index_num %in% ind_eve_2),]

data_Period4_L_3 <- data_Period4_L[which(data_Period4_L$day_index_num %in% ind_eve_3),]

data_Period4_L_4 <- data_Period4_L[which(data_Period4_L$day_index_num %in% ind_eve_4),]


write.csv(data_Period4_L_1,"data_Period4_L_1.csv")

write.csv(data_Period4_L_2,"data_Period4_L_2.csv")

write.csv(data_Period4_L_3,"data_Period4_L_3.csv")

write.csv(data_Period4_L_4,"data_Period4_L_4.csv")


### APriori##########

install.packages("arulesViz")

library("arulesViz")

data_evening_ARM <- data_evening_L[c(27,29,30)]

rules <- apriori(data_evening_ARM, parameter = list(support=0.1, confidence=0.4))

data_evening_ARM$motion_label=as.factor(data_evening_ARM$motion_label)

data_evening_L <- data_evening_L[c(1,19:23,25,26,2:18,24,27:30)]


write.csv(data_evening_L,"data_evening_L.csv")



############ For Very low activity############

Eve_LowActivity_bins_Power <- discretizeUnsupervised(data_Period4_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                                     
                                                     categories = 4, method = "Cluster")

Eve_LowActivity_bins_Light <- discretizeUnsupervised(data_Period4_VL$data_light_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                     
                                                     categories = 3, method = "Cluster")

Eve_LowActivity_bins_CO2 <- discretizeUnsupervised(data_Period4_VL$data_CO2_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   categories = 4, method = "Cluster")


data_Period4_VL$motion_label <- 0

data_Period4_VL$motion_Label[which(data_Period4_VL$motion_total==0)] <- "no movement"

data_Period4_VL$motion_Label[which(data_Period4_VL$motion_total==1)] <- " one movement"

data_Period4_VL$motion_Label[which(data_Period4_VL$motion_total >1)] <- "frequent movement"



data_Period4_VL$light_label <- Eve_LowActivity_bins_Light[["Disc.data"]]

data_Period4_VL$power_label <- Eve_LowActivity_bins_Power[["Disc.data"]]


data_Period4_VL$co2_label <- Eve_LowActivity_bins_CO2[["Disc.data"]]


data_Period4_VL <- data_Period4_VL[, -26]
data_Period4_VL <- data_Period4_VL[c(1,19:22,24,25,2:18,23,26:29)]


write.csv(data_Period4_VL, "data_Period4_Verylow.csv")

### Seperating days with respect to different clusters##########

###Very Low activity####

ind_eve_1 <- b_4[which(b_4 %in% a_1)]

ind_eve_2 <- b_4[which(b_4 %in% a_2)]

ind_eve_3 <- b_4[which(b_4 %in% a_3)]

ind_eve_4 <- b_4[which(b_4 %in% a_4)]




data_Period4_VL_1 <- data_Period4_VL[which(data_Period4_VL$day_index_num %in% ind_eve_1),]

data_Period4_VL_2 <- data_Period4_VL[which(data_Period4_VL$day_index_num %in% ind_eve_2),]

data_Period4_VL_3 <- data_Period4_VL[which(data_Period4_VL$day_index_num %in% ind_eve_3),]

data_Period4_VL_4 <- data_Period4_VL[which(data_Period4_VL$day_index_num %in% ind_eve_4),]


write.csv(data_Period4_VL_1,"data_Period4_VL_1.csv")

write.csv(data_Period4_VL_2,"data_Period4_VL_2.csv")

write.csv(data_Period4_VL_3,"data_Period4_VL_3.csv")

write.csv(data_Period4_VL_4,"data_Period4_VL_4.csv")



############ FOr Period4 - medium activity################

Eve_MActivity_bins_Power <- discretizeUnsupervised(data_Period4_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   categories = 4, method = "Cluster")

Eve_MActivity_bins_Light <- discretizeUnsupervised(data_Period4_M$data_light_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                   
                                                   categories = 3, method = "Cluster")

Eve_MActivity_bins_CO2 <- discretizeUnsupervised(data_Period4_M$data_CO2_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                 
                                                 categories = 4, method = "Cluster")


data_Period4_M$motion_label <- 0

data_Period4_M$motion_label[which(data_Period4_M$motion_total==0)] <- "no movement"

data_Period4_M$motion_label[which(data_Period4_M$motion_total >=1 & data_Period4_M$motion_total <=2)] <- "Less movement"

data_Period4_M$motion_label[which(data_Period4_M$motion_total >=3)] <- "frequent movement"



data_Period4_M$light_label <- Eve_MActivity_bins_Light[["Disc.data"]]

data_Period4_M$power_label <- Eve_MActivity_bins_Power[["Disc.data"]]


data_Period4_M$co2_label <- Eve_MActivity_bins_CO2[["Disc.data"]]


data_Period4_M <- data_Period4_M[, -23]

data_Period4_M <- data_Period4_M[c(1,19:22,24,25,2:18,23,26:29)]


write.csv(data_Period4_M,"data_Period4_M.csv")


### Seperating days with respect to different clusters##########

###Very Medium activity####

ind_eve_1 <- b_1[which(b_1 %in% a_1)]

ind_eve_2 <- b_1[which(b_1 %in% a_2)]

ind_eve_3 <- b_1[which(b_1 %in% a_3)]

ind_eve_4 <- b_1[which(b_1 %in% a_4)]




data_Period4_M_1 <- data_Period4_M[which(data_Period4_M$day_index_num %in% ind_eve_1),]

data_Period4_M_2 <- data_Period4_M[which(data_Period4_M$day_index_num %in% ind_eve_2),]

data_Period4_M_3 <- data_Period4_M[which(data_Period4_M$day_index_num %in% ind_eve_3),]

data_Period4_M_4 <- data_Period4_M[which(data_Period4_M$day_index_num %in% ind_eve_4),]


write.csv(data_Period4_M_1,"data_Period4_M_1.csv")

write.csv(data_Period4_M_2,"data_Period4_M_2.csv")

write.csv(data_Period4_M_3,"data_Period4_M_3.csv")

write.csv(data_Period4_M_4,"data_Period4_M_4.csv")



############ For Evening - High activity############

Eve_HighActivity_bins_Power <- discretizeUnsupervised(data_Period4_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                                      
                                                      categories = 4, method = "Cluster")

Eve_HighActivity_bins_Light <- discretizeUnsupervised(data_Period4_H$data_light_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                      
                                                      categories = 4, method = "Cluster")

Eve_HighActivity_bins_CO2 <- discretizeUnsupervised(data_Period4_H$data_CO2_Period4, labels = TRUE, infinite_bounds = FALSE,
                                                    
                                                    categories = 4, method = "Cluster")


data_Period4_H$motion_label <- 0

data_Period4_H$motion_label[which(data_Period4_H$motion_total==0)] <- "no movement"

data_Period4_H$motion_label[which(data_Period4_H$motion_total >=1 & data_Period4_H$motion_total <=2)] <- "Less movement"

data_Period4_H$motion_label[which(data_Period4_H$motion_total >=3)] <- "frequent movement"



data_Period4_H$light_label <- Eve_HighActivity_bins_Light[["Disc.data"]]

data_Period4_H$power_label <- Eve_HighActivity_bins_Power[["Disc.data"]]


data_Period4_H$co2_label <- Eve_HighActivity_bins_CO2[["Disc.data"]]

data_Period4_H <- data_Period4_H[, -23]

data_Period4_H <- data_Period4_H[c(1,19:22,24,25,2:18,23,26:29)]


write.csv(data_Period4_H,"data_Period4_High.csv")

### Seperating days with respect to different clusters##########

###High activity####

ind_eve_1 <- b_2[which(b_2 %in% a_1)]

ind_eve_2 <- b_2[which(b_2 %in% a_2)]

ind_eve_3 <- b_2[which(b_2 %in% a_3)]

ind_eve_4 <- b_2[which(b_2 %in% a_4)]


data_Period4_H_1 <- data_Period4_H[which(data_Period4_H$day_index_num %in% ind_eve_1),]

data_Period4_H_2 <- data_Period4_H[which(data_Period4_H$day_index_num %in% ind_eve_2),]

data_Period4_H_3 <- data_Period4_H[which(data_Period4_H$day_index_num %in% ind_eve_3),]

data_Period4_H_4 <- data_Period4_H[which(data_Period4_H$day_index_num %in% ind_eve_4),]


write.csv(data_Period4_H_1,"data_Period4_H_1.csv")

write.csv(data_Period4_H_2,"data_Period4_H_2.csv")

write.csv(data_Period4_H_3,"data_Period4_H_3.csv")

write.csv(data_Period4_H_4,"data_Period4_H_4.csv")



#################### PERIOD5############

data_motion_Period5 <- Minami_182_Motion_10min[which(Minami_182_Motion_10min$Day.period=="Period5"),c(1:15,18)]

data_power_Period5 <- minami_182_power_processed[which(minami_182_power_processed$Day.period=="Period5"),c(4:9)]

data_CO2_Period5 <- minami_182_indoor_processed[which(minami_182_indoor_processed$Day.period=="Period5"),22]


data_light_Period5 <- minami_182_Light_processed[which(minami_182_Light_processed$Day.period=="Period5"),9]


data_Period5 <- cbind(data_motion_Period5,data_CO2_Period5,data_light_Period5,data_power_Period5)


data_Period5$day_index_num <- format(data_Period5$Time,"%Y-%m-%d")


data_Period5$season <- Func.season(data_Period5$Month)


data_Period5_VL <- data_Period5[which(data_Period5$day_index_num %in% b_1),]
data_Period5_M <- data_Period5[which(data_Period5$day_index_num %in% b_2),]
data_Period5_L <- data_Period5[which(data_Period5$day_index_num %in% b_3),]
data_Period5_H <- data_Period5[which(data_Period5$day_index_num %in% b_4),]



############## Low period################

library("arc")


Night_L_bins_Power <- discretizeUnsupervised(data_Period5_L$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                            
                                              categories = 4, method = "Cluster")

Night_L_bins_Light <- discretizeUnsupervised(data_Period5_L$data_light_Period5, labels = TRUE, infinite_bounds = FALSE,
                                            
                                            categories = 4, method = "Cluster")

Night_L_bins_CO2 <- discretizeUnsupervised(data_Period5_L$data_CO2_Period5, labels = TRUE, infinite_bounds = FALSE,
                                          
                                          categories = 4, method = "Cluster")


data_Period5_L$motion_label <- 0

data_Period5_L$motion_label[which(data_Period5_L$motion_total==0)] <- "no movement"

data_Period5_L$motion_label[which(data_Period5_L$motion_total==1)] <- " One movement"

#data_Period5_L$motion_label[which(data_Period5_L$motion_total >=1 & data_Period5_L$motion_total <=3)] <- "Less movement"

data_Period5_L$motion_label[which(data_Period5_L$motion_total >1)] <- "frequent movement"



data_Period5_L$light_label <- Night_L_bins_Light[["Disc.data"]]

data_Period5_L$power_label <- Night_L_bins_Power[["Disc.data"]]


data_Period5_L$co2_label <- Night_L_bins_CO2[["Disc.data"]]

data_Period5_L <- data_Period5_L[, -23]


data_Period5_L <- data_Period5_L[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period5_L, file = "data_Period5_Low.csv")


### Seperating days with respect to different clusters##########

###Low activity####

ind_Night_1 <- b_3[which(b_3 %in% a_1)]

ind_Night_2 <- b_3[which(b_3 %in% a_2)]

ind_Night_3 <- b_3[which(b_3 %in% a_3)]

ind_Night_4 <- b_3[which(b_3 %in% a_4)]


data_Period5_L_1 <- data_Period5_L[which(data_Period5_L$day_index_num %in% ind_Night_1),]

data_Period5_L_2 <- data_Period5_L[which(data_Period5_L$day_index_num %in% ind_Night_2),]

data_Period5_L_3 <- data_Period5_L[which(data_Period5_L$day_index_num %in% ind_Night_3),]

data_Period5_L_4 <- data_Period5_L[which(data_Period5_L$day_index_num %in% ind_Night_4),]


write.csv(data_Period5_L_1,"data_Period5_L_1.csv")

write.csv(data_Period5_L_2,"data_Period5_L_2.csv")

write.csv(data_Period5_L_3,"data_Period5_L_3.csv")

write.csv(data_Period5_L_4,"data_Period5_L_4.csv")


##############High period################

library("arc")


Night_H_bins_Power <- discretizeUnsupervised(data_Period5_H$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_H_bins_Light <- discretizeUnsupervised(data_Period5_H$data_light_Period5, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_H_bins_CO2 <- discretizeUnsupervised(data_Period5_H$data_CO2_Period5, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           categories = 4, method = "Cluster")


data_Period5_H$motion_Label <- 0

data_Period5_H$motion_Label[which(data_Period5_H$motion_total==0)] <- "no movement"

#data_Night_H$motion_Label[which(data_Night_H$motion_total==1)] <- " One movement"

data_Period5_H$motion_Label[which(data_Period5_H$motion_total >=1 & data_Period5_H$motion_total <=2)] <- "Less movement"

data_Period5_H$motion_Label[which(data_Period5_H$motion_total >=3)] <- "frequent movement"



data_Period5_H$light_Label <- Night_H_bins_Light[["Disc.data"]]

data_Period5_H$power_Label <- Night_H_bins_Power[["Disc.data"]]


data_Period5_H$co2_Label <- Night_H_bins_CO2[["Disc.data"]]

data_Period5_H <- data_Period5_H[, -23]

data_Period5_H <- data_Period5_H[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period5_H, file = "data_Period5_High.csv")


### Seperating days with respect to different clusters##########

###High activity####

ind_Night_1 <- b_4[which(b_4 %in% a_1)]

ind_Night_2 <- b_4[which(b_4 %in% a_2)]

ind_Night_3 <- b_4[which(b_4 %in% a_3)]

ind_Night_4 <- b_4[which(b_4 %in% a_4)]


data_Period5_H_1 <- data_Period5_H[which(data_Period5_H$day_index_num %in% ind_Night_1),]

data_Period5_H_2 <- data_Period5_H[which(data_Period5_H$day_index_num %in% ind_Night_2),]

data_Period5_H_3 <- data_Period5_H[which(data_Period5_H$day_index_num %in% ind_Night_3),]

data_Period5_H_4 <- data_Period5_H[which(data_Period5_H$day_index_num %in% ind_Night_4),]


write.csv(data_Period5_H_1,"data_Period5_H_1.csv")

write.csv(data_Period5_H_2,"data_Period5_H_2.csv")

write.csv(data_Period5_H_3,"data_Period5_H_3.csv")

write.csv(data_Period5_H_4,"data_Period5_H_4.csv")


##############MEdium period################

library("arc")


Night_M_bins_Power <- discretizeUnsupervised(data_Period5_M$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_M_bins_Light <- discretizeUnsupervised(data_Period5_M$data_light_Period5, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_M_bins_CO2 <- discretizeUnsupervised(data_Period5_M$data_CO2_Period5, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           categories = 4, method = "Cluster")


data_Period5_M$motion_Label <- 0

data_Period5_M$motion_Label[which(data_Period5_M$motion_total==0)] <- "no movement"

#data_Night_M$motion_Label[which(data_Night_M$motion_total==1)] <- " One movement"

data_Period5_M$motion_Label[which(data_Period5_M$motion_total >=1 & data_Period5_M$motion_total <=2)] <- "Less movement"

data_Period5_M$motion_Label[which(data_Period5_M$motion_total >=3)] <- "frequent movement"



data_Period5_M$light_Label <- Night_M_bins_Light[["Disc.data"]]

data_Period5_M$power_Label <- Night_M_bins_Power[["Disc.data"]]


data_Period5_M$co2_Label <- Night_M_bins_CO2[["Disc.data"]]

data_Period5_M <- data_Period5_M[, -23]


data_Period5_M <- data_Period5_M[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period5_M, file = "data_Period5_Medium.csv")


### Seperating days with respect to different clusters##########

###High activity####

ind_Night_1 <- b_2[which(b_2 %in% a_1)]

ind_Night_2 <- b_2[which(b_2 %in% a_2)]

ind_Night_3 <- b_2[which(b_2 %in% a_3)]

ind_Night_4 <- b_2[which(b_2 %in% a_4)]




data_Period5_M_1 <- data_Period5_M[which(data_Period5_M$day_index_num %in% ind_Night_1),]

data_Period5_M_2 <- data_Period5_M[which(data_Period5_M$day_index_num %in% ind_Night_2),]

data_Period5_M_3 <- data_Period5_M[which(data_Period5_M$day_index_num %in% ind_Night_3),]

data_Period5_M_4 <- data_Period5_M[which(data_Period5_M$day_index_num %in% ind_Night_4),]


write.csv(data_Period5_M_1,"data_Period5_M_1.csv")

write.csv(data_Period5_M_2,"data_Period5_M_2.csv")

write.csv(data_Period5_M_3,"data_Period5_M_3.csv")

write.csv(data_Period5_M_4,"data_Period5_M_4.csv")




############## VLow period################

library("arc")


Night_L_bins_Power <- discretizeUnsupervised(data_Period5_VL$Total.KPWR, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_L_bins_Light <- discretizeUnsupervised(data_Period5_VL$data_light_Period5, labels = TRUE, infinite_bounds = FALSE,
                                             
                                             categories = 4, method = "Cluster")

Night_L_bins_CO2 <- discretizeUnsupervised(data_Period5_VL$data_CO2_Period5, labels = TRUE, infinite_bounds = FALSE,
                                           
                                           categories = 4, method = "Cluster")


data_Period5_VL$motion_label <- 0

data_Period5_VL$motion_label[which(data_Period5_VL$motion_total==0)] <- "no movement"

data_Period5_VL$motion_label[which(data_Period5_VL$motion_total==1)] <- " One movement"

#data_Period5_L$motion_label[which(data_Period5_L$motion_total >=1 & data_Period5_L$motion_total <=3)] <- "Less movement"

data_Period5_VL$motion_label[which(data_Period5_VL$motion_total >1)] <- "frequent movement"



data_Period5_VL$light_label <- Night_L_bins_Light[["Disc.data"]]

data_Period5_VL$power_label <- Night_L_bins_Power[["Disc.data"]]


data_Period5_VL$co2_label <- Night_L_bins_CO2[["Disc.data"]]

data_Period5_VL <- data_Period5_VL[, -23]


data_Period5_VL <- data_Period5_VL[c(1,19:22,24,25,2:18,23,26:29)]



write.csv(data_Period5_VL, file = "data_Period5_VLow.csv")


### Seperating days with respect to different clusters##########

###Low activity####

ind_Night_1 <- b_1[which(b_1 %in% a_1)]

ind_Night_2 <- b_1[which(b_1 %in% a_2)]

ind_Night_3 <- b_1[which(b_1 %in% a_3)]

ind_Night_4 <- b_1[which(b_1 %in% a_4)]


data_Period5_VL_1 <- data_Period5_VL[which(data_Period5_VL$day_index_num %in% ind_Night_1),]

data_Period5_VL_2 <- data_Period5_VL[which(data_Period5_VL$day_index_num %in% ind_Night_2),]

data_Period5_VL_3 <- data_Period5_VL[which(data_Period5_VL$day_index_num %in% ind_Night_3),]

data_Period5_VL_4 <- data_Period5_VL[which(data_Period5_VL$day_index_num %in% ind_Night_4),]


write.csv(data_Period5_VL_1,"data_Period5_VL_1.csv")

write.csv(data_Period5_VL_2,"data_Period5_VL_2.csv")

write.csv(data_Period5_VL_3,"data_Period5_VL_3.csv")

write.csv(data_Period5_VL_4,"data_Period5_VL_4.csv")


##Finding the frequency_energy

rel_freq_energy_P1 <- table(data_morning_L_2$Total.KPWR)

freq_energy_P1 <- table(data_morning_L_2$Total.KPWR)/length(data_morning_L_2$Total.KPWR)

barplot(freq_energy_P1, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow")
box()


matrix <- cbind(prop.table(table(data_morning_L_2$Total.KPWR)))
barplot(matrix)

