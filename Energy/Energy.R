####
library(dplyr)

library(ggplot2)

Minami182_10min <- read.csv("Minami182_10min_Processed_Latest.csv")

Minami182_10min_index <- write.csv(Minami182_10min, file = "Minami182_10min_index.csv")


# index 10min

Minami182_10min <-  Minami182_10min %>%
  group_by(Month, Day) %>%
  mutate(index_10min = seq(from=1, to =length(Day), by=1))


tenmin_data <- Minami182_10min %>%
  
  group_by(Month, index_10min) %>%
  
  summarise(tenmin_mean=mean(Total.KPWR))

# Change point for 10 min data
w <- tenmin_data[which(tenmin_data$Month=='12'),]

# Change point
library("changepoint")

z <- cpt.mean(w$tenmin_mean, Q=5, method="BinSeg")
cpts(z)
plot(z,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='December', xaxt='n')
axis(1,c(12,24,36,48,60,72,84,96,108,120,132,144))

summary(w$tenmin_mean)
logLik(z)


## hourly data
hourly_data <- Minami182_10min %>%
  
  group_by(Month, Hour) %>%
  
  summarise(hourly_mean=mean(Total.KPWR)*6)

## daytype
hourly_daytp_data <- Minami182_10min %>%
  
  group_by(DayOfWeek, Hour) %>%
  
  summarise(hourly_mean1=mean(Total.KPWR)*6)


monthly_data <- Minami182_10min %>%
  
  group_by(Month) %>%
  
  summarise(month_total=sum(Total.KPWR))



daily_data <- Minami182_10min %>%
  
  group_by(Month, Day) %>%
  
  summarise(daily_total=sum(Total.KPWR))

data12 <- Minami182_10min_Processed_2_Copy[which(Minami182_10min_Processed_2_Copy$Weekday_weekend=="weekday"),]


daily_data$Month <- as.factor(daily_data$Month)

hourly_data$seq <- rep(seq(from=0, to=23, by=1),12)
hourly_data$Month <- as.factor(hourly_data$Month)

monthly_data$seq <- seq(from=1, to=12, by=1)




ggplot(hourly_data, aes(x= seq, y= hourly_mean, color= Month)) + geom_line() + labs(x = "Time of day", y= "Average energy consumption (Wh)")+ ggtitle("Minami 182, Average hourly energy consumption")

plot(x=monthly_data$seq, y=monthly_data$month_total,type="l",xlab="Month",ylab="Total energy consumption(Wh)",main = "Monthly energy consumption - Minami 182", xaxt="n",col="red")

axis(side=1,at=seq(1,12,by=1), las = 1)


boxplot(daily_total~Month, data=daily_data, main="Daily energy consumption-Minami 182", font.main=3, cex.main=1.2, xlab="Month", ylab="Total energy consumption(wh)", font.lab=3, col="yellow")

#Changepoint hourly data
CP <- hourly_data[which(hourly_data$Month=="12"),]

# Change point
library("changepoint")

h <- cpt.mean(CP$hourly_mean, Q=5, method="BinSeg")
cpts(h)
plot(h, xlab="Time(hr)",ylab="Average energy consumption (Wh)", main ='December', xaxt='n')
axis(1,c(2,4,6,8,10,12,14,16,18,20,22,24))


#Changepoint hourly data
CP <- hourly_daytp_data[which(hourly_daytp_data$DayOfWeek=="Friday"),]

# Change point
library("changepoint")

h_tp <- cpt.mean(CP$hourly_mean1, penalty = 'AIC', Q=5, method="BinSeg")
cpts(h_tp)
plot(h_tp, xlab="Time(hr)",ylab="Average energy consumption (Wh)", main ='Monday', xaxt='n')
axis(1,c(2,4,6,8,10,12,14,16,18,20,22,24))



#### 10 min data _ CP analysis_ Daily basis

# index 10min

Minami182_10min <-  Minami182_10min %>%
  group_by(DayOfWeek, Month, Day) %>%
  mutate(index_10min = seq(from=1, to =length(Day), by=1))

tenmin_data_day1 <- Minami182_10min %>%
  
  group_by(DayOfWeek, index_10min) %>%
  
  summarise(tenmin_mean=mean(Total.KPWR))

# Change point for 10 min data
min_daily <- tenmin_data_day1[which(tenmin_data_day1$DayOfWeek=='Friday'),]

# Change point
library("changepoint")

cpt_daily <- cpt.mean(min_daily$tenmin_mean, Q=6, penalty = "AIC", method="BinSeg")
cpts(cpt_daily)
plot(cpt_daily,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='Energy analysis - Friday - Mean')


##Finding the frequency on daily basis
vec_df_daily <- c(1,	9,	17,	19,	19,	23,
                  8,	11,	12,	13,	18,	19,
                  2,	8,	11,	13,	17,	23,
                  2,	8,	11,	13,	19,	20,
                  1,	7,	10,	12,	18,	23,
                  8,	11,	13,	18,	19,	23,
                  2,	8,	11,	13,	19,	23)



rel_freq_daily <- table(vec_df_daily)

freq_daily <- table(vec_df_daily)/length(vec_df_daily)

barplot(freq_daily, xlab = "Time (hrs)", ylab = "Relative Frequency", ylim = c(0.0, 0.20), col = "yellow", main = "CPA of Power consumption trend - Building 2")

box()



##Finding the frequency on daily basis, Q=4, Just a try
vec_df_daily_Q4 <- c(9,	17,	19,	23, 8, 13,	18,	19, 2,	8,	17,	22, 8, 13,	19,	20, 1,	7,	10,	12,	
                     8,	13,	18,	23, 2,	8,	11, 24)

rel_freq_daily_Q4 <- table(vec_df_daily_Q4)

freq_daily_Q4 <- table(vec_df_daily_Q4)/length(vec_df_daily_Q4)

barplot(freq_daily_Q4)

##Finding the frequency on monthly basis (Don't consider, Just a try)
vec_df_Monthly <- c(8,	11,	13,	19,	21,
                  8,	11,	13,	17,	21,
                  8,	11,	13,	17,	20,
                  2,	8,	9,	11,	13,
                  2,	8,	11,	13,	15,
                  9,	11,	13,	15,	16,
                  8,	11,	12,	18,	21,
                  7,	12,	13,	18,	21,
                  2,	7,	8,	18,	20,
                  8,	10,	13,	18,	21,
                  2,	7,	8,	17,	20,
                  2,	7,	8,	18,	20)

rel_freq_monthly <- table(vec_df_Monthly)

freq_Monthly <- table(vec_df_Monthly)/length(vec_df_Monthly)
barplot(freq_Monthly, col = 'YELLOW')


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


Minami182_10min$Season <- Func.season(Minami182_10min$Month)
Minami182_10min <- Minami182_10min[c(1:8, 149, 9:148)]

####### season #######
library(dplyr)
Minami182_Winter <- Minami182_10min %>%
  
  filter(Season=="Winter")

Minami182_Summer <- Minami182_10min %>%
  
  filter(Season=="Summer")

Minami182_Autumn <- Minami182_10min %>%
  
  filter(Season=="Autumn")

Minami182_Spring <- Minami182_10min %>%
  
  filter(Season=="Spring")



#### 10 min data _ CP analysis_ Winter Season

tenmin_data_winter <- Minami182_Winter %>%
  
  group_by(DayOfWeek, index_10min) %>%
  
  summarise(tenmin_mean=mean(Total.KPWR))

# Change point for 10 min data
Energy_Winter_CPT <- tenmin_data_winter[which(tenmin_data_winter$DayOfWeek=='Sunday'),]

# Change point
library("changepoint")

cpt_winter <- cpt.mean(Energy_Winter_CPT$tenmin_mean, Q=10, penalty = "AIC", method="BinSeg")
cpts(cpt_winter)
plot(cpt_winter,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='Energy analysis - Monday - Minami 182')

##Finding the frequency on Winter
vec_df_Winter <- c(3,	8,	9,	9,	15,	15,	16,	19,	19,	23,
                   3,	8,	11,	12,	12,	12,	13,	17,	18,	20,
                   3,	8,	9,	9,	15,	15,	16,	19,	19,	23,
                   1,	7,	8,	12,	12,	13,	14,	19,	20,	20,
                   2,	8,	11,	11,	13,	16,	16,	18,	21,	22,
                   8,	12,	14,	16,	17,	19,	19,	20,	21,	22,
                   2,	8,	10,	11,	12,	13,	19,	21,	22,	23)

rel_freq_Winter <- table(vec_df_Winter)

freq_winter <- table(vec_df_Winter)/length(vec_df_Winter)

barplot(freq_winter, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow", main = "CPA of Energy-Daily basis, Q=10")
box()

#### 10 min data _ CP analysis_ Summer Season

tenmin_data_summer <- Minami182_Summer %>%
  
  group_by(DayOfWeek, index_10min) %>%
  
  summarise(tenmin_mean=mean(Total.KPWR))

# Change point for 10 min data
Energy_Summer_CPT <- tenmin_data_summer[which(tenmin_data_summer$DayOfWeek=='Sunday'),]

# Change point
library("changepoint")

cpt_Summer <- cpt.mean(Energy_Summer_CPT$tenmin_mean, Q=10, penalty = "AIC", method="BinSeg")
cpts(cpt_Summer)
plot(cpt_Summer,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='Energy analysis - Monday - Minami 182')


##Finding the frequency on Summer
vec_df_Summer <- c(8,	9,	12,	12,	13,	18,	21,	21,	22,	23,
                   8,	8,	9,	9,	12,	12,	13,	16,	18,	21,
                   2,	8,	9,	11,	11,	12,	12,	13,	15,	23,
                   1,	8,	11,	13,	15,	19,	19,	20,	21,	22,
                   8,	10,	10,	11,	12,	12,	16,	16,	18,	21,
                   8,	10,	11,	11,	12,	12,	13,	14,	15,	20,
                   7,	11,	12,	15,	15,	17,	17,	19,	19,	23)

rel_freq_Summer <- table(vec_df_Summer)

freq_summer <- table(vec_df_Summer)/length(vec_df_Summer)

barplot(freq_summer, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow", main = "CPA of Energy-Daily basis, Q=10")
box()

#### 10 min data _ CP analysis_ Autumn Season

tenmin_data_autumn <- Minami182_Autumn %>%
  
  group_by(DayOfWeek, index_10min) %>%
  
  summarise(tenmin_mean=mean(Total.KPWR))



# Change point for 10 min data
Energy_Autumn_CPT <- tenmin_data_autumn[which(tenmin_data_autumn$DayOfWeek=='Sunday'),]

# Change point
library("changepoint")

cpt_Autumn <- cpt.mean(Energy_Autumn_CPT$tenmin_mean, Q=10, penalty = "AIC", method="BinSeg")
cpts(cpt_Autumn)
plot(cpt_Autumn,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='Energy analysis - Monday - Minami 182')




##Finding the frequency on Autumn
vec_df_Autumn <- c(3,	8,	11,	12,	13,	16,	16,	17,	19,	19,
                   1,	7,	11,	17,	18,	18,	19,	19,	22,	22,
                   0,	8,	10,	12,	12,	13,	15,	18,	20,	22,
                   2,	7,	9,	9,	11,	12,	17,	19,	19,	20,
                   1,	8,	10,	12,	12,	15,	16,	16,	18,	23,
                   2,	8,	9,	11,	12,	12,	13,	18,	20,	23,
                   2,	8,	11,	13,	18,	19,	20,	22,	23,	24)

rel_freq_Autumn <- table(vec_df_Autumn)

freq_Autumn <- table(vec_df_Autumn)/length(vec_df_Autumn)

barplot(freq_Autumn, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow", main = "CPA of Energy-Daily basis, Q=10")
box()



#### 10 min data _ CP analysis_ Spring Season

tenmin_data_spring <- Minami182_Spring %>%
  
  group_by(DayOfWeek, index_10min) %>%
  
  summarise(tenmin_mean=mean(Total.KPWR))

# Change point for 10 min data
Energy_Spring_CPT <- tenmin_data_spring[which(tenmin_data_spring$DayOfWeek=='Sunday'),]

# Change point
library("changepoint")

cpt_Spring <- cpt.mean(Energy_Spring_CPT$tenmin_mean, Q=10, penalty = "AIC", method="BinSeg")
cpts(cpt_Spring)
plot(cpt_Spring,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='Energy analysis - Monday - Minami 182')

##Finding the frequency on Spring
vec_df_Spring <- c(0,	8,	10,	12,	12,	12,	12,	14,	16,	23,
                   8,	12,	12,	13,	13,	18,	18,	18,	19,	19,
                   2,	9,	12,	12,	13,	15,	16,	19,	20,	21,
                   2,	8,	11,	12,	12,	12,	16,	18,	20,	23,
                   1,	7,	8,	10,	10,	10,	12,	12,	13,	14,
                   2,	8,	9,	10,	11,	12,	12,	13,	16,	23,
                   1,	2,	2,	8,	9,	12,	13,	16,	17,	19)

rel_freq_Spring <- table(vec_df_Spring)

freq_Spring <- table(vec_df_Spring)/length(vec_df_Spring)

barplot(freq_Spring, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow", main = "CPA of Energy-Daily basis, Q=10")
box()

##Finding the frequency on Spring
vec_df_energyallseason <- c(3,	8,	9,	9,	15,	15,	16,	19,	19,	23,
                            3,	8,	11,	12,	12,	12,	13,	17,	18,	20,
                            3,	8,	9,	9,	15,	15,	16,	19,	19,	23,
                            1,	7,	8,	12,	12,	13,	14,	19,	20,	20,
                            2,	8,	11,	11,	13,	16,	16,	18,	21,	22,
                            8,	12,	14,	16,	17,	19,	19,	20,	21,	22,
                            2,	8,	10,	11,	12,	13,	19,	21,	22,	23,
                            8,	9,	12,	12,	13,	18,	21,	21,	22,	23,
                            8,	8,	9,	9,	12,	12,	13,	16,	18,	21,
                            2,	8,	9,	11,	11,	12,	12,	13,	15,	23,
                            1,	8,	11,	13,	15,	19,	19,	20,	21,	22,
                            8,	10,	10,	11,	12,	12,	16,	16,	18,	21,
                            8,	10,	11,	11,	12,	12,	13,	14,	15,	20,
                            7,	11,	12,	15,	15,	17,	17,	19,	19,	23,
                            3,	8,	11,	12,	13,	16,	16,	17,	19,	19,
                            1,	7,	11,	17,	18,	18,	19,	19,	22,	22,
                            0,	8,	10,	12,	12,	13,	15,	18,	20,	22,
                            2,	7,	9,	9,	11,	12,	17,	19,	19,	20,
                            1,	8,	10,	12,	12,	15,	16,	16,	18,	23,
                            2,	8,	9,	11,	12,	12,	13,	18,	20,	23,
                            2,	8,	11,	13,	18,	19,	20,	22,	23,	24,
                            0,	8,	10,	12,	12,	12,	12,	14,	16,	23,
                            8,	12,	12,	13,	13,	18,	18,	18,	19,	19,
                            2,	9,	12,	12,	13,	15,	16,	19,	20,	21,
                            2,	8,	11,	12,	12,	12,	16,	18,	20,	23,
                            1,	7,	8,	10,	10,	10,	12,	12,	13,	14,
                            2,	8,	9,	10,	11,	12,	12,	13,	16,	23,
                            1,	2,	2,	8,	9,	12,	13,	16,	17,	19)
                            


rel_freq_energyallseason <- table(vec_df_energyallseason)

freq_allseason <- table(vec_df_energyallseason)/length(vec_df_energyallseason)

barplot(freq_allseason, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow", main = "CPA of Energy-Daily basis- All season, Q=10")
box()


##Finding the frequency on Spring
vec_df_energyallseason1 <- c(9,	17,	19,	19,	23,
                            8,	11,	13,	18,	19,
                            2,	8,	13,	17,	22,
                            8,	11,	13,	19,	20,
                            1,	7,	10,	12,	23,
                            8,	11,	13,	18,	23,
                            2,	8,	11,	13,	24)




rel_freq_energyallseason1 <- table(vec_df_energyallseason1)

freq_allseason1 <- table(vec_df_energyallseason1)/length(vec_df_energyallseason1)

barplot(freq_allseason1, xlab = "Time (hrs)", ylab = "Frequency", col = "yellow", main = "CPA of Energy-Daily basis- All season, Q=10")
box()




# Change point
library("changepoint")

cpt_season <- cpt.meanvar(Season_daily$tenmin_mean, Q=5, penalty = "AIC", method="BinSeg")
cpts(cpt_season)
plot(cpt_season,type="l",xlab="Time(10 min)",ylab="Average energy consumption (Wh)", main ='Energy analysis - Monday - Minami 182')


##Finding the frequency on daily basis
vec_df_daily <- c(9,	17,	19,	19,	23, 8,	11,	13,	18,	19, 2,	8,	13,	17,	22, 8, 11,	13,	19,	20, 1,	7,	10,	12,	23,
                  8,	11,	13,	18,	23, 2,	8,	11,	13,	24)

rel_freq_daily <- table(vec_df_daily)

freq_daily <- table(vec_df_daily)/length(vec_df_daily)

barplot(freq_daily, xlab = "Time (hrs)", ylab = "Frequency", ylim = c(0.00, 0.35), col = "yellow", main = "CPA of Energy-Daily basis")
box()



##Finding the frequency on daily basis, Q=4, Just a try
vec_df_daily_Q4 <- c(9,	17,	19,	23, 8, 13,	18,	19, 2,	8,	17,	22, 8, 13,	19,	20, 1,	7,	10,	12,	
                     8,	13,	18,	23, 2,	8,	11, 24)

rel_freq_daily_Q4 <- table(vec_df_daily_Q4)

freq_daily_Q4 <- table(vec_df_daily_Q4)/length(vec_df_daily_Q4)

barplot(freq_daily_Q4)

##Finding the frequency on monthly basis (Don't consider, Just a try)
vec_df_Monthly <- c(8,	11,	13,	19,	21,
                    8,	11,	13,	17,	21,
                    8,	11,	13,	17,	20,
                    2,	8,	9,	11,	13,
                    2,	8,	11,	13,	15,
                    9,	11,	13,	15,	16,
                    8,	11,	12,	18,	21,
                    7,	12,	13,	18,	21,
                    2,	7,	8,	18,	20,
                    8,	10,	13,	18,	21,
                    2,	7,	8,	17,	20,
                    2,	7,	8,	18,	20)

rel_freq_monthly <- table(vec_df_Monthly)

freq_Monthly <- table(vec_df_Monthly)/length(vec_df_Monthly)
barplot(freq_Monthly, col = 'YELLOW')

