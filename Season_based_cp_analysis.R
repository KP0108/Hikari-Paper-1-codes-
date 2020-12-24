Func.season <- function(month){
  
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

#### Motion Season analysis###

minami182_motion_tot$Season <- Func.season(minami182_motion_tot$Month)

minami182_motion_tot <- minami182_motion_tot[c(1:7,23,8:22)]

minami182_motion_season <- minami182_motion_tot %>%
  group_by(Season, Hour, Minute) %>%
  summarise(occ_count=length(which(motion_tot==1)),
            occ_prob=length(which(motion_tot==1))/length(motion_tot))


Motion_Season <- minami182_motion_season[which(minami182_motion_season$Season=='Autumn'),]

Motion_Season$seq <- seq(1,1441, by=1)

# Change point
library("changepoint")

motion_season_cp <- cpt.meanvar(Motion_Season$occ_prob,penalty = "AIC", Q=7, method="BinSeg")
cpts(motion_season_cp)
plot(motion_season_cp, xlab="Time(min)",ylab="Occupant probability", main ='June', xaxt='n')
axis(1,c(120,240,360,480,600,720,840,960,1080,1200,1320,1440))

motion <- cpt.mean(x$occ_count,method="BinSeg")
cpts(motion)
plot(motion, xlab="Time(min)",ylab="Occupant probability", main ='June - Minami 182', xaxt='n')
axis(1,c(120,240,360,480,600,720,840,960,1080,1200,1320,1440))


#### Energy Season analysis###