
library(dplyr)
Minami182_10min_index <- Minami182_10min_index %>%
  mutate(window_total= KOCL113D.00+KOCL113E.00+KOCL113F.00+KOCL1140.00+KOCL1141.00+KOCL1142.00+KOCL1143.00)


Minami182_10min_index$window_total[which(Minami182_10min_index$window_total>1)] <- 1


day_ind <- unique(data_morning$day_index_num)


data_window <- Minami182_10min_index$window_total[which(Minami182_10min_index$day_index %in% day_ind & Minami182_10min_index$Day.period=='Morning')]

data_morning$data_window <- data_window


summary(data_morning)





####

waste_count <- data_Night %>%
  
  filter(data_Night$Total.KPWR >= 40, data_Night$motion_total <=1) %>%
  
  group_by(day_index_num) %>%
  
  summarise(instances = length(day_index_num))


####Relative frequency######

prop.table(table(data_morning_VL$motion_total))


#matrix<- cbind(prop.table(table(data_Night$motion_total)))
#colnames(matrix) <- c("Relative frequency")

barplot(prop.table(table(data_morning_VL$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 1), col = "yellow", main = "Very low activity cluster - Occupancy distribution", cex.axis = 1.5, cex.lab=1.5)
box()

prop.table(table(data_morning_L$motion_total))
barplot(prop.table(table(data_morning_L$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 0.4), col = "yellow", main = "Low activity cluster - Occupancy distribution", cex.axis = 1.5, cex.lab=1.5)
box()

prop.table(table(data_morning_M$motion_total))
barplot(prop.table(table(data_morning_M$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 0.2), col = "yellow", main = "Medium activity cluster - Occupancy distribution", cex.axis = 1.5, cex.lab=1.5)
box()


prop.table(table(data_morning_H$motion_total))
barplot(prop.table(table(data_morning_H$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 0.2), col = "yellow", main = "High activity cluster - Occupancy distribution", cex.axis = 1.5, cex.lab=1.5)
box()


#### Morning Energy 

prop.table(table(data_morning_L_2$Total.KPWR))
barplot(prop.table(table(data_morning_L_2$Total.KPWR)), xlab = "Total energy consumption (Wh)", ylab = "Relative Frequency", ylim = c(0.00, 0.025), col = "Black", cex.axis = 1.2, cex.lab=1.5)
box()

prop.table(table(data_morning_VL_1$Total.KPWR))
barplot(prop.table(table(data_morning_L_1$Total.KPWR)), xlab = "Total number of movements", ylab = "Relative Frequency", col = "Green", main = "High activity cluster - Occupancy distribution")
box()



#### Afternoon
prop.table(table(data_Afternoon_VL$motion_total))
barplot(prop.table(table(data_Afternoon_VL$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 1), col = "yellow", main = "Very low activity cluster - Occupancy distribution")
box()

prop.table(table(data_Afternoon_L$motion_total))
barplot(prop.table(table(data_Afternoon_L$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 0.4), col = "yellow", main = "Low activity cluster - Occupancy distribution")
box()

prop.table(table(data_Afternoon_M$motion_total))
barplot(prop.table(table(data_Afternoon_M$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 0.2), col = "yellow", main = "Medium activity cluster - Occupancy distribution")
box()


prop.table(table(data_Afternoon_H$motion_total))
barplot(prop.table(table(data_Afternoon_H$motion_total)), xlab = "Total number of movements", ylab = "Relative Frequency", ylim = c(0.00, 0.2), col = "yellow", main = "High activity cluster - Occupancy distribution")
box()

#######
prop.table(table(data_morning_L_2$motion_total))


matrix<- cbind(prop.table(table(data_morning_VL$motion_total)))
colnames(matrix) <- c("Relative frequency")

barplot(prop.table(table(data_morning_L_2$motion_total)))


