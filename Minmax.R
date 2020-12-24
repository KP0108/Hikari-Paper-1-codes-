#any(is.na(minami182_09_new1$KTCO1145))
#sum(is.na(minami182_09_new1$KTCO1144))
#colSums(is.na(minami182_09_new1))
#ncol(minami182_01_new1)
#is.na(minami182_09_new1$KTCO1144)
#which(is.na(minami182_09_new1$KTCO1144[1:89]))
 
 
 Func.minmax <- function (ts){
   ts.min <- min(ts, na.rm = TRUE)
     ts.max <- max(ts, na.rm = TRUE)
     (ts-ts.min)/(ts.max-ts.min)
 }

 
