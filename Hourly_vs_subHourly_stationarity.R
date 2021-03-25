data <- read.csv("1-OK.csv", skip = 3 , header=T)

indx1 <-  which(data$Month==1 & data$Day==31)
indx2 <-  which(data$Month==4 & data$Day==24)
indx3 <-  which(data$Month==7 & data$Day==11)
indx4 <-  which(data$Month==9 & data$Day==4)

indx <- data.frame(indx1, indx2, indx3, indx4)
y <-  matrix(0, dim(indx[1]), 4)
hourlyDta <- matrix(0, 24, 4)

subHourlyTrend <- vector("list", 1)
hourlyTrend <- vector("list", 1)

for(i in 1:4){
  y[,i] <- data[indx[,i], 7]
  dfr <-  data.frame(y[,i],1:length(y[,i]))
  names(dfr) <-  c("speed","time")
  subHourlyTrend[[i]] = loess(speed~-1+time , data = dfr)
  
  hourlyDta[,i] <- rollapply(y[,i], 12, mean, by = 12)
  Hdfr <-  data.frame(hourlyDta[,i],1:length(hourlyDta[,i]))
  names(Hdfr) <-  c("speed","time")
  hourlyTrend[[i]] = loess(speed~-1+time , data = Hdfr)

}

# sub-hourly data
plot.ts(subHourlyTrend[[1]]$residuals)
PP.test(subHourlyTrend[[1]]$residuals)

# hourly data
plot.ts(hourlyTrend[[1]]$residuals)
PP.test(hourlyTrend[[1]]$residuals)

# sub-hourly data
plot.ts(subHourlyTrend[[2]]$residuals)
PP.test(subHourlyTrend[[2]]$residuals)

# hourly data
plot.ts(hourlyTrend[[2]]$residuals)
PP.test(hourlyTrend[[2]]$residuals)

# sub-hourly data
plot.ts(subHourlyTrend[[3]]$residuals)
PP.test(subHourlyTrend[[3]]$residuals)

# hourly data
plot.ts(hourlyTrend[[3]]$residuals)
PP.test(hourlyTrend[[3]]$residuals)

# sub-hourly data
plot.ts(subHourlyTrend[[4]]$residuals)
PP.test(subHourlyTrend[[4]]$residuals)

# hourly data
plot.ts(hourlyTrend[[4]]$residuals)
PP.test(hourlyTrend[[4]]$residuals)
