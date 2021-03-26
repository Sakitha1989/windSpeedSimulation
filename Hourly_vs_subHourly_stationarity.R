data <- read.csv("1-OK.csv", skip = 3 , header=T)

indx <-  matrix(0, 288, 12)
subHourlyData <-  matrix(0, 288, 12)
hourlyData <- matrix(0, 24, 12)

subHourlyModel <- vector("list", 1)
hourlyModel <- vector("list", 1)
subHourlyPvalue <- vector("numeric", 1)
hourlyPvalue <- vector("numeric", 1)

days <- c(31,17,5,24,26,13,9,19,21,8,12,24)

for(i in 1:12){
  indx[,i] <-  which(data$Month==i & data$Day==days[i])
  subHourlyData[,i] <- data[indx[,i], 7]
  dfr <-  data.frame(subHourlyData[,i],1:288)
  names(dfr) <-  c("speed","time")
  subHourlyModel[[i]] = loess(speed~-1+time , data = dfr)
  subHourlyTest <- PP.test(subHourlyModel[[i]]$residuals)
  subHourlyPvalue[i] <- subHourlyTest$p.value
  
  hourlyData[,i] <- rollapply(subHourlyData[,i], 12, mean, by = 12)
  Hdfr <-  data.frame(hourlyData[,i],1:length(hourlyData[,i]))
  names(Hdfr) <-  c("speed","time")
  hourlyModel[[i]] = loess(speed~-1+time , data = Hdfr)
  hourlyTest <- PP.test(hourlyModel[[i]]$residuals)
  hourlyPvalue[i] <- hourlyTest$p.value
}

bars <- rbind(hourlyPvalue, subHourlyPvalue)
rownames(bars) <-  c("Hourly","Sub-hourly")
columnNames <-  c("Jan-31","Feb-17","Mar-05","Apr-24","May-26","Jun-13","Jul-09","Aug-19","Sep-21","Oct-08","Nov-12","Dec-24")
barplot(bars, main="Phillips-Perron Unit Root Test",
        xlab="Day", ylab="p-value", col=c("darkblue","red"),
        legend.text = rownames(bars), names.arg = columnNames, las=2, beside=TRUE)
abline(h=0.05, lty = 2, lwd = 3, col = "grey")
text(35,0.065,"0.05")