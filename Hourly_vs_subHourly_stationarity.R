multiSubHourlyPvalue <- vector("numeric", 1)
multiHourlyPvalue <- vector("numeric", 1)

hourlyResiduals <- matrix(0, 24, 5)
uniSubHourlyPvalue <- matrix(0, 12, 5)
uniHourlyPvalue <- matrix(0, 12, 5)

# selected days from Jan to Dec
days <- c(31,17,5,24,27,13,9,19,21,8,12,24)

numStations <- 5
numObs <- 288
numMonths <- 1
numDays <- 1

for(i in 1:12){
  
  subHourlyData <- ReadingFiles(numStations, numObs, numMonths, numDays, i, days[i])
  
  # multi-variate test for sub-hourly data
  multiSubHourlyPvalue[i] <- test_stationarity(subHourlyData$residual, Boot = 30)
  
  for (j in 1:ncol(subHourlyData$residual)) {
    
    # univariate test for sub-hourly data
    uniSubHourlyPvalue[i,j] <- stationarity.testRI_updated(subHourlyData$residual[,j], 1, 3)
    
    #calculating hourly data
    hourlyData <- rollapply(subHourlyData$original[,j], 12, mean, by = 12)
    Hdfr <-  data.frame(hourlyData,1:length(hourlyData))
    names(Hdfr) <-  c("speed","time")
    hourlyModel = loess(speed~-1+time , data = Hdfr)
    hourlyResiduals[,j] <-  hourlyModel$residual
    
    # univariate test for hourly data
    uniHourlyPvalue[i,j] <- stationarity.testRI_updated(hourlyResiduals[,j], 1, 3)
  }
  
  # multi-variate test for sub-hourly data
  multiHourlyPvalue[i] <- test_stationarity(hourlyResiduals[,1:2], Boot = 30)
}

#############################################################################################################
#multivariate plot
multiBars <- rbind(multiHourlyPvalue, multiSubHourlyPvalue)
rownames(multiBars) <-  c("Hourly","Sub-hourly")
columnNames <-  c("Jan-31","Feb-17","Mar-05","Apr-24","May-27","Jun-13","Jul-09","Aug-19","Sep-21","Oct-08","Nov-12","Dec-24")
barplot(multiBars, xlab="Day", ylab="p-value", col=c("darkblue","red"),
         names.arg = columnNames, las=2, beside=TRUE)
legend(x = 1, y = 1, legend = rownames(multiBars), fill = c(4,2))
abline(h=0.05, lty = 2, lwd = 3, col = "grey")
text(1,0.09,"0.05")

#############################################################################################################
#univariate plot
uniBars <- rbind(c(uniHourlyPvalue), c(uniSubHourlyPvalue))
rownames(uniBars) <-  c("Hourly","Sub-hourly")
columnNames <-  c("Jan-31","Feb-17","Mar-05","Apr-24","May-27","Jun-13","Jul-09","Aug-19","Sep-21","Oct-08","Nov-12","Dec-24")
columnNames <- rep(columnNames, 5)
barplot(uniBars, xlab="Day", ylab="p-value", col=c("darkblue","red"), names.arg = columnNames, las=2, beside=TRUE,
        cex.names = 0.5)
legend(x = 1, y = 1, legend = rownames(uniBars), fill = c(4,2))
abline(h=0.05, lty = 2, lwd = 3, col = "grey")
text(0.0005,0.09,"0.05")


##############################################################################################################
par(mfrow=c(1,1))
ccf(decompseData$residual[1:48,1], decompseData$residual[1:48,2], main = "First 48 observations of series 1 & 2")
ccf(decompseData$residual[240:288,1], decompseData$residual[240:288,2], main = "Last 48 observations of series 1 & 2")
ccf(decompseData$residual[1:48,1], decompseData$residual[1:48,3], main = "First 48 observations of series 1 & 3")
ccf(decompseData$residual[240:288,1], decompseData$residual[240:288,3], main = "Last 48 observations of series 1 & 3")
ccf(decompseData$residual[1:48,2], decompseData$residual[1:48,3], main = "First 48 observations of series 2 & 3")
ccf(decompseData$residual[240:288,2], decompseData$residual[240:288,3], main = "Last 48 observations of series 2 & 3")

acf(decompseData$residual, 10, type = "covariance")



