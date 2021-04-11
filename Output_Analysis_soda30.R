OutputData <- read.csv("Output_1mill_soda30.csv", header=T)

pool.residuals <- matrix(0,dim(OutputData)[1],dim(OutputData)[2])
pool.fit <- matrix(0,dim(OutputData)[1],dim(OutputData)[2])

for (i in 1:dim(OutputData)[2]) {
  dfr <- data.frame(OutputData[,i],1:dim(OutputData)[1])
  names(dfr) <-  c("measure","time")
  lmf <-  loess(measure~-1+time , data = dfr)
  
  pool.residuals[,i] <- lmf$residuals
  pool.fit[,i] <- lmf$fitted
}

changePointDFR <- data.frame(pool.residuals[,c(1,2,5)])

poolChangePoints <- cpt.locations.multi(changePointDFR, 0.05)
poolChangePoints <- append(poolChangePoints, c(1,dim(OutputData)[1]), after = length(poolChangePoints))
poolChangePoints <- sort(poolChangePoints)
poolChangePoints
