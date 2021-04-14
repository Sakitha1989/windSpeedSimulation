##############################################################################
# NonParametricSimulation.R                                                  #
#                                                                            #
# Created on: Mar 27, 2021                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#------------------------ Change point experiments -----------------------------#

numStations <- 5
numObs <- 288
numMonths <- 1
numDays <- 1

month <- 7
day <- 25
expData1 <- ReadingFiles(numStations, numObs, numMonths, numDays, month, day)

month <- 7
day <- 26
expData2 <- ReadingFiles(numStations, numObs, numMonths, numDays, month, day)

for(s in 1:numStations)
{
  # Smoothing and subtracting
  dfr = data.frame(expData1[1:144, s ],1:144)
  names(dfr) = c("speed","time")
  lmf = loess(speed~-1+time , data = dfr) # local polynomial regression fit
  
  yt[,s+(d-1)*numStations] = y
  xt[,s+(d-1)*numStations] = y - lmf$fitted
  ut[,s+(d-1)*numStations] = lmf$fitted
} 



dfr = data.frame(expData1$original[1:144,],1:144)
names(dfr) = c("speed","time")
lmf144 = loess(speed~-1+time , data = dfr) # local polynomial regression fit
  
expData12 <- rbind(expData1$residual, expData2$residual)
dfr = data.frame(expData12,1:576)
names(dfr) = c("speed","time")
lmf576 = loess(speed~-1+time , data = dfr) # local polynomial regression fit

expData <- list(lmf144$residuals, expData1$residual, lmf576$residuals)

alpha <- c(0.01, 0.05, 0.10)

multiSeries <- array(list(), dim = 4)
cpTable <- array(list(),dim=c(3, 4, 3))

for (i in 2:5) {
  multiSeries[i-1][[1]] <- sample(1:5, i, replace=FALSE)
  for (j in 1:3) {
    for (k in 1:3) {
      temp <- cpt.locations.multi(expData[1:(144*j), multiSeries[[i-1]]], alpha[k])
      if(!is.null(temp)){
        cpTable[j,i-1,k][[1]] <- temp
      }
    }
  }
}


# cp05.144.4 <- array(list(),dim=c(10,10,10,10))
# for (k in 1:9) {
#   for (l in k+1:10-k) {
#     if(k<l){
#       for (i in l+1:10-l) {
#         if(l<i){
#           for (j in i+1:10-i) {
#             if(i<j){
#               temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j,l,k)],0.05)
#               if(!is.null(temp)){
#                 cp05.144.4[k,l,i,j][[1]] <- temp
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }