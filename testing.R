##############################################################################
# ReadingFiles.R                                                             #
#                                                                            #
# Created on: Dec 6, 2020                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#--------------------------------- Testing ----------------------------------#

testingSimulations <- function(simData,seriesNumber, numStations, alpha){
  
  xt = matrix(0,length(simData[seriesNumber,,1]),numStations) # de-trended time series
  ut = matrix(0,length(simData[seriesNumber,,1]),numStations) # trend component of the time series
  yt = matrix(0,length(simData[seriesNumber,,1]),numStations) # original time series
  
  for (s in 1:numStations) {
    dfr = data.frame(simData[seriesNumber,,s],1:length(simData[seriesNumber,,s]))
    names(dfr) = c("speed","time")
    lmf = loess(speed~-1+time , data = dfr)
    
    yt[,s] = simData[seriesNumber,,s]
    xt[,s] = simData[seriesNumber,,s] - lmf$fitted
    ut[,s] = lmf$fitted
  }
  SimulatedData <- list("original" = yt, "trend" = ut, "residual" = xt)
  
  changePoints <- cpt.locations.multi(SimulatedData$residual[,c(1:numStations)],alpha)
  changePoints <- append(changePoints, c(1,numObs), after = length(changePoints))
  changePoints <- sort(changePoints)
  
  return(changePoints)
}
