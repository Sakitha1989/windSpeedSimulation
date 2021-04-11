##############################################################################
# NonParametricSimulation.R                                                  #
#                                                                            #
# Created on: Sep 2, 2020                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#------------------------ Non-Parametric simulation -----------------------------#
library(np)

nonParametricSimulation <- function(data, changePoints, numStations, numObs, numSimulations){
  
  simSamples = array( 0 , c(numSimulations, numObs, numStations) )
  simSeries <- NULL
  for (i in 1:(length(changePoints)-1)) {
    stationarySegment <- data$residual[changePoints[i]:changePoints[i+1],] 
    
    # Optimal Block length Selection: Politis and White (2004)
    optimalBlockLengths<-b.star(stationarySegment, round=TRUE)[,1]
    # taking the average of the component-wise chosen block length
    averageBlockLength<-mean(optimalBlockLengths)
    
    # Block bootstrap 
    bootSegments = resamplr::tsbootstrap(data.frame(stationarySegment) , R = numSimulations, size = averageBlockLength, 
                                     m = nrow(stationarySegment), type = "geom" )
    
    bootSamples = array( 0 , c(numSimulations, nrow(stationarySegment), numStations) )
    for(j in 1:numSimulations) {
      tmp = data.frame( bootSegments$sample[[j]] )
      bootSamples[j,,] = as.matrix(tmp, nrow(stationarySegment), numStations) + data$trend[changePoints[i]:changePoints[i+1],]
    }
    simSeries <- abind(simSeries, bootSamples, along = 2)
  }
  return(simSeries)
}