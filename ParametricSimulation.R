##############################################################################
# ParametricSimulation.R                                                     #
#                                                                            #
# Created on: Sep 2, 2020                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#------------------------ Parametric simulation -----------------------------#

parametricSimulation <- function(data, changePoints, numObs, numStations, numSimulations){

  simSamples = array( 0 , c(numSimulations, numObs, numStations) )
  for (j in 1: numSimulations) {
  selectOrder <- rep(0,length(changePoints)-1)
    residualSeries <- NULL
    for (i in 1:(length(changePoints)-1)) {
      selectOrder[i] <- min(VARselect(data[changePoints[i]:changePoints[i+1]-1,], type = "const", lag.max = 10)$selection)
      varModel <- vars::VAR(data,p=selectOrder[i], type = "const")
      simSeries <- varSimulate(varModel = varModel, simLength = numSimulations, lookaheadPeriods = 50, numScenarios = 1)
      
      index <- sample(1:length(simSeries[,1,1]), if(i==1) {changePoints[i+1] - changePoints[i]+1} else {changePoints[i+1] - changePoints[i]}, replace=FALSE)
      simTS <- simSeries[index, , 1]
      residualSeries <- rbind(residualSeries, simTS)
    }

    simSamples[j,,] <- residualSeries+data$ut
  }
  
  return(simSamples)
  
}