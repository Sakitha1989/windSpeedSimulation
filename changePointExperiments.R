##############################################################################
# changePointExperiments.R                                                      #
#                                                                            #
# Created on: Apr 1, 2021                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#--------------------------------- Main -------------------------------------#

changePointExperiments <- function(residualsData, alpha, numAlphas, dataLength, numLengths, numSeries){
  
  changePointsVec <- matrix(vector("list", numSeries),numAlpha*numLengths,numSeries)
  vector("list", 9)
  for (i in 2:10) {
    changePoints[i-1] <- list(cpt.locations.multi(decompseData$residual[1:144,c(1:i)],0.05))
  }
  
}