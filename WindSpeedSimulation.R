##############################################################################
# WindSpeedSimulation.R                                                      #
#                                                                            #
# Created on: Dec 3, 2020                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#--------------------------------- Main -------------------------------------#

# List of Required Libraries #

library(resamplr)
library(tseries)
library(abind)
#library(smooth)

numStations <- 5
numObs <- 288
numMonths <- 1
numDays <- 1
numSimulations <- 500

alpha <- 0.05

# reading files
decompseData <- ReadingFiles(numStations, numObs, numMonths, numDays)

# function returns change point locations
changePoints <- cpt.locations.multi(decompseData$residual[,c(1:numStations)],alpha)
changePoints <- append(changePoints, c(1,numObs), after = length(changePoints))
changePoints <- sort(changePoints)

# parametric simulation
parametricSimulations <- parametricSimulation(decompseData, changePoints, numObs, numStations, numSimulations)
#colnames(parametricSimulations) <- c(paste0("S1.",1:288),paste0("S2.",1:288),paste0("S3.",1:288),paste0("S4.",1:288),paste0("S5.",1:288))
write.csv(parametricSimulations,"C:\\Users\\sakit\\Documents\\Academic\\Research\\StatisticalModeling\\WindSpeedDataAnalysisNREL\\Codes\\windSpeedSimulation\\windSpeedSimulation\\Output\\parametricSimulation.csv", row.names = TRUE)

# non-parametric simulation
nonParametricSimulations <- nonParametricSimulation(decompseData, changePoints, numStations, numObs, numSimulations)
write.csv(nonParametricSimulations,"C:\\Users\\sakit\\Documents\\Academic\\Research\\StatisticalModeling\\WindSpeedDataAnalysisNREL\\Codes\\windSpeedSimulation\\windSpeedSimulation\\Output\\nonParametricSimulation.csv", row.names = TRUE)

# testing simulated data
seriesNumber <- 1
parametricSimChangePoints <- testingSimulations(parametricSimulations, seriesNumber, numStations, alpha)
nonParametricSimChangePoints <- testingSimulations(nonParametricSimulations, seriesNumber, numStations, alpha)
  
#plotting
plot.ts(decompseData$original, main = "Original series")
plot.ts(parametricSimulations[seriesNumber,,], main = "Parametric simulated series")
plot.ts(nonParametricSimulations[seriesNumber,,], main = "Non-parametric simulated series")
