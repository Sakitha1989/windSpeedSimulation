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
library(reshape2)
library(ggplot2)
#library(smooth)

# directory
setwd("~/Academic/Research/StatisticalModeling/WindSpeedDataAnalysisNREL/Codes/windSpeedSimulation/windSpeedSimulation/Data")

# other R scripts
source('~/Academic/Research/StatisticalModeling/WindSpeedDataAnalysisNREL/Codes/windSpeedSimulation/windSpeedSimulation/ReadingFiles.R')
source('~/Academic/Research/StatisticalModeling/WindSpeedDataAnalysisNREL/Codes/windSpeedSimulation/windSpeedSimulation/np_change_point.R')
source('~/Academic/Research/StatisticalModeling/WindSpeedDataAnalysisNREL/Codes/windSpeedSimulation/windSpeedSimulation/ParametricSimulation.R')
source('~/Academic/Research/StatisticalModeling/WindSpeedDataAnalysisNREL/Codes/windSpeedSimulation/windSpeedSimulation/NonParametricSimulation.R')


numStations <- 5
numObs <- 288
numMonths <- 1
numDays <- 1
numSimulations <- 500

alpha <- 0.05

# reading files
decompseData <- ReadingFiles(numStations, numObs, numMonths, numDays)

# univariate stationary test
for (i in 1 : ncol(decompseData)) {
  
}



# function returns change point locations
changePoints <- cpt.locations.multi(decompseData$residual[1:288,c(1:numStations)],alpha)
changePoints <- append(changePoints, c(1,numObs), after = length(changePoints))
changePoints <- sort(changePoints)
changePoints

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
plot.ts(decompseData$original[,1], main = " ")
plot.ts(decompseData$residual, main = " ")
plot.ts(parametricSimulations[seriesNumber,,], main = "Parametric simulated series")
plot.ts(nonParametricSimulations[seriesNumber,,], main = "Non-parametric simulated series")

plot.ts(decompseData$residual[,5])
abline(v=c(151,235))

PP.test(decompseData$residual[1:151,5])
