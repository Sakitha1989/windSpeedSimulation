##############################################################################
# ReadingFiles.R                                                             #
#                                                                            #
# Created on: Sep 2, 2020                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#------------------------ Reading data from file-----------------------------#

ReadingFiles <- function(numStations, numObs, numMonths, numDays){
  
  xt = matrix(0,numObs,numStations*numMonths*numDays) # de-trended time series
  ut = matrix(0,numObs,numStations*numMonths*numDays) # trend component of the time series
  yt = matrix(0,numObs,numStations*numMonths*numDays) # original time series
  
  selectedMonths <- rep(0,numMonths)
  selectedDays <- rep(0,numDays)
  
  for (s in 0:3) # s <- number of seasons
  {
    month.select <-  floor(runif(1,3+s*3,6+s*3))
    if(month.select>12) month.select <- month.select-12
    selectedMonths[s+1] <- month.select
    
    days.select = floor(runif(1,1,30))
    selectedDays[s+1] <- days.select
    
    fnm = paste0(1:numStations,"-OK.csv")
    for(i in 1:numStations)
    {
      x = read.csv(fnm[i] , skip = 3 , header=T)
      
      indx = which(x$Month==month.select & x$Day %in% days.select )
      
      y = x[indx,7] # wind speed
      
      # Smoothing and subtracting
      dfr = data.frame(y,1:length(y))
      names(dfr) = c("speed","time")
      lmf = loess(speed~-1+time , data = dfr) # local polynomial regression fit
      
      yt[,i+s*numStations] = y
      xt[,i+s*numStations] = y - lmf$fitted
      ut[,i+s*numStations] = lmf$fitted
      
    }
  }
  decompseData <- list("original" = yt, "trend" = ut, "residual" = xt)
  return(decompseData)
  
}