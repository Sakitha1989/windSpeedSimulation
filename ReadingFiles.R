##############################################################################
# ReadingFiles.R                                                             #
#                                                                            #
# Created on: Sep 2, 2020                                                    #
# Author: Skitha Ariyarathne                                                 #
#                                                                            #
##############################################################################

#------------------------ Reading data from file-----------------------------#
# capable of reading multiple days from one month

ReadingFiles <- function(numStations, numObs, numMonths, numDays, month, day){
  
  xt = matrix(0,numObs*numDays,numStations*numMonths) # de-trended time series
  ut = matrix(0,numObs*numDays,numStations*numMonths) # trend component of the time series
  yt = matrix(0,numObs*numDays,numStations*numMonths) # original time series
  
  selectedMonths <- rep(0,numMonths)
  selectedDays <- rep(0,numDays)
  
  fnm = paste0(1:5,"-OK.csv")
  
  for (m in 1:numMonths)
  {
    selectedMonths[m] <- floor(runif(1,1,13))
    
    for (d in 1:numDays) 
    {
      selectedDays[d] <- floor(runif(1,1,30))
      
      for(s in 1:numStations)
      {
        x <-  read.csv(fnm[s] , skip = 1 , header=T)
      
        #indx <-  which(x$Month==selectedMonths[m] & x$Day==selectedDays[d] )
        #indx <-  which(x$Month==11 & (x$Day==13 | x$Day==14) )
        indx <-  which(x$Month==month & (x$Day==day) )
      
        y <-  x[indx,6] # wind speed
      
        # Smoothing and subtracting
        dfr = data.frame(y,1:length(y))
        names(dfr) = c("speed","time")
        lmf = loess(speed~-1+time , data = dfr) # local polynomial regression fit
      
        yt[,s+(d-1)*numStations] = y
        xt[,s+(d-1)*numStations] = y - lmf$fitted
        ut[,s+(d-1)*numStations] = lmf$fitted
      } 
    }
  }
  decompseData <- list("original" = yt, "trend" = ut, "residual" = xt, "Months" = selectedMonths, "Days" = selectedDays)
  return(decompseData)
  
}