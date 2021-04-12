# The function dft and dft.covariance.standardized.lagl are required to construct the test
# statistic. dft.covariance.standardized.lagl requires a lag variable to be given to it. This is
# \el 


dft <- function(x){
  
  n=length(x)
  
  dft <- fft(x)/sqrt(n)
  
  return(dft)
  
}


# choose any lag l, from 0 to length of the time series/2. However, the larger the lag
# the less likely you are to `see' the nonstationarity.

dft.covariance.standardized.lagl <- function(test,l){
  
  #####################################
  # First take DFT of data
  #####################################  
  
  temp1 <- dft(test)  
  
  
  ##################################
  # In the next few steps we define the function
  #  \frac{1}{n}\sum_{k=1}^{n}J_{n}(\omega_{k})
  #       \overline{J}_{n}(\omega_{k+r})exp(il\omega_{k})/\sqrt{f(\omega_{k})f(\omega_{k+r})}              
  #
  # Ie. The standardized DFT. Of course in the periodic-stationary case we need to
  # properly define f(\omega), but it is basically E(|J_{T}(omega_{k})|^{2}) in the
  # in the periodic case which needs to be calculated.                 
  
  
  # To estimate the spectral density function I use the Daniell kernel and
  # with 4 points on either side (no real reason for choosing this)                
  
  k<-kernel("daniell",6)
  
  temp2 <-spec.pgram(test,k, taper=0, log = "no" , plot = F)$spec
  
  n <- length(temp2)
  
  temp3 <- c(temp2[c(1:n)],temp2[c(n:1)])
  
  # In temp4 we are standardizing the DFT
  
  temp4 <- temp1/sqrt(temp3)
  
  # Take the dft of the standardized DFT and modulo square it
  
  temp5 <- Mod(dft(temp4))**2
  
  # Temp6 should give us  \frac{1}{n}\sum_{k=1}^{n}J_{n}(\omega_{k})
  #       \overline{J}_{n}(\omega_{k+r})/\sqrt{f(\omega_{k})f(\omega_{k+r})}
  # (through DFTs and convolutions)                
  
  temp6 <- fft(temp5, inverse = TRUE)/(2*n) 
  
  return(temp6)
  
}

# IMPORTANT THE ABOVE DFT COVARIANCE USES A VERY CRUDE ESTIMATOR OF THE SPECTRAL DENSITY FUNCTION. THE RECTANGLE KERNEL USING A LOCAL AVERAGE
# OF 13 LAGS. THIS IS REGARDLESS OF THE SAMPLE SIZE. A BETTER ESTIMATOR WOULD BE TO USE A DATA ADAPTIVE METHOD.



###############################################################################################################################

# PLEASE NOTE THAT THE INPUT test refers to the time series you want to check. 

# To visually `see' if there is nonstationarity, make a plot of the real and imaginary parts of the DFT covariance.
# l refers to the lag you want to use. Typically, you can set l=0.

dftcov <- dft.covariance.standardized.lagl(test,l)


# The test statistic, first choose the number of lags r in your vector and also number of l.
# Typically, you can just fix l and choose the number of lags (this is what has been done below) and the real and imaginary parts.
# But you can choose different l and r, and whether you want to look at just the real or imaginary parts


stationarity.testRI_updated <- function(test,l,r){
  
  cov.dfts <- dft.covariance.standardized.lagl(test,l)
  
  Rcov.dfts <- sqrt(length(test))*Re(cov.dfts)
  Icov.dfts <- sqrt(length(test))*Im(cov.dfts)
  
  Rcov.select <- Rcov.dfts[c(2:(r+1))]
  Icov.select <- Icov.dfts[c(2:(r+1))]           
  
  test.statistic <- (sum(Rcov.select**2) + sum(Icov.select**2))
  
  p <- (1-pchisq(test.statistic,2*r)) 
  
  return(p)
  
}

stationarity.testR <- function(test,l,r){
  
  cov.dfts <- dft.covariance.standardized.lagl(test,l)
  
  Rcov.dfts <- sqrt(length(test))*Re(cov.dfts)
  
  Rcov.select <- Rcov.dfts[c(2:(r+1))]
  
  test.statistic <- (sum(Rcov.select**2))
  
  p <- (1-pchisq(test.statistic,r)) 
  
  return(p)
  
}


stationarity.testI <- function(test,l,r){
  
  cov.dfts <- dft.covariance.standardized.lagl(test,l)
  
  Icov.dfts <- sqrt(length(test))*Im(cov.dfts)
  
  Icov.select <- Icov.dfts[c(2:(r+1))]           
  
  test.statistic <- (sum(Icov.select**2))
  
  p <- (1-pchisq(test.statistic,r)) 
  
  return(p)
  
}

