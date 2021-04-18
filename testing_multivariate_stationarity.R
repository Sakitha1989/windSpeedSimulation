###########################################################################
### An R function "test_stationarity" to test for stationarity of       ###
### multivariate time series data as proposed in:                       ###
###                                                                     ###
### Jentsch, C. and Subba Rao, S. (2015).                               ###
### A test for second order stationarity of a multivariate time series. ###
### Journal of Econometrics 185, No. 1, 124-161.                        ###
###########################################################################

### For an example see below ###

### Packages required for the function "test_stationarity" ###
library(np)
library(vars)

test_stationarity<-function(x,Boot){

d<-dim(x)[2]
n<-dim(x)[1]

# maximal Number of lags used for computing the test statistic 
m_max<-10

# step size for the bandwidths for cross validation
# for spectral density estimation
h_step<-1/20


# Initialize the test statistic (with bootstrap and without bootstrap))
statistik<-rep(0,m_max+1)
statistik_wb<-rep(0,m_max+1)

# Defining the kernel function (Bartlett-Priestley)
Kern<-function(h,x){
Kern<-matrix(0,length(h),length(x))
for(j in 1:length(h)){
	for(k in 1:length(x)){
		if(abs(x[k])<=pi*h[j]){Kern[j,k]<-(2*pi)*1/h[j]*(3/(4*pi))*(1-(x[k]/(pi*h[j]))^2)}
		else{Kern[j,k]<-0}
	}
}

Kern
}

# Cholesky decomposition for a complex matrix
cchol<-function(x){
cchol<-matrix(rep(0,(dim(x)[1]*dim(x)[1])),dim(x)[1],dim(x)[2])
for(k in 1:(dim(x)[1])){
	for(l in 1:k){
		hilf<-0
		if ((k-1)>0 && k==l) hilf<-t(cchol[k,(1:(k-1))])%*%Conj(cchol[k,(1:(k-1))])
		if (k==l) cchol[k,l]<-sqrt(x[k,l]-hilf)
		if ((l-1)>0 && k>l) hilf<-t(cchol[k,(1:(l-1))])%*%Conj(cchol[l,(1:(l-1))])
		if (k>l) cchol[k,l]<-(1/cchol[l,l])*(x[k,l]-hilf)
	}
}
cchol
}

################################################
### spectral density estimation ###
################################################

# Computing the matrix of mDFT of data x at frequencies 
# \omega_0,...,\omega_{n-1}
Jn<-mvfft(x)/sqrt(2*pi*n)
#Jn

# Arranging a bigger matrix of mDFT of data x at frequencies
# \omega_{-n},...,\omega_{2n-1}
Jnbig<-matrix(rep(0,3*n*d),(3*n),d)
Jnbig<-rbind(Jn[1,],Conj(Jn[n:1,]),Jn[2:n,],Jn[1,],Conj(Jn[n:2,]))
#Jnbig

# Arranging fourier frequencies -pi to 2*pi*(2*n-1)/n
freq<-rep(0,3*n)
freq<-(2*pi/n)*(-n):(2*n-1)

# Arranging a large matrix containing the periodograms
Inbig<-matrix(rep(0,3*n*d^2),d,3*d*n)
for(j in 1:(3*n)){
	Inbig[,((j-1)*d+1):(j*d)]<-Jnbig[j,]%*%t(Conj(Jnbig[j,]))
}

##########################################################################
### Computing the bandwidth h_CV using CV criteria ###
### on a grid of [0,1] with step size h_step       ###
##########################################################################

# g starts at ((2/(h_step*n))+1) not to run into singularities
CV<-rep(0,2/h_step)
CV_new<-rep(0,2/h_step)
for(g in 2:(2/h_step)){
h<-g*h_step
omega<-0
kernvec<-rep(0,n)
fhut_mult_cv<-t(matrix(rep(0,n*d*d/2),(n*d)/2,d))
#fhut_mult<-t(matrix(rep(0,n*d*d/2),(n*d)/2,d))
#rep(0,n/2)
for(k in 1:floor(n/2)){
	omega<-(2*pi/n)*k
	kernvec<-Kern(h,omega-freq)

	kernvec[k+1]<-0
	kernvec[n+1-k]<-0
	kernvec[n+1+k]<-0
	kernvec[2*n+1-k]<-0
	kernvec[2*n+1+k]<-0
	kernvec[3*n+1-k]<-0
	kernvec_matrix<-kronecker(kernvec,diag(1,d))
	# The conjugate due to transposing Inbig (nein)
	#fhut_mult_cv[,((k-1)*d+1):(k*d)]<-(1/n)*(kernvec_matrix%*%t(Inbig))
	fhut_mult_cv[,((k-1)*d+1):(k*d)]<-(1/n)*(Inbig%*%t(kernvec_matrix))
}
#fhut_mult_cv[,((k-1)*d+1):(k*d)]

blub<-0
for(j in 1:floor(n/2)){	#Achtung hier die Indizes so, dass fhut_mult_cv und Inbig passen
	blub<-log(prod(eigen(fhut_mult_cv[,((j-1)*d+1):(j*d)])$values))+sum(diag(Inbig[,((n+j+1-1)*d+1):((n+j+1)*d)]%*%solve(fhut_mult_cv[,((j-1)*d+1):(j*d)])))+blub
}
CV_new[g]<-blub
}

#CV_new
CV_new<-Re(CV_new)

# Choosing the minimizer ((+1 because the loop over g starts in second entry)
h_CV_new<-(which.min(CV_new[2:(2/h_step)])+1)*h_step
#h.vector[t]<-h_CV_new
h<-h_CV_new
#h

#######################################################################################################

# spectral density matrix estimation at 
# fourier frequencies omega_1,...,omega_n (more efficient implementation!)
omega<-0
kernvec<-rep(0,n)
fhut_mult<-t(matrix(rep(0,n*d*d),(n*d),d))
for(k in 1:n){
	omega<-(2*pi/n)*k
	kernvec_matrix<-kronecker(Kern(h,omega-freq),diag(1,d))
	fhut_mult[,((k-1)*d+1):(k*d)]<-(1/n)*(Inbig%*%t(kernvec_matrix))
	###fhut_mult[,((k-1)*d+1):(k*d)]<-(1/n)*(kernvec_matrix%*%t(Inbig))
}
#fhut_mult

# Rearranging and transposing Jn to get Jn_help at frequencies 
# omega_1,...,omega_n
Jn_help<-t(rbind(Jn[2:n,],Jn[1,]))

# Computing the standardized DFT Jn_stand
Jn_stand<-matrix(rep(0,n*d),d,n)
for(k in 1:n){
	Jn_stand[,k]<-solve(cchol(fhut_mult[,((k-1)*d+1):(k*d)]))%*%Jn_help[,k]
}

# Computing covariances of the DFTs from lag 0 to lag n-1
Jn_stand_long<-cbind(Jn_stand,Jn_stand)
DFTcov<-matrix(rep(0,(m_max+1)*d*d),d,(m_max+1)*d)
for(j in 1:(m_max+1)){
	for(k in 1:d){
		for(i in 1:d){
		DFTcov[k,((j-1)*d+i)]<-(1/n)*Jn_stand_long[k,1:n]%*%Conj(Jn_stand_long[i,((1+j-1):(n+j-1))])
		}
	}
}

# Creating a matrix that contains the vectorized DFT covariances
DFTcovvec<-matrix(rep(0,n*d*d),d*d,n)
for(k in 1:(m_max+1)){
	blub<-DFTcov[,((k-1)*d+1):(k*d)]
	dim(blub)<-NULL
	DFTcovvec[,k]<-blub	
}


#########################################################################
### Stationary Bootstrap begins here ###
#########################################################################

########################################################################
### Rule-of-thumb for the tuning parameter l (Politis & White (2003) ###
########################################################################

# Block length selection 
s<-b.star(x,round=FALSE)[,1]
# taking the average of the component-wise chosen block length
l<-mean(s)

#s
#l
p<-1/l
#p

########################################################################
### Stationary bootstrap loop begins here ###
########################################################################

DFTcovstern_all<-matrix(rep(0,(m_max+1)*d*d*Boot),d*d*(m_max+1),Boot)
for(b in 1:Boot){

# Generating the bootstrap series
xstern<-matrix(rep(0,10*n*d),10*n,d)
xlong<-rbind(x,x,x,x,x,x,x,x,x,x)
length<-0
while(length<n){
	L<-sample(1:n,1)
	L
	R<-rgeom(1,p)
	# This ensures that the blocks are not too large
	R<-min(R,n/2)
	R
	xstern[(length+1):(length+1+R),]<-xlong[L:(L+R),]
length<-length+R+1
}
xstern<-xstern[1:n,]
xstern

# Computing the matrix of mDFT of data xstern at frequencies 
# \omega_0,...,\omega_{n-1}
Jnstern<-mvfft(xstern)/sqrt(2*pi*n)
#Jnstern

# Arranging a bigger matrix of mDFT of data x at frequencies
# \omega_{-n},...,\omega_{2n-1}
Jnbigstern<-matrix(rep(0,3*n*d),(3*n),d)
Jnbigstern<-rbind(Jnstern[1,],Conj(Jnstern[n:1,]),Jnstern[2:n,],Jnstern[1,],Conj(Jnstern[n:2,]))
#Jnbigstern

# Arranging fourier frequencies -pi to 2*pi*(2*n-1)/n
freq<-rep(0,3*n)
freq<-(2*pi/n)*(-n):(2*n-1)

# Arranging a large matrix containing the periodograms
Inbigstern<-matrix(rep(0,3*n*d^2),d,3*d*n)
for(j in 1:(3*n)){
	Inbigstern[,((j-1)*d+1):(j*d)]<-Jnbigstern[j,]%*%t(Conj(Jnbigstern[j,]))
}


omega<-0
kernvec<-rep(0,n)
fhut_mult_stern<-t(matrix(rep(0,n*d*d),(n*d),d))
for(k in 1:n){
	omega<-(2*pi/n)*k
	kernvec_matrix<-kronecker(Kern(h,omega-freq),diag(1,d))
	###fhut_mult_stern[,((k-1)*d+1):(k*d)]<-(1/n)*(Inbigstern%*%t(kernvec_matrix))
	fhut_mult_stern[,((k-1)*d+1):(k*d)]<-(1/n)*(kernvec_matrix%*%t(Inbigstern))
}
#fhut_mult_stern


# Rearranging and transposing Jnstern to get frequencies omega_1,...,omega_n
Jnstern_help<-t(rbind(Jnstern[2:n,],Jnstern[1,]))

# Computing the standardized DFT Jnstern_stand
Jnstern_stand<-matrix(rep(0,n*d),d,n)
for(k in 1:n){
	Jnstern_stand[,k]<-solve(cchol(fhut_mult_stern[,((k-1)*d+1):(k*d)]))%*%Jnstern_help[,k]
}

# Computing covariances of the DFTs from lag 0 to lag n-1
Jnstern_stand_long<-cbind(Jnstern_stand,Jnstern_stand)
DFTcovstern<-matrix(rep(0,(m_max+1)*d*d),d,(m_max+1)*d)
for(j in 1:(m_max+1)){
	for(k in 1:d){
		for(i in 1:d){
		DFTcovstern[k,((j-1)*d+i)]<-(1/n)*Jnstern_stand_long[k,1:n]%*%Conj(Jnstern_stand_long[i,((1+j-1):(n+j-1))])
		}
	}
}
#DFTcovstern

# Creating a big matrix that contains the vectorized DFTs
# in its colums for all b
blub<-DFTcovstern
dim(blub)<-NULL
DFTcovstern_all[,b]<-blub

DFTcovstern_all[,b]
}


###########################################################################
### Stationary Bootstrap loop ends here ###
###########################################################################

###########################################################################
### Stationary Bootstrap ends here ###
###########################################################################


# Computing the bootstrap variances of the vectorized DFT covariances
W1<-matrix(rep(0,d*d*d*d*(m_max+1)),d*d,d*d*(m_max+1))
for(k in 1:(m_max+1)){

mean_W1<-rep(0,d*d)
for(q in 1:(d*d)){
	mean_W1[q]<-mean(DFTcovstern_all[(1+(k-1)*d*d+q-1),])
}
	W1[,((k-1)*d*d+1):(k*d*d)]<-(1/Boot)*Re(DFTcovstern_all[((1+(k-1)*d*d):(k*d*d)),]-mean_W1)%*%t(Re(DFTcovstern_all[((1+(k-1)*d*d):(k*d*d)),]-mean_W1))
}

# Computing the bootstrap variances of the vectorized DFT covariances
W2<-matrix(rep(0,d*d*d*d*(m_max+1)),d*d,d*d*(m_max+1))
for(k in 1:(m_max+1)){

mean_W2<-rep(0,d*d)
for(q in 1:(d*d)){
	mean_W2[q]<-mean(DFTcovstern_all[(1+(k-1)*d*d+q-1),])
}
	W2[,((k-1)*d*d+1):(k*d*d)]<-(1/Boot)*Im(DFTcovstern_all[((1+(k-1)*d*d):(k*d*d)),]-mean_W2)%*%t(Im(DFTcovstern_all[((1+(k-1)*d*d):(k*d*d)),]-mean_W2))
}


# Setting the elimination matrix (case with bootstrap)
E<-matrix(0,d*(d+1)/2,d*d)
row_counter<-0
column_counter<-0
for(k in 1:d){
	E[(row_counter+1):(row_counter+d-k+1),(column_counter+1):(column_counter+d-k+1)]<-diag(1,d-k+1)

	row_counter<-row_counter+d-k+1
	column_counter<-column_counter+d-k+1+k
}


# Setting the elimination matrix (case without bootstrap)
E_wb<-matrix(0,d*(d+1)/2,d*d)
row_counter<-0
column_counter<-0
for(k in 1:d){
	E_wb[(row_counter+1):(row_counter+d-k+1),(column_counter+1):(column_counter+d-k+1)]<-diag(c(1,rep(sqrt(2),d-k)),d-k+1)

	row_counter<-row_counter+d-k+1
	column_counter<-column_counter+d-k+1+k
}

# Computing the real and imaginary parts of the (via stationary bootstrap)
# standardized and vech-erized DFT covariances
DFTcovvech_stand_re<-matrix(rep(0,(m_max+1)*d*(d+1)/2),d*(d+1)/2,m_max+1)
DFTcovvech_stand_im<-matrix(rep(0,(m_max+1)*d*(d+1)/2),d*(d+1)/2,m_max+1)
# without Bootstrap
DFTcovvech_stand_re_wb<-matrix(rep(0,(m_max+1)*d*(d+1)/2),d*(d+1)/2,m_max+1)
DFTcovvech_stand_im_wb<-matrix(rep(0,(m_max+1)*d*(d+1)/2),d*(d+1)/2,m_max+1)


for(k in 1:(m_max+1)){
	blub_gesamt<-solve(cchol((1/2)*E%*%W1[,((k-1)*d*d+1):(k*d*d)]%*%t(E)+(1/2)*E%*%W2[,((k-1)*d*d+1):(k*d*d)]%*%t(E)))
	DFTcovvech_stand_re[,k]<-blub_gesamt%*%(E%*%Re(DFTcovvec[,k]))
	DFTcovvech_stand_im[,k]<-blub_gesamt%*%(E%*%Im(DFTcovvec[,k]))
	DFTcovvech_stand_re_wb[,k]<-sqrt(n)*E_wb%*%Re(DFTcovvec[,k])
	DFTcovvech_stand_im_wb[,k]<-sqrt(n)*E_wb%*%Im(DFTcovvec[,k])

}

#################################################################################
### Here start the computations for selction of m (cf, Escanciano and Lobato) ###
#################################################################################

DFTcovvech_stand_acc<-rep(1/(d*(d+1)),d*(d+1))%*%rbind(DFTcovvech_stand_re,DFTcovvech_stand_im)
DFTcovvech_stand_acc_wb<-rep(1/(d*(d+1)),d*(d+1))%*%rbind(DFTcovvech_stand_re_wb,DFTcovvech_stand_im_wb)
#DFTcovvech_stand_acc
#DFTcovvech_stand_acc_wb

vector_portmanteau<-rep(0,m_max)
vector_portmanteau_wb<-rep(0,m_max)
for(k in 1:m_max){
vector_portmanteau[k]<-n*sum(DFTcovvech_stand_acc[2:(k+1)]^2)
vector_portmanteau_wb[k]<-n*sum(DFTcovvech_stand_acc_wb[2:(k+1)]^2)
}
#vector_portmanteau
#vector_portmanteau_wb


if(max(sqrt(n)*abs(DFTcovvech_stand_acc[2:(m_max+1)]))<=sqrt(2.4*log(n))){
vector_penalty<-seq(1,m_max,by=1)*log(n)
}

if(max(sqrt(n)*abs(DFTcovvech_stand_acc_wb[2:(m_max+1)]))<=sqrt(2.4*log(n))){
vector_penalty_wb<-seq(1,m_max,by=1)*log(n)
}

if(max(sqrt(n)*abs(DFTcovvech_stand_acc[2:(m_max+1)]))>sqrt(2.4*log(n))){
vector_penalty<-seq(1,m_max,by=1)*2
}

if(max(sqrt(n)*abs(DFTcovvech_stand_acc_wb[2:(m_max+1)]))>sqrt(2.4*log(n))){
vector_penalty_wb<-seq(1,m_max,by=1)*2
}

# Setting the chosen number of lags used for the test statistic (with bootstrap)
m_opt<-1
maxima<-which((vector_portmanteau[2:m_max]-vector_penalty[2:m_max])-(vector_portmanteau[1:m_max-1]-vector_penalty[1:m_max-1])<=0)
m_opt<-maxima[1]
if(is.na(maxima[1])==TRUE) m_opt<-m_max

# Setting the chosen number of lags used for the test statistic (without bootstrap)
m_opt_wb<-1
maxima_wb<-which((vector_portmanteau_wb[2:m_max]-vector_penalty_wb[2:m_max])-(vector_portmanteau_wb[1:m_max-1]-vector_penalty_wb[1:m_max-1])<=0)
m_opt_wb<-maxima_wb[1]
if(is.na(maxima_wb[1])==TRUE) m_opt_wb<-m_max

##############################################
### Here ends the selection procedure of m ###
##############################################

# Computing the test statistics based on DFT covariance lags up to 1,2,3,...,m_max
for(m in 1:m_max){
	for(k in 1:m){
	statistik[m]<-sum((DFTcovvech_stand_re[,k+1])^2)+sum((DFTcovvech_stand_im[,k+1])^2)+statistik[m]	
	statistik_wb[m]<-sum((DFTcovvech_stand_re_wb[,k+1])^2)+sum((DFTcovvech_stand_im_wb[,k+1])^2)+statistik_wb[m]	
	}
}

# Computing the test statistics with automatically chosen number of DFT covariance lags 
for(k in 1:m_opt){
#statistik[m_max+1]<-sum((DFTcovvech_stand_re[,k+1])^2)+sum((DFTcovvech_stand_im[,k+1])^2)+statistik[m_max+1]	
statistik_wb[m_max+1]<-sum((DFTcovvech_stand_re_wb[,k+1])^2)+sum((DFTcovvech_stand_im_wb[,k+1])^2)+statistik_wb[m_max+1]	
}

kz=cbind(statistik_wb,qchisq(1-0.05,c(1:m_max,m_opt_wb)*d*(d+1)))
#print(kz)
#return(1-pchisq(statistik[m_max+1],df=m_opt*d*(d+1)))

lagsc = 5
return(1-pchisq(statistik_wb[lagsc],df=lagsc*d*(d+1)))


# lagsc = 3
# return(1-pchisq(statistik[lagsc],df=lagsc*d*(d+1)))



} # end function test_stationarity()

################################################
### End of Definition of "test_stationarity" ###
################################################


##############################################################################################
##############################################################################################
##############################################################################################


# Testing Stationarity Univariate Case #

# Dwivedi and Subba Rao (2011), Journal of Time Series Analysis 


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
  
  # If odd no. of observations
  if(length(test)%%2==1) test = test[-length(test)]
  
  temp1 <- dft(test)  
  n1 = length(temp1) # even no.
  
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
  
  temp2 <-spec.pgram(test,kernel=k, taper=0, log = "no", plot=FALSE )$spec
  
  n <- length(temp2)
  
  #print(n)
  #print(length(temp1))
  #print(length(temp3))
  
  temp3 <- c(temp2[c(1:n1)],temp2[c(n1:1)])
  
  # In temp4 we are standardizing the DFT
  
  temp4 <- temp1/sqrt(temp3)
  
  # Take the dft of the standardized DFT and modulo square it
  
  temp5 <- Mod(dft(temp4))**2
  
  # Temp6 should give us  \frac{1}{n}\sum_{k=1}^{n}J_{n}(\omega_{k})
  #       \overline{J}_{n}(\omega_{k+r})/\sqrt{f(\omega_{k})f(\omega_{k+r})}
  # (through DFTs and convolutions)                
  
  temp6 <- fft(temp5, inverse = TRUE)/(2*n1) 
  
  return(temp6)
  
}


# The test statistic, first choose the number of lags r in your vector and also number of l.
# Typically, you can just fix l and choose the number of lags (this is what has been done below) and the real and imaginary parts.
# But you can choose different l and r, and whether you want to look at just the real or imaginary parts


stationarity.testRI <- function(test,l,r){
  
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
