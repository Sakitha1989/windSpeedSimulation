OutputData <- read.csv(file.choose())

obj <- matrix(0,dim(OutputData)[1],2)
gen <- matrix(0,dim(OutputData)[1],2)

objDfr <-  data.frame(OutputData$Objective,1:dim(OutputData)[1])
names(objDfr) <-  c("Objective","time")
objLmf <-  loess(Objective~-1+time , data = objDfr)

obj[,1] <- objLmf$residuals
obj[,2] <- objLmf$residuals

changePoints <- cpt.locations.multi(obj, 0.05)
changePoints <- append(changePoints, c(1,dim(OutputData)[1]), after = length(changePoints))
changePoints <- sort(changePoints)
changePoints

plot.ts(OutputData$Objective)
plot.ts(objLmf$residuals)
plot.ts(objLmf$fitted)


plot.ts(OutputData$gen4)
plot.ts(OutputData$gen6)

gen4Dfr <-  data.frame(OutputData$gen4,1:dim(OutputData)[1])
names(gen4Dfr) <-  c("gen","time")
gen4Lmf <-  loess(gen~-1+time , data = gen4Dfr)

gen6Dfr <-  data.frame(OutputData$gen6,1:dim(OutputData)[1])
names(gen6Dfr) <-  c("gen","time")
gen6Lmf <-  loess(gen~-1+time , data = gen6Dfr)

gen[,1] <- gen4Lmf$residuals
gen[,2] <- gen6Lmf$residuals

genChangePoints <- cpt.locations.multi(gen, 0.05)
genChangePoints <- append(genChangePoints, c(1,dim(OutputData)[1]), after = length(genChangePoints))
genChangePoints <- sort(genChangePoints)
genChangePoints

plot.ts(gen4Lmf$residuals)
plot.ts(gen6Lmf$residuals)

#############################################################################
farm1 <- read.csv("1-OK.csv", skip = 3 , header=T)
farm2 <- read.csv("2-OK.csv", skip = 3 , header=T)

indx <-  which(data$Month==7 & data$Day==25)

original <- matrix(0,length(indx),2)

farm1Dfr <-  data.frame(farm1[indx, 7],1:length(indx))
names(farm1Dfr) <-  c("speed","time")
farm1Lmf <-  loess(speed~-1+time , data = farm1Dfr)

farm2Dfr <-  data.frame(farm2[indx, 7],1:length(indx))
names(farm2Dfr) <-  c("speed","time")
farm2Lmf <-  loess(speed~-1+time , data = farm2Dfr)

original[,1] <- farm1Lmf$residuals
original[,2] <- farm1Lmf$residuals


wSppedChangePoints <- cpt.locations.multi(original, 0.05)
wSppedChangePoints <- append(wSppedChangePoints, c(1,dim(original)[1]), after = length(wSppedChangePoints))
wSppedChangePoints <- sort(wSppedChangePoints)
wSppedChangePoints
