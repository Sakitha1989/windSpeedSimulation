###################################################################################################################
#time series plots

matplot(decompseData$original[1:288,], type = "l", lty = 1, lwd = 2, col = 2:6, 
        ylab = "Wind Speed", xlab = "Time", xlim = c(0,300), cex.axis=1.25, cex.lab=1.5) #plot
legend(x=0.5, y=6, legend = paste0("Series ", 1:5), col=2:6, lty=1, cex=1, box.lty=0) # optional legend


matplot(decompseData$original[1:288,], type = "l", lty = 1, lwd = 2, col = 2:6, 
        ylab = "Wind Speed", xlab = "Time", xlim = c(0,300)) #plot
legend(x=0.5, y=6, legend = paste0("Series ", 1:5), col=2:6, lty=1, box.lty=0) # optional legend

matplot(decompseData$residual[1:288,], type = "l", lty = 1, lwd = 2,  col = 2:6,
        ylab = "Residuals", ylim = c(-5,5), xlab = "Time", xlim = c(0,300), cex.axis=1.25, cex.lab=1.5) #plot
legend(x=-11, y=5.25, legend = paste0("Series ", 1:5), col=2:6, lty=1, cex=1, box.lty=0) # optional legend

abline(v=c(151,235), lty = 2, lwd = 2)

####################################################################################################################

matplot(parametricSimulations[1,,1:5], type = "l", lty = 1, lwd = 2, col = 2:6, 
        ylab = "Wind Speed", xlab = "Time", xlim = c(0,300), cex.axis=1.25, cex.lab=1.5) #plot
legend(x=0.5, y=6, legend = paste0("Series ", 1:5), col=2:6, lty=1, cex=1, box.lty=0) # optional legend




####################################################################################################################
#correlation matrix

library("corrplot")

correlationMatrix <- cor(decompseData$original[1:288,])
correlationMatrix
corrplot.mixed(correlationMatrix)


correlationMatrix[upper.tri(correlationMatrix)]<- NA
correlationMatrix <- melt(correlationMatrix, na.rm = TRUE)

ggheatmap <- ggplot(data = correlationMatrix, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "black", high = "grey", mid = "white", midpoint = 0, limit = c(-1,1),name="Correlation") +
  theme_minimal()+
  theme(axis.text.x = element_text(vjust = 1, size = 10, hjust = 1), axis.text.y = element_text(vjust = 1, size = 10, hjust = 1))+ 
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "white", size = 4) +
  theme(
    axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.8, 0.2),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))+
  scale_x_continuous(name="Speed of cars", limits=c(0, 5)) +
  scale_y_continuous(name="Stopping distance", limits=c(0, 5))
  #scale_x_discrete(breaks=c("1","2","3"),
  #                 labels=c("Dose 0.5", "Dose 1", "Dose 2"))+
  #scale_y_discrete(breaks=c("1","2","3"),
  #                 labels=c("Dose 0.5", "Dose 1", "Dose 2"))
print(ggheatmap)

###############################################################################################################
library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
