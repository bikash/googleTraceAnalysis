#################################################################################################
#################################################################################################
## Author: Bikash Agrawal
## Date: 12th March Feb 2015
## Email: er.bikash21@gmail.com
## Description: Anamoly detection in google cluster data trace 2011.
##          
## References: 
## [1] http://dnene.bitbucket.org/docs/mlclass-notes/lecture16.html
## [2] 
## 
#################################################################################################
#################################################################################################
library(gmm)


### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/google/data/")

## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(cpurate=data$X0.03143, memory_usage=data$X0.05389 , page_cache=data$X0.006645 , diskio_time=data$X7.629e.05 , cycle_inst=data$X2.911)

Data1 <- Data[1:10000,]

## Omit NA and data imputation before doing PCA analysis
data1 <- data.frame(cpurate=Data1$cpurate, memory_usage=Data1$memory_usage, page_cache = Data1$page_cache, diskio_time=Data1$diskio_time)

## plotting cpu rate and memory usage
pc <- princomp(data1, cor = TRUE, na.action=na.exclude)
plot(pc)

# First component dominates greatly. What are the loadings?
summary(pc) # 1 component has > 99% variance
loadings(pc) # Can see all variance is in the range in miles 

# verify by plotting variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(data, var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)



# Proceed with principal components
pc <- princomp(data1)
plot(pc)
plot(pc, type='l')
summary(pc) # 4 components is both 'elbow' and explains >85% variance

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(data1)
# First for principal components
comp <- data.frame(pc$x[,1:4])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))
library(rgl)
plot3d(comp$PC1, comp$PC2, comp$PC3)


#Do the PCA on your dataframe 
exp.data.selected.pca=prcomp(t(data1))

library(rgl)
#Extract the first three principal components. This is what you will plot.
plotme=cbind(data1$x[,1:3])

#This is the code to draw the 3d plot. Once executed, the plot in the X11 window can be rotated by clicking and draging with your mouse.
plot3d(plotme,col=cols,type="s",size=1.5,xlim=c(-2,2),ylim=c(-2,2),zlim=c(-2,2),box=FALSE)
for(i in row.names(plotme)){
  segments3d(c(plotme[i,1],plotme[i,1]),c(plotme[i,2],2),c(plotme[i,3],plotme[i,3]),col="grey89",lwd=3)
}

#This removes the default bounding box so it can be replaced with something more aesthetically pleasing
rgl.clear(type="bbox")


