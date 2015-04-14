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


makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
  legend ("topleft", legend =names, cex=0.8, col=colours,lwd=c(1,1,1,1), lty=c(1,1,1,1))
}
### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/google/data/")

## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(cpurate=data$X0.03143, memory_usage=data$X0.05389 , page_cache=data$X0.006645 , diskio_time=data$X7.629e.05 , cycle_inst=data$X2.911)

Data1 <- Data[1:10000,]
## plotting cpu rate and memory usage
#pdf("graph/CPUusage_memory.pdf",bg="white")
png("graph/CPUusage_memory.png")
x = Data1$cpurate
y = Data1$memory_usage
y_range <- range(0, y)
x_range <- range(0, x)
plot(x,y, ylim=y_range, xlim=x_range, xlab="CPU usage", ylab="Memory usage", xaxt="n")
points(x, y, lwd=1, col="red")
axis(1, at=x, labels=x)
dev.off()
#################################################################################################
#################################################################################################

### CPU usage density function
plot(hist(Data1$cpurate,breaks=101),col="grey",border="grey",freq=FALSE,
     xlab="CPU usage",main="")
lines(density(Data1$cpurate),lty=2)

### Memmory usage density function
plot(hist(Data1$memory_usage,breaks=101),col="grey",border="grey",freq=FALSE,
     xlab="Memory usage",main="")
lines(density(Data1$memory_usage),lty=2)

### PRincipal component Analysis
## Log transformation
data.log <- log(Data1)
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
fit <- prcomp(Data1[,1:4],
                 center = TRUE,
                 scale. = TRUE) 
print(fit)
summary(fit)
# sqrt of eigenvalues
fit$sdev

plot(fit, type = "l")

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(fit, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

library("car")
scatterplotMatrix(Data1)

library(RColorBrewer)
names <- c("CPU rate","Memory usage","Page Cache","Disk I/O time")
mylist <- list(Data1$cpurate,Data1$memory_usage,Data1$page_cache,Data1$diskio_time)
makeProfilePlot(mylist,names)


### Gaussian Mixture Model (GMM)
# Two-component Gaussian mixture
library(mixtools)
fit.k2 <- normalmixEM(Data1$cpurate,k=2,maxit=100,epsilon=0.01)

plot(fit.k2,which=2)
lines(density(Data1$cpurate), lty=2, lwd=2, xlab="CPU usage")



# Two-component Gaussian mixture
library(mixtools)
fit.k1 <- normalmixEM(Data1$memory_usage,k=2,maxit=100,epsilon=0.01)

plot(fit.k1,which=2)
lines(density(Data1$memory_usage), lty=2, lwd=2, xlab="Memory usage")



# Determine number of clusters
library(fpc)
wss <- (nrow(Data1)-1)*sum(apply(Data1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Data1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# K-Means Cluster Analysis
fit <- kmeans(Data1, 6) # 6 cluster solution
# get cluster means 
aggregate(Data1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(Data1, fit$cluster)

# vary parameters for most readable graph
png("graph/CLUSPLOT.png")
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
dev.off()

require(vegan)
fit <- cascadeKM(scale(mydata, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 5 clusters!

# Centroid Plot against 1st 2 discriminant functions
png("graph/2df.png")
library(fpc)
plotcluster(mydata, fit$cluster)
dev.off()

# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)





## Log transformed
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

xlog = signedlog10(x)
ylog = signedlog10(y)

plot(exp(xlog + ylog^2/2), type="l", lwd=2, ylim=c(0,250)); 

points(1, type="l", lwd=2, lty=2)


##
