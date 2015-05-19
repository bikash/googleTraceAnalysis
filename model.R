
### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/google/data/")

## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(cpurate=data$X0.03143, memory_usage=data$X0.05389 , page_cache=data$X0.006645 , diskio_time=data$X7.629e.05 , cycle_inst=data$X2.911, 
                   start_date=data$X5612000000)

Data1 <- Data[1:10000,]

## Omit NA and data imputation before doing PCA analysis
#data1 <- data.frame(cpurate=Data1$cpurate, memory_usage=Data1$memory_usage, page_cache = Data1$page_cache, diskio_time=Data1$diskio_time)

## Plot CPU usage
y = Data1$cpurate
x = c(1:length(y))

plot(x, y, type="n", xlab="Time", ylab="Value" ) 
points(y, pch=19, col="red")
lines(y, pch=19, col="red")


## PCA
library(rgl)
plotPCA <- function(x, nGroup) {
  n <- ncol(x) 
  if(!(n %in% c(2,3))) { # check if 2d or 3d
    stop("x must have either 2 or 3 columns")
  }
  
  fit <- hclust(dist(x), method="complete") # cluster
  groups <- cutree(fit, k=nGroup)
  
  if(n == 3) { # 3d plot
    plot3d(x, col=groups, type="s", size=1, axes=F)
    axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
    grid3d("x")
    grid3d("y")
    grid3d("z")
  } else { # 2d plot
    maxes <- apply(abs(x), 2, max)
    rangeX <- c(-maxes[1], maxes[1])
    rangeY <- c(-maxes[2], maxes[2])
    plot(x, col=groups, pch=19, xlab=colnames(x)[1], ylab=colnames(x)[2], xlim=rangeX, ylim=rangeY)
    lines(c(0,0), rangeX*2)
    lines(rangeY*2, c(0,0))
  }
}
pca<-prcomp(na.omit(Data1), scale=TRUE)

plotPCA(pca$x[,1:2], 3)

fit <- hclust(dist(pca$x[,1:3]), method="complete") # 1:3 -> based on 3 components
groups <- cutree(fit, k=5)      
##
periodicity_options <- c('second', 'minute', 'hour', 'day', 'week', 'month', 'year')
date <- as.POSIXct(Data1$start_date, origin="1970-01-01")
date <-  as.POSIXct(c(946684801:946714800), origin="1970-01-01")
dates <- round_date(date, 'second')
input_agg <- aggregate(Data1$cpurate ~ dates, FUN=sum)
# Invoke AnomalyDetectionTs
library(AnomalyDetection)
anoms = AnomalyDetectionTs(input_agg, direction='pos', alpha = 0.05, plot=T,  ylabel='value') 
anoms$plot

AnomalyDetectionVec(input_agg[,2], max_anoms=0.02, period=1400, direction='both', only_last=FALSE, plot=TRUE)


