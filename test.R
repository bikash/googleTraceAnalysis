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

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/google/data/")

## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(cpurate=data$X0.03143, memory_usage=data$X0.05389)

Data1 <- Data[1:1000,]
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

## Log transformed
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

xlog = signedlog10(x)
ylog = signedlog10(y)

plot(exp(xlog + ylog^2/2), type="l", lwd=2, ylim=c(0,250)); 

points(1, type="l", lwd=2, lty=2)


##
