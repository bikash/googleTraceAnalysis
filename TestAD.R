
### setting path of repo folder.
setwd("/Users/bikash/repos/google/data/")

source("/Users/bikash/repos/googleTraceAnalysis/tsmeasures.R")
source("/Users/bikash/repos/googleTraceAnalysis/anomay.R")
source("/Users/bikash/repos/googleTraceAnalysis/figfunc.R")
## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)

setwd("/Users/bikash/repos/googleTraceAnalysis/")

ts = rnorm(70, 1, 1)
ts[68:70] = 40
x1= length(ts)
y.max= max(ts)
y.min=min(ts)
plot(NULL, xlim=c(0,x1), ylim=c(y.min,y.max), xlab="Time in seconds", ylab="", main ="Time series" )
lines(ts, lwd=2, col="red",lty=6)
ggplot_AnomalyDetection.rpca(AnomalyDetection.rpca(ts, autodiff=T)) + theme_grey(base_size = 20)


ts1 = cumsum(rnorm(70, 1, 1))
ts1[68:70] = 100
x1= length(ts1)
y.max= max(ts1)
y.min=min(ts1)
plot(NULL, xlim=c(0,x1), ylim=c(y.min,y.max), xlab="Time in seconds", ylab="", main ="Time series" )
lines(ts1, lwd=2, col="red",lty=6)
ggplot_AnomalyDetection.rpca(AnomalyDetection.rpca(ts1, autodiff=T,L.penalty = 1,)) + theme_grey(base_size = 20)



## plot graph for anomaly detection


## testing in google data
ts.cpu <- Data1[1:350,1]
ts1 = (ts.cpu)
x1= length(ts1)
y.max= max(ts1)
y.min=min(ts1)
plot(NULL, xlim=c(0,x1), ylim=c(y.min,y.max), xlab="Time in seconds", ylab="CPU usage" )
lines(ts1, lwd=2, col="black",lty=1)
ggplot_ADM(AnomalyDetection.rpca(ts1, autodiff=T, L.penalty = 100,s.penalty=0.4 )) 

## testing in google data memory
ts.mem <- Data1[1:350,2]
ts1 = cumsum(ts.mem)
x1= length(ts1)
y.max= max(ts1)
y.min=min(ts1)
plot(NULL, xlim=c(0,x1), ylim=c(y.min,y.max), xlab="Time in seconds", ylab="memory utilization", )
lines(ts1, lwd=2, col="black",lty=1)
ggplot_ADM(AnomalyDetection.rpca(ts1, autodiff=T, L.penalty = 100,s.penalty=0.4 )) 
