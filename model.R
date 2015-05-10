
### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/google/data/")

## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(cpurate=data$X0.03143, memory_usage=data$X0.05389 , page_cache=data$X0.006645 , diskio_time=data$X7.629e.05 , cycle_inst=data$X2.911, 
                   start_date=data$X5612000000)

Data1 <- Data[1:30000,]

## Omit NA and data imputation before doing PCA analysis
#data1 <- data.frame(cpurate=Data1$cpurate, memory_usage=Data1$memory_usage, page_cache = Data1$page_cache, diskio_time=Data1$diskio_time)

## Plot CPU usage
y = Data1$cpurate
x = c(1:length(y))

plot(x, y, type="n", xlab="Time", ylab="Value" ) 
points(y, pch=19, col="red")
lines(y, pch=19, col="red")

##
periodicity_options <- c('second', 'minute', 'hour', 'day', 'week', 'month', 'year')
date <- as.POSIXct(Data1$start_date, origin="1970-01-01")
dates <- round_date(date, 'hour')
input_agg <- aggregate(Data1$cpurate ~ dates, FUN=sum)
# Invoke AnomalyDetectionTs
library(AnomalyDetection)
anoms = AnomalyDetectionTs(input_agg, direction='pos', alpha = 0.05, plot=T,  ylabel='value') 
anoms$plot




