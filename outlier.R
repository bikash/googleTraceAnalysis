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
library(DMwR)

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/google/data/")

## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(task_index=data$X369, cpurate=data$X0.03143, cmemory_usage=data$X0.05389, ass_memory_usage=data$X0.06946
                   ,unmapped_page_cache=data$X0.005997,page_cache=data$X0.006645, max_mem_usage=data$X0.05408,
                   diskio_time=data$X7.629e.05 , local_disk_space=data$X0.0003834, max_cpu_rate=data$X0.2415,
                   max_disk_io=data$X0.002571, cycle_inst=data$X2.911, memory_per_inst=data$X)

Data1 <- Data[1:10000,]

## Omit NA and data imputation before doing PCA analysis
data1 <- data.frame(task_index=Data1$task_index, cpurate=Data1$cpurate, cmemory_usage=Data1$cmemory_usage, ass_memory_usage=Data1$ass_memory_usage,
                    unmapped_page_cache=Data1$unmapped_page_cache, page_cache = Data1$page_cache, max_mem_usage=Data1$max_mem_usage,
                    diskio_time=Data1$diskio_time, local_disk_space=Data1$local_disk_space, max_cpu_rate=Data1$max_cpu_rate,
                    max_disk_io=Data1$max_disk_io, cycle_inst=Data1$cycle_inst, memory_per_inst=Data1$memory_per_inst)

data1 = na.omit(data1)
# Proceed with principal components
pca.1 <- princomp(data1)
unclass(loadings(pca.1))  # component loadings
pca.1$sd^2  # component variances
summary(pca.1) # proportions of variance
plot(pca.1) # if you would like a scree plot


heatmap <- qplot(x=Var1, y=Var2, data=melt(cor(data)), geom="tile",
                 fill=value)

outlier.score <-lofactor(data1, k=5)
plot(density(outlier.score))

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)

## visualizing outlier in plots

n <- nrow(data1)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(data1), cex=.8, xlabs=labels)

