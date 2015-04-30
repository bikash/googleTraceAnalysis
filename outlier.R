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

##
# Conduct a preliminary analysis to see means and variances of each variable:
apply ( data1 , 2 , mean )
apply ( data1 , 2 , var )

# Prcomp calculated with normalized data, with mean 0 and variance 1
pr.out  = prcomp ( data1 , scale = TRUE )   # with scale = True we get all the data have the same scale

# We make a biplot for the first 2 components and their values
biplot ( pr.out , scale  =  0 )

summary(pr.out)
#Information from each compenentes that gives me pr.out We will get:
  names ( pr.out ) # get each component.
pr.out $ SDEV  # I get the standard deviation
pr.out $ rotation  P # I get the base change matrix
pr.out $ x  # I get the array data and after the change of base

# To identify key addresses that I will stay, I can use two methods including:
# Criteria of the "elbow" which will look like the slope decreases in func of the main components that have
# Calculating the proportion of variance explained (PVE)

# First we calculate the PVE:
## Calculation of the variance.
pr.var  <-  pr.out $ SDEV ^ 2

## Calculation of proportion PVE 
pve  <-  pr.var / sum ( pr.var )
pve

cumSum ( PVE ) *  100  # see the cumulative percentage of each major component.

## You can see that with the election of the first two components 64.4?? explains the variation in the data and 
## If I select the first 3 components, increased to 76.8%

# Represent the opinion of the elbow:
plot ( pve ,   xlab = " Major Component " , ylab = " Proportion of Variance Explained " , type  =  ' b ' )
plot (cumSum ( PVE ), xlab = " Major Component " , ylab = " Proportion of Variance Explained Acumulatica " , ylim = c ( 0 , 1 ), type = ' b ' )


x = data.frame(data1$cpurate,data1$cmemory_usage)
xncol <- ncol(x)
#Now we find the scaling values, which are basically the column ranges:
#First we create an empty vector:
#range.vector=c()
#Find the range values and place in a vector:
min.col<-NULL
max.col<-NULL
for (j in 1:xncol){
  min.col [j] <- min(x [,j])
  max.col [j] <- max(x [,j])    
}
rangediff <- max.col - min.col
y <- scale(x, center = TRUE, scale=rangediff)
ysvd <- svd(y)

#Calculate squared of the singular value...
sqr.ysvd <- ysvd$d^2
#...and its %
total.sqr.ysvd <- sum(sqr.ysvd)
percent.ysvd <- (sqr.ysvd/total.sqr.ysvd)*100

#Plot the vector coordinates
plot <- plot(ysvd$v)

ds = sum(y * y) #Data scatter
mu1 <- round(ysvd$d[1] ^2) #Mu
contr <- (mu1 / ds)*100 #Contribution of first component
answer <- list(ysvd$d, sqr.ysvd, percent.ysvd, ysvd$u, ysvd$v, ds, mu1, contr, plot)



# Proceed with principal components
fit <- princomp(data1, cor = TRUE)
summary(fit)
loadings(fit)
pdf(file = "graph/PCA_lineplot.pdf")
plot(fit, type='lines')
dev.off()
fit$scores
pdf(file = "graph/PCA_Biplot.pdf")
biplot(fit)
dev.off()

variances <- data.frame(variances=fit$sdev**2, pcomp=1:length(fit$sdev))
varPlot <- ggplot(variances, aes(pcomp, variances)) + geom_bar(stat="identity", fill="gray") + geom_line()
pcaPlot <- ggbiplot(fit,ellipse=F,circle=F,varname.size=5, varname.adjust = 1.5)


heatmap <- qplot(x=Var1, y=Var2, data=melt(cor(data)), geom="tile", fill=value)

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

