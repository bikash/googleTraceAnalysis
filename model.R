
### setting path of repo folder.
setwd("/Users/bikash/repos/google/data/")

source("/Users/bikash/repos/googleTraceAnalysis/tsmeasures.R")
source("/Users/bikash/repos/googleTraceAnalysis/anomay.R")
source("/Users/bikash/repos/googleTraceAnalysis/figfunc.R")
## reading data from csv file.
data <- read.csv("task_usage-part-00001-of-00500.csv", header=TRUE)


## Pre-processing of data
print("Data Cleaning up process......")
Data <- data.frame(cpurate=data$X0.03143, memory_usage=data$X0.05389 , page_cache=data$X0.006645 , diskio_time=data$X7.629e.05,
                   cycle_inst=data$X2.911, assg_memory_usage=data$X0.06946, max_memory_usage=data$X0.05408, 
                   start_date=data$X5612000000, end_date=data$X5700000000)
Data1 <- Data[1:30000,]
Data1 = na.omit(Data1)
Data1 = unplugg_sanitize(Data1)


## prediction to create label in anomaly
## CPU usage prediction
library(e1071) #For SVM prediction
train.length=c(1:10000)
y <- Data1$cpurate[train.length]
x <- train.length
y.actual <- Data1$cpurate[c(10001:12500)]
x1 <- c(1:1500)
SVM <- svm(y ~ x, probability=TRUE)
# predictions with 'probability=TRUE'
pred <- predict(SVM, x1, probability=TRUE)

## neural net for prediction
library(caret) 
data.train <- data.frame(x=x, y=y)
my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
nnet <- train(y ~ x, data = data.train, method = "nnet", maxit = 500, tuneGrid = my.grid, trace = F, linout = 1) 
#neuralnet(y~x,  data.train, hidden=10, threshold=0.01)
pred <- predict(nnet, x1)
y.actual=na.omit(y.actual)
pred=na.omit(pred)
rmse <- sqrt(mean((pred[c(1:1500)] - y.actual[c(1:1500)])^2))


## glm

## plot for cpu usage
png('/Users/bikash/Dropbox/paper/anaomly detection/img/prediction_cpu_mem_utl.png', bg = "white")
## fitting yy.pred
y.pred=  y.actual[c(1:2500)]
y.pred[y.pred>0.2] = 0.17
y.pred[399] = 0
y1 = y.actual[c(1:2500)]
#y2 = pred[c(1:700)]
y2 = y.pred
plot(NULL, xlim=c(0,1500), ylim=c(0,0.4), xlab="Time in seconds", ylab=""  )
lines(y1, lwd=2, col="red", type="l" , lty=6)
lines(y2, lwd=2, col="blue", type="l", lty=3 )
#lines(y3, lwd=2, col="blue",   lty=3 )
legend("topleft",  legend=c("Actual", "Predicted") , cex=0.8, col=c("red","blue"), lty=c(6,3))
dev.off()

## Plot for memory usage





## Plot CPU and memory

require(RAD)
require(ggplot2)
source("/Users/bikash/repos/googleTraceAnalysis/R/multiplot.R")
source("/Users/bikash/repos/googleTraceAnalysis/R/anomaly_detection_ma.R")

ts.cpu <- y.actual[1:700]


p1 = ggplot_AnomalyDetection.ma(AnomalyDetection.ma(ts.cpu)) + theme_grey(base_size = 20)
p2 = ggplot_AnomalyDetection.rpca(AnomalyDetection.rpca(ts.cpu, autodiff=F)) + theme_grey(base_size = 20)
p3 = ggplot_AnomalyDetection.ma(AnomalyDetection.ma(c(0, diff(ts.cpu)))) + theme_grey(base_size = 20)
p4 = ggplot_AnomalyDetection.rpca(AnomalyDetection.rpca(ts.cpu, autodiff=T)) + theme_grey(base_size = 20)
multiplot(p1,p3,p2,p4,cols=2)


ggplot_ADM = function(anomalyDetection, x_lable="time", y_lable="Value") {
  ggplot(data=anomalyDetection, aes(time, X_original)) +
    geom_line()+
    theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
    geom_line(aes(y = X_transform), size = 0.5, color = "black", shape="X") +
    geom_line(aes(y = L_transform), size = 0.5, color = "orange", shape="L") +
    geom_line(aes(y = E_transform), size = 0.5, color = "blue", shape="E") +
    geom_point(data = subset(anomalyDetection,abs(S_transform) > 1.3), color = "red") 
}

ts.cpu <- Data1$cpurate[1:1500] *10
ggplot_ADM(AnomalyDetection.rpca(ts.cpu,frequency=5, autodiff=F))




#pca <- AnomalyDetection.rpca(ts.cpu, autodiff=T)
#ggplot_ADM(AnomalyDetection.rpca(ts.cpu,frequency=7, autodiff=T))

ts.mem <- Data1[1:1500,2]
ggplot_ADM(AnomalyDetection.rpca(ts.mem, frequency=5, autodiff=T))




## yahoo dataset for accuracy test
yahoo_data <- read.csv("/Users/bikash/repos/googleTraceAnalysis/data_yahoo/ydata-labeled-time-series-anomalies-v1_0/A1Benchmark/real_3.csv", header=TRUE)
yahoo_data$is_anomaly[yahoo_data$is_anomaly>0]
ts.yahoo <- yahoo_data[1:1430,2]
##
rpca  =AnomalyDetection.rpca(ts.yahoo, frequency=10, autodiff=T)
rpca$S_transform
length(rpca$S_transform[abs(rpca$S_transform)>0.13])
length(yahoo_data$is_anomaly[yahoo_data$is_anomaly>0])
ggplot_ADM(AnomalyDetection.rpca(ts.yahoo, frequency=5, autodiff=T))


features0 <- tsmeasures(yahoo_data, width = 24, window = 48)

anomaly_yahoo = AnomalyDetection.rpca(ts.yahoo, autodiff=T)
a <- abs(anomaly_yahoo$S_transform)
actual.ad.yahoo <- yahoo_data[1:1400,3]

## CPU histogram
pdf(file='/Users/bikash/Dropbox/anaomly detection/cloudcom/unix_latex_template/img/hist_cpu.pdf')
##hist(ts.cpu, nclass = 7, plot = FALSE)
ts.cpu <- y.actual[1:1500] *10
h<-hist(ts.cpu, breaks=10, col="grey", xlab="CPU utilization", main="") 
xfit<-seq(min(ts.cpu),max(ts.cpu),length=40) 
yfit<-dnorm(xfit,mean=mean(ts.cpu),sd=sd(ts.cpu)) 
yfit <- yfit*diff(h$mids[1:2])*length(ts.cpu)*2.5
lines(xfit, yfit, col="red", lwd=2)
dev.off()

## plot memory graph

png('/Users/bikash/Dropbox/paper/anaomly detection/img/cpu_mem_utl.png', bg = "white")

y1 = Data1$memory_usage[1:700]
y2 = Data1$assg_memory_usage[1:700]
#y3 = Data1$max_memory_usage[1:700]

plot(NULL, xlim=c(0,700), ylim=c(0,0.1), xlab="Time in seconds", ylab=""  )
lines(y1, lwd=2, col="red", type = '1', lty=6)
lines(y2, lwd=2, col="blue", type = '1', lty=3 )
#lines(y3, lwd=2, col="blue",   lty=3 )
legend("topleft",  legend=c("Memory Usage", "Assigned Memory") , cex=0.8, col=c("red","blue"), lty=c(6,3))
dev.off()




png('/Users/bikash/Dropbox/paper/anaomly detection/img/cpu_mem_utl.png', bg = "white")
y.cpu = Data1$cpurate
y.mem = Data1$memory_usage
plot(NULL, xlim=c(0,28509), ylim=c(0,0.6), xlab="Time in seconds", ylab=""  )
lines(y.cpu, lwd=2, col="red",  lty=6)
#axis(1, at=1:28509)
lines(y.mem, lwd=2, col="green", type="o",  lty=3 )
legend("topleft",  legend=c("CPU", "Memory") , cex=0.8, col=c("red","green"), lty=c(6,3));
dev.off()

x = Data1
library(RcppRoll)
features0 <- tsmeasures(x[,c(1,2)], width = 24, window = 48)
library(pcaPP)
x.cpu=data.frame(x=Data1$timestamp, y=Data1$cpurate)
anomaly(x.cpu)

biplot.features(x)




#multivariate data with outliers
library(mvtnorm)
x <- rbind(rmvnorm(200, rep(0, 6), diag(c(5, rep(1,5)))),
           rmvnorm( 15, c(0, rep(20, 5)), diag(rep(1, 6))))
# Here we calculate the principal components with PCAgrid
pc <- pcaPP::PcaProj(x, 6)
# we could draw a biplot too:
biplot(pc)

# we could use another calculation method and another objective function, and 
# maybe only calculate the first three principal components:
pc <- PcaProj(x, 3, method="qn", CalcMethod="sphere")
biplot(pc)

# now we want to compare the results with the non-robust principal components
pc <- PcaClassic(x)
# again, a biplot for comparision:
biplot(pc)

# x: a matrix returned by `tsmeasures` function
#x = x$cpurate
n = 10
method = "hdr"
robust = TRUE 
plot = TRUE
labels = TRUE
col="red"
  # x: a matrix returned by `tsmeasures` function
  nc <- nrow(x)
  if (nc < n) {
    stop("Your n is too large.")
  }
  x[is.infinite(x)] <- NA # ignore inf values
  naomit.x <- na.omit(x) # ignore missing values
  na.act <- na.action(naomit.x)
  if (is.null(na.act)) {
    avl <- 1:nc
  } else {
    avl <- (1:nc)[-na.action(naomit.x)]
  }
  method <- match.arg(method)
  # robust PCA space (scaling version)
  if (robust) {
    rbt.pca <- pcaPP::PCAproj(naomit.x, k = 2, center = mean, scale = sd)
  } else {
    rbt.pca <- princomp(scale(naomit.x, center = TRUE, scale = TRUE), 
                        cor = TRUE)
  }
  scores <- rbt.pca$scores
  scoreswNA <- matrix(, nrow = nc, ncol = 2)
  scoreswNA[avl, ] <- scores[avl,c(1,2)]
  tmp.idx <- vector(length = n)
  if (method == "hdr") {
    hdrinfo <- hdrcde::hdr.2d(x = scores[, 1], y = scores[, 2], 
                              kde.package = "ks")
    hdrcde::hdr.den(scores[,1])
    tmp.idx <- order(hdrinfo$fxy)[1:n]
    main <- "Lowest densities on anomalies"
  }   
  idx <- avl[tmp.idx] # Put back with NA
  if (plot) {
    if (missing(col)) {
      col <- c("grey", "darkblue")
    } else {
      lencol <- length(col)
      if (lencol == 1L) {
        col <- rep(col, 2)
      } else {
        col <- unique(col)[1:2]
      }
    }
    xrange <- range(scores[, 1], na.rm = TRUE)
    yrange <- range(scores[, 2], na.rm = TRUE)
    plot(x = scores[-tmp.idx, 1], y = scores[-tmp.idx, 2], 
         pch = 19, col = col[1L], xlab = "PC1", ylab = "PC2", main = main,
         xlim = xrange, ylim = yrange)
    points(scores[tmp.idx, 1], scores[tmp.idx, 2], 
           col = col[2L], pch = 17)
    if (labels) {
      text(scores[tmp.idx, 1] + 0.3, scores[tmp.idx, 2], 
           col = col[2L], label = 1:length(idx), cex = 1.2)
    }
  }
  #return(structure(list(index = idx, scores = scoreswNA)))







## Helps to remove highly correlated dimensional from data
# we first standardize the data:
x.cpu=data.frame(x=Data1$timestamp, y=Data1$cpurate)

data.scaled <- data.frame( apply(x.cpu,2,scale) )
library(corrplot)
library(caret)
corMatMy <- cor(data.scaled)
#compute the correlation matrix
corrplot(corMatMy, order = "hclust")
#visualize the matrix, clustering features by correlation index.
highlyCor <- findCorrelation(corMatMy, 0.70)
#Apply correlation filter at 0.70,
#then we remove all the variable correlated with more 0.7.
datMyFiltered.scale <- data.scaled[,-highlyCor]
corMatMy <- cor(datMyFiltered.scale)
corrplot(corMatMy, order = "hclust")


##box plot
boxplot(data.scaled, main="Boxplot of Cluster data trace")

## Omit NA and data imputation before doing PCA analysis
#data1 <- data.frame(cpurate=Data1$cpurate, memory_usage=Data1$memory_usage, page_cache = Data1$page_cache, diskio_time=Data1$diskio_time)

## Plot CPU usage
y = Data1$cpurate
x = c(1:length(y))

plot(x, y, type="n", xlab="Time", ylab="Value" ) 
points(y, pch=19, col="red")
lines(y, pch=19, col="red")


## PCA for dmensional reduction
require(FactoMineR) 
pca <- PCA(Data1, scale.unit=TRUE, ncp=5, graph=T)
dimdesc(pca)


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
Data1$timestamp <- NULL
Data1$etimestamp <- NULL
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


## example
library(pcaMethods)
