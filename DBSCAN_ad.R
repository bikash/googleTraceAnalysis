## DBSCAN approach
library(fpc)
set.seed(1234)


## yahoo dataset for accuracy test
yahoo_data <- read.csv("/Users/bikash/repos/googleTraceAnalysis/data_yahoo/ydata-labeled-time-series-anomalies-v1_0/A1Benchmark/real_3.csv", header=TRUE)
yahoo_data$is_anomaly[yahoo_data$is_anomaly>0]
ts.yahoo <- yahoo_data[1:1430,2]

par(bg="grey40")
ds <- dbscan(ts.yahoo, 0.2)
# run with showplot=1 to see how dbscan works.
ds
plot(ds, ts.yahoo)

x2 <- matrix(0,nrow=4,ncol=2)
x2[1,] <- c(5,2)
x2[2,] <- c(8,3)
x2[3,] <- c(4,4)
x2[4,] <- c(9,9)
predict(ds, ts.yahoo, x2)

n <- 600
x <- cbind((1:3)+rnorm(n, sd=0.2), (1:3)+rnorm(n, sd=0.2))



## twitter anomaly detection
library(AnomalyDetection)

ts <- data.frame(timestamp=yahoo_data$timestamp,value=yahoo_data$value)
res = AnomalyDetectionTs(ts, max_anoms=0.02, direction='both', plot=TRUE)
res$plot



## 
#####################################################################
# R code for the SVM (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( kernlab );

## trainSVM
# Train a SVM detector
##
# The SVM anomaly detector was described by Yu and Cho (2003), who
# listed Scholkopf et al (2001) as the primary source.  Trains a
# one-class support-vector machine on the training data.  Requires the
# kernlab library.  The SVM does have a nu parameter designed to
# enable one to choose the target false-alarm rate.  We aim for 5%.
##
# Y: a matrix wherein each row is a timing vector
##
# Returns an (opaque) trained detector object

trainSVM <- function( Y ) {
  
  svm <- ksvm( Y, type = 'one-svc', nu=.05 );
  
  obj <- list( type = 'SVM',
               svm = svm );
  
  return( obj );
}

## classifySVM
# Generate anomaly scores using a trained SVM detector
##
# Each test sample is compared to the separator learned by the SVM.
# That (signed) distance is flipped to calculate the anomaly score so
# that non-self scores are higher/positive and self scores are
# lower/negative.
##
# obj: a trained SVM detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifySVM <- function( obj, Y ) {
  
  thescores <- -predict( obj$svm, Y, type='decision' );
  
  res <- data.frame( score = thescores );
  
  return( res );
}

## plot for cpu usage
pdf('/Users/bikash/Dropbox/anaomly detection/img/spark5node.pdf')
x<-c(0.5, 1, 1.5, 2)
y4<-c(690, 790, 840, 890)
plot (x, y4, pch=17, cex=1.5, col="black", xlab="Data set size, records in millions", ylab="Time taken in seconds")
lines (x, y4, col="black")
dev.off()



Reasonstats <- read.table(text="Algorithm      Datatype   accuracy
                                   DBSCAN       Yahoo      81.1
                                   SVM          Yahoo      74.42
                                   IPCA         Yahoo      76.41
                                   RSPCA        Yahoo      82.4
                                   DBSCAN       KDD     72.64
                                   SVM          KDD      84.48
                                   IPCA         KDD      82.41
                                   RSPCA        KDD      90.71
                                   DBSCAN       ECG      74.19
                                   SVM          ECG      61.48
                                   IPCA         ECG      71.57
                                   RSPCA        ECG      68.67", header=T)

ReasonstatsDBSCAN <- Reasonstats[which(Reasonstats$Algorithm=="DBSCAN"),]
ReasonstatsSVM <- Reasonstats[which(Reasonstats$Algorithm=="SVM"),]
ReasonstatsIPCA <- Reasonstats[which(Reasonstats$Algorithm=="IPCA"),]
ReasonstatsRSPCA <- Reasonstats[which(Reasonstats$Algorithm=="RSPCA"),]

Reasonstats3   <- cbind(ReasonstatsDBSCAN[,3], ReasonstatsSVM[,3],ReasonstatsIPCA[,3],ReasonstatsRSPCA[,3])
colnames(Reasonstats3) <- c("DBSCAN", "SVM", "Inc. PCA","RSPCA")
rownames(Reasonstats3) <- ReasonstatsDBSCAN$Datatype

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(t(Reasonstats3), col=c("lightblue", "mistyrose", "lavender","cornsilk"), width=2, beside=TRUE,cex.axis=0.75, cex.names=0.75)
legend("topright",inset=c(0,0), fill=c("lightblue", "mistyrose", "lavender","cornsilk"), legend=colnames(Reasonstats3),text.font=2,cex=0.64)

