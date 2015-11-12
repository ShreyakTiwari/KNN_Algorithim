rm(list=ls())

setwd("/rmt/csfiles/pgrads/mbvb820/Downloads/KLearning")
source("tangent.R")

kNearestAlgorithim <- function(data,k,factor){
  
  # dividing training and test data points in 3:1 ratio
  train = data[1:(0.75*nrow(data)),]
  test  = data[-c(1:(0.75*nrow(data))),]  
  
  train_set = train;
  train1_set= train;
  train2_set= train;
  
  test1_set      = test;
  prediction_set = test;
  test3_set      = test;
  
  test1_set[,ncol(data)]<-NULL
  prediction_set[,ncol(data)]<-0
  
  train_set[,ncol(data)]<-NULL
  train1_set$distance<-0
  
  for(j in 1:nrow(test1_set)){
    for(i in 1:nrow(train_set)){
      
      # find distance between two vector points and put that value in new column
      if(factor=='euclidean'){
        train1_set[i,]$distance <- sqrt(sum((test1_set[j,]-train_set[i,])^2))
      }
      else if(factor=='tangent'){
        train1_set[i,]$distance <- distance(test1_set[j,],train_set[i,])
      }
    }
    
    #order that training set to find the lowest distance
    newOrder <- train1_set[order(train1_set$distance),]
    
    # count the frequency of labels and take maximum value and assign it to test vector
    label=as.numeric(names(sort(summary(as.factor(c(newOrder[1:k,ncol(newOrder)-1]))))[1]))
    prediction_set[j,ncol(prediction_set)]<-label
  }
  #comparing labels of test set and predicted set
  v=test3_set[,ncol(test3_set)]-prediction_set[,ncol(prediction_set)];
  #calculating fraction of correct labels prediction
  a=(sum(v==0)/nrow(test3_set));
  return(c(a))
}

############################# IRIS and IONOSPHERE DATA ########################################################

iris<-read.csv("/rmt/csfiles/pgrads/mbvb820/Downloads/KLearning/iris.txt", header=FALSE)
PercentageOfCorrectLabels = kNearestAlgorithim(iris,1,'euclidean')
PercentageOfCorrectLabels = kNearestAlgorithim(iris,3,'euclidean')


ionosphere<-read.csv("/rmt/csfiles/pgrads/mbvb820/Downloads/KLearning/ionosphere.txt", header=FALSE)
PercentageOfCorrectLabels = kNearestAlgorithim(ionosphere,1,'euclidean')
PercentageOfCorrectLabels = kNearestAlgorithim(ionosphere,3,'euclidean')


######################################################### USPSsubset Data ##############################################################################

USPS<-read.table("USPSsubset.txt", quote="\"")

PercentageOfCorrectLabels = kNearestAlgorithim(USPS,1,'tangent')
PercentageOfCorrectLabels = kNearestAlgorithim(USPS,1,'euclidean')
PercentageOfCorrectLabels = kNearestAlgorithim(USPS,3,'tangent')
PercentageOfCorrectLabels = kNearestAlgorithim(USPS,3,'euclidean')

Accuracy<-0.0
for(k in 1:5){
  Accuracy[k]<-kNearestAlgorithim(USPS,k,'tangent');
}
plot(1:5,Accuracy, type = "b", xlab = "k", ylab = "Accuracy", main = "KNN (Tangent Distance) on USPSsubset Data")

Accuracy<-0.0
for(k in 1:5){
  Accuracy[k]<-kNearestAlgorithim(USPS,k,'euclidean');
}
plot(1:5,Accuracy, type = "b", xlab = "k", ylab = "Accuracy", main = "KNN (Euclidean Distance) on USPSsubset Data")
