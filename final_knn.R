library(class)
library(caret)# loading the data
library(openxlsx)
set.seed(10)
fdt<-read.xlsx('2_count_standardize.xlsx')
trainntest<-read.csv('trntst_dts_no.csv')
trainntest<-as.matrix(trainntest[2:ncol(trainntest)])

# creating matrices for Xs and Y
responseY <- as.matrix(fdt[,1])
X <- as.matrix(fdt[,c(2:ncol(fdt))])



# data partition for train/test sets. # data partition for 1 train/test sets. Test k=1:30
trainIndex <- createDataPartition(responseY, times=1, p = 0.8, list = F)

#Result

# fitting models for 30 different k-values (one for test and one for train set for each K)
train.error = rep(0,30)
test.error = rep(0,30)
for(k in 1:30){
  model.knn.train <- knn(train=X[trainIndex,], test=X[trainIndex,], cl=responseY[trainIndex], k=k, prob=F)
  train.error[k] <- sum(model.knn.train!=responseY[trainIndex])/length(responseY[trainIndex])
  model.knn.test <- knn(train=X[trainIndex,], test=X[-trainIndex,], cl=responseY[trainIndex], k=k, prob=F)
  test.error[k] <- sum(model.knn.test!=responseY[-trainIndex])/length(responseY[-trainIndex])
}

# PLOTTING:
plot(1:30, train.error, col='red', type = 'b', ylim = c(0,0.65))
points(1:30, test.error, col='blue', type = 'b')
title("Error for k=1:30, 1 trial", sub = "Red= Train; Blue= Test",
      cex.main = 2,   font.main= 4, col.main= "blue",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")
#Result: 1

# data partition for 10 train/test sets. Test k=1:30
train.error = c()
test.error = c()
for(k in 1:30){
  test.error.tmp = c()
  train.error.tmp = c()
  for(i in 1:10){
    pred <- knn(train = X[-trainntest[,i],],test = X[trainntest[,i],], cl = responseY[-trainntest[,i],], k=k)
    test.error.tmp = c(test.error.tmp,mean(responseY[trainntest[,i],] != pred))
    pred <- knn(train = X[-trainntest[,i],],test = X[-trainntest[,i],], cl = responseY[-trainntest[,i],], k=k)
    train.error.tmp = c(train.error.tmp,mean(responseY[trainntest[,i],] != pred))
  }
  test.error = rbind(test.error,test.error.tmp)
  train.error = rbind(train.error,train.error.tmp)
}
plot(1:30, rowMeans(train.error), col='blue', type='b', ylim = c(0,0.65))
points(1:30, rowMeans(test.error), col='red', type='b')
title("Error for k=1:30, 10 trial", sub = "Red= Train; Blue= Test",
      cex.main = 2,   font.main= 4, col.main= "blue",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")

# data partition for 10 train/test sets. Show the confusion matrix where k=3



train.error = c()
test.error = c()
conmat<-matrix(c(0,0,0,0),nrow=2)
for(i in 1:10){
  pred <- knn(train = X[-trainntest[,i],],test = X[trainntest[,i],], cl = responseY[-trainntest[,i],], k=5)
  test.error.tmp = c(test.error.tmp,mean(responseY[trainntest[,i],] != pred))
  conmat<-as.matrix(table(pred, responseY[trainntest[,i]],dnn=c('pred','actual')))+conmat
  pred <- knn(train = X[-trainntest[,i],],test = X[-trainntest[,i],], cl = responseY[-trainntest[,i],], k=5)
  train.error.tmp = c(train.error.tmp,mean(responseY[-trainntest[[i]],] != pred))
}
conmat

# k=1:30 result

train.error = c()
test.error = c()
conmat<-matrix(c(0,0,0,0),nrow=2)
for (k in 1:10){
  conmat<-matrix(c(0,0,0,0),nrow=2)
  for(i in 1:10){
    pred <- knn(train = X[-trainntest[,i],],test = X[trainntest[,i],], cl = responseY[-trainntest[,i],], k=k)
    test.error.tmp = c(test.error.tmp,mean(responseY[trainntest[,i],] != pred))
    conmat<-t(as.matrix(table(responseY[trainntest[,i]],pred,dnn=c('predict',paste('actual',k)))))+conmat
    sensitivity<-conmat[2,2]/(conmat[2,2]+conmat[2,1])
    accuracy<-(conmat[2,2]+conmat[1,1])/(conmat[2,1]+conmat[2,2]+conmat[1,1]+conmat[1,2])
    precision<-conmat[2,2]/(conmat[2,2]+conmat[1,2])
    specificity<-conmat[1,1]/(conmat[1,1]+conmat[1,2])
    ppv<-conmat[2,2]/(conmat[2,2]+conmat[1,2])
    npv<-conmat[1,1]/(conmat[1,1]+conmat[2,1])
    performance1<-t(as.matrix(c(accuracy,precision,sensitivity,specificity,ppv,npv)))
  }
  print(conmat)
  colnames(performance1)<-c('Accuracy','Precision','Sensitivity','Specificity','Positive Predictive Value','Negative Predictive Value')
  print(performance1)
}
