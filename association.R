count<-read.xlsx('count.xlsx')
dummy<-read.xlsx('dummy.xlsx')
trainntest<-read.csv('trntst_dts_no.csv')
trainntest<-trainntest[,2:ncol(trainntest)]

responseY <- as.matrix(count[,1])
predictorX <- as.matrix(count[,c(2:ncol(count))])

library(GoodmanKruskal)
library(caret)
library(mlbench)
gmk_count<-GoodmanKruskal::GKtauDataframe(count[,10:ncol(count)])
gmk_dummy<-GoodmanKruskal::GKtauDataframe(dummy[,9:ncol(dummy)])
write.csv(gmk_count,file='gmk_count.csv')
write.csv(gmk_dummy,file='gmk_dummy.csv')

control<-rfeControl(functions = rfFuncs,method='cv',number=10)

result_count<-rfe(count[,2:ncol(count)],count[,1],size=(c(1:10)),rfeControl = control)

result_dummy<-rfe(dummy[,2:ncol(dummy)],dummy[,1],size=(c(1:10)),rfeControl = control)