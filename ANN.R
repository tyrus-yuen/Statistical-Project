setwd("~/Google Drive/Sync/Year 4 Sem 1/STAT 4011/Project 1")
library(tidyverse)
library(neuralnet)
library(GGally)
library(class)
library(caret)# loading the data
library(openxlsx)
library(nnet)
library(kernlab)
set.seed(10)
fdt<-read.xlsx('3_regression selection.xlsx')
#ggpairs(fdt,title='Scatterplot Matrix of the features of the dataset')
trainntest<-read.csv('draws.csv')
trainntest<-as.matrix(trainntest[2:nrow(trainntest),1:ncol(trainntest)])

conmat<-matrix(c(0,0,0,0),nrow=2)
  for(i in 1:10){
rbf <- rbfdot(sigma=0.1)
irisSVM <- ksvm(FraudFound_P~.,data=fdt[-trainntest[,i],],type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)
fitted(irisSVM)
pdt_svm1<-predict(irisSVM, fdt[trainntest[,i],2:ncol(fdt)], type="response")
conmat<-conmat+table(pdt_svm1,fdt[trainntest[,i],1])
}
table(pdt_svm1,fdt[trainntest[,1],1])

#nnet
train<-fdt[trainntest[,1],]
fdt_NN1 <- nnet(fdt[,2:ncol(fdt)],fdt[,1], size=10)
summary(fdt_NN1)
pred<-round(fdt_NN1$fit)
table(pred,fdt[,1])
pdt_NN1<-predict(fdt_NN1, fdt[trainntest[,1],2:ncol(fdt)], type="raw")
table(pdt_NN1,fdt[trainntest[,1],1])

#neuralnet
fdt_NeurN1<- neuralnet(formula = FraudFound_P	~  Date+WeekNumber+Dateclaimed+Month+PolicyType+VehicleCategory+BasePolicy,fdt[-trainntest[,1],], linear.output = FALSE,err.fct='ce',likelihood=TRUE)

fdt_NeurN1_trainerror<-fdt_NeurN1$result.matrix[1,1] 
paste('CE Error: ',round(fdt_NeurN1_trainerror,3)) #CE Error

fdt_NeurN1_AIC<-fdt_NeurN1$result.matrix[4,1]
paste('AIC: ',round(fdt_NeurN1_AIC,3))

fdt_NeurN1_BIC<-fdt_NeurN1$result.matrix[5,1]
paste('BIC: ',round(fdt_NeurN1_BIC,3))

main=glm(FraudFound_P~ Date+WeekNumber+Dateclaimed+Deductible+Year+Month+WeekOfMonth+DayOfWeek+AccidentArea+Sex+Fault+PolicyType+VehicleCategory+VehiclePrice+Days_Policy_Accident+PoliceReportFiled+AddressChange_Claim+BasePolicy,fdt[trainntest[,1],],family=binomial())
prediction(fdt_NeurN1,list.glm = list(main=main))

compute(fdt_NeurN1,fdt[trainntest[,1],2:ncol(fdt)])