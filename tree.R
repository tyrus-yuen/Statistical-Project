count <- read.xlsx("count.xlsx")
count<-as.matrix(count)
count<-as.data.frame(count)

trntst_dts_no <- read.csv("trntst_dts_no.csv")
trntst<-as.matrix(trntst_dts_no)[,2:ncol(trntst_dts_no)]

Y<-count[,1]
X<-count[,c(2:ncol(count))]

trainX<-X[-trntst[,1],]
trainY<-Y[-trntst[,1]]

testX<-X[trntst[,1],]
testY<-Y[trntst[,1]]

free <- rpart(count$FraudFound_P~., data=count, method='class')

plot(free);text(free)

prtr<-predict(free,testX)
table(prtr,testY)


