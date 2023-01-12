library(openxlsx)
count <- read.xlsx("2_count_standardize.xlsx") 

model_count<-glm(formula = FraudFound_P ~ ., family = binomial(link = "logit"), data = count)
model_count
summary(model_count)

#Results: 

#Extra: Feature selection #Take super long time
# load the library
library(mlbench)
library(caret)

newcount<-read.xlsx('3_regression selection.xlsx')
idpnt<-newcount[,c(2:ncol(newcount))] 
depnt<-newcount[,1]

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(idpnt,depnt, sizes=c(1:15), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

count1 <- read.xlsx("3_regression selection.xlsx") 
count2 <- read.xlsx("4_random forest selection.xlsx") 
ggpairs(count2)
ggpairs(count1)
ggpairs(count)