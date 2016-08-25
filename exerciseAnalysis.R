#exerciseAnalysis.R

#cleanup the environment to support performance
rm(list = ls())

#load the libraries that may be needed
library(AppliedPredictiveModeling)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)

#load the data
setwd('c:/users/dedward2/desktop/github/mlearn')
fPath <- getwd()
trainPath <- paste(fPath,'pml-training.csv',sep='/')
testPath <-  paste(fPath,'pml-testing.csv',sep='/')


dfTest  <- read.csv(testPath,na.strings=c("NA",""),header = TRUE)
dfTrain <- read.csv(trainPath,na.strings=c("NA",""),header = TRUE)

#Prepare the data
#1. Remove NAs
#2. Remove columns that are not relevant to analysis- also remove id and user name

cols <- names(dfTest[,colSums(is.na(dfTest))== 0])[8:59]
dfTrain<- dfTrain[,c(cols,"classe")]
dfTest <- dfTest[,c(cols,"problem_id")]

#Test for near zero values
nzv <- nearZeroVar(dfTrain)
print(nzv)

#Further partition training set to create an extra training set
dfTrain1 <- createDataPartition(dfTrain$classe, p = 0.7, list = F)
training = dfTrain[dfTrain1,]
testing =  dfTrain[-dfTrain1,]

#Fit a model using random forest with cross validation
fitParams <- trainControl(method='cv',number=4)
set.seed(1234)
modelFit <- train(classe ~ .,data=training,method='rf',trControl=fitParams)
print(modelFit$finalModel)

#Use the 30% partition of the training model to predict fit
preds <- predict(modelFit, newdata=testing)
# show confusion matrix to get estimate of out-of-sample error
matrix <- confusionMatrix(testing$classe, preds)
print(matrix)


#Use the actual test set to predict outcomes using the 20 item test set
preds <- predict(modelFit, newdata=dfTest)
print(preds)

