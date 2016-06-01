library(caret)
library(ggplot2)
library(tree)
library(randomForest)

## set seed for reproducibility when testing
set.seed(333)

## download data and ensure that all NAs are picked up
data = read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!"))
testing = read.csv("pml-testing.csv",  na.strings=c("NA", "#DIV/0!"))

## remove junk variables (empty or lots of NA)
training <- data[ , colSums(is.na(data)) == 0]
training <- training[,-c(1:6)]

## check near zero var to see if PCR is likely to yield results - it doesn't.
nzv <- nearZeroVar(training, saveMetrics = TRUE)

## fit random forest model using cross validation to boost results.
rffit<-train(classe~.,data=training, method="rf", 
             trControl= trainControl(method="cv", number=5, allowParallel=T, verbose=T), 
             verbose=F, allowParallel=T)


## predict and check confusion matrix for results
pred <- predict(rffit,training)
confusionMatrix(pred,training$classe)

## 
predtst <- predict(rffit,testing)
confusionMatrix(pred,testing$classe)