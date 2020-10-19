# Practical-Machine-Learning-Report
---
title: "Practical Machine Learning Project"
output:
  html_document: default
  pdf_document: default
---


### Project Statement Given:
##### The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

# Acquiring and Manipulating the Data:
### Loading Data Lib:
```{r, message=FALSE, warning=FALSE}
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)
```
### Acquiring Data:
```{r echo=TRUE}
data_to_train <- read.csv("pml-training.csv")
data_to_test <- read.csv("pml-testing.csv")
inTrain <- createDataPartition(data_to_train$classe, p=0.6, list=FALSE)
DeepTraining  <- data_to_train[inTrain, ]
BlueTesting <- data_to_train[-inTrain, ]
```
### Data wipe and Cleaning Procedure:
```{r echo=TRUE}
# remove variables with nearly zero variance
nzv <- nearZeroVar(myTraining)
DeepTraining  <- DeepTraining [, -nzv]
BlueTesting <- BlueTesting[, -nzv]

# remove variables that are almostly NA
mostlyNA <- sapply(DeepTraining , function(x) mean(is.na(x))) > 0.95
DeepTraining  <- DeepTraining [, mostlyNA==F]
BlueTesting <- BlueTesting[, mostlyNA==F]

# remove identification only variables (columns 1 to 5)
DeepTraining  <- DeepTraining [, -(1:5)]
BlueTesting  <- BlueTesting[, -(1:5)]
```
# Final Predictive analysis using multiple models:
### 1. Random forest
```{r echo=TRUE}
modularFitting <- randomForest(classe ~ ., data=DeepTraining )
modularFitting

# Prediction using Random forest
predict <- predict(modularFitting, BlueTesting, type="class")
confusionMatrix(BlueTesting$classe, predict)
```
### 2. Decision tree
```{r echo=TRUE}
modularFitting_T <- rpart(classe~., DeepTraining )

# Prediction using Decision tree
predict_T <- predict(modularFitting_T, BlueTesting, type="class")
confusionMatrix(BlueTesting$classe, predict_T)
```
### 3. Generalized Boosted Model or (GBM)
```{r, message=FALSE, warning=FALSE}
control_GBM <- trainControl(method = "repeatedcv", number=5, repeats=1)
modularFitting_GBM <- train(classe~.,DeepTraining , method="gbm", trControl=control_GBM, verbose=FALSE)
```
```{r echo=TRUE}
# Prediction using GBM
predict_GBM <- predict(modularFitting_GBM, BlueTesting)
confusionMatrix(predict_GBM, BlueTesting$classe)
```

# Comparison and Error Percentages:
#### Random forest-99.66% , Decision tree- 75.47% and GBM model-98.9% (accuracy).
#### The expected sample errors for Random forest-0.4 Decision tree-24.6 and GBM-1.2 percentages of sampling error.


# End Result Test:
#### Run the algorithm to the 20 test cases in the test data using most accurate model Random forest.
```{r echo=TRUE}
predictive_testing <- predict(modularFitting, data_to_test, type = "class")
predictive_testing
```
