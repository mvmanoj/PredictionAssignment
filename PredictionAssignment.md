## This is a markdown File ##

---
title: "Prediction Assignment"
author: "Manoj Krishna"
date: "3 March 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

### Objective

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. The participants were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Goal

The goal of this project is to predict the manner in which participants did the exercise. This report describes how the weight lifting data was analysed and the prediction model generated. The prediction model was used successfully to accurately predict all 20 different test cases on the Coursera website.

## Cllecting and Loading the Data

The training data for this project are available at: (https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)

The test data are available at: (https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)



###Loading libraries

```{r}
library(ggplot2)
library(caret)
library(randomForest)
library(splines)
library(gbm)
library(plyr)
library(MASS)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(RGtk2)
library(rattle)
library(e1071)
```

###Loading Data

```{r cache=TRUE}
training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE)
dim(training)
testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE)
dim(testing)

```



## Data Preparation

### Initial Analysis of the Raw Data

There are 160 variables, which is a lot and we need to remove the variables which won't be used in this project.

Looking at the data in the **training** dataset, you can see there are a large number of NA values.  So the next stage is to check for near zero-variance predictors and remove those variables from the model.

### Data Cleansing

```{r}
features <- names(testing[,colSums(is.na(testing)) == 0])[8:59]

# Only use features used in testing cases.
training <- training[,c(features,"classe")]
testing <- testing[,c(features,"problem_id")]

dim(training); dim(testing);

set.seed(12345)

inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)
f_training <- training[inTrain,]
f_testing <- training[-inTrain,]

dim(f_training); dim(f_testing);
```

#Building Decision Tree Model for Prediction

```{r}
modFitDT <- rpart(classe ~ ., data = f_training, method="class")
fancyRpartPlot(modFitDT)
```
#Traning three different types of models 


##Predicting with the Decision Tree Model

```{r cache=TRUE}
set.seed(12345)

prediction <- predict(modFitDT, f_testing, type = "class")
confusionMatrix(prediction, f_testing$classe)
```


## Builing Random Forest Model


We will now test the accuracy of the training data with the training models. 

```{r}
set.seed(12345)
modFitRF <- randomForest(classe ~ ., data = f_training, ntree = 1000)
```

## Prediting with Random Forest Model

```{r}
prediction <- predict(modFitRF, f_testing, type = "class")
confusionMatrix(prediction, f_testing$classe)
```


#Predicting on the Testing Data

###Decision Tree Prediction
```{r PredTree}
predictionDT <- predict(modFitDT, testing, type = "class")
predictionDT
```


##Random Forest Prediction
```{r RFPred}
predictionRF <- predict(modFitRF, testing, type = "class")
predictionRF
```
# Preparing Submission file using test data
AAccuracy of the Random Forest Model is about 99%. Because of that we could expect nearly all of the submitted test cases to be accurate.


```{r submission}
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionRF)
```

#Conclusion
The model and test indicates that the random forest model is the best fir model. Random Forest model can eb used in this case to predict the outcome values for the test data set that was provided.

