
library(caret)
library(corrplot)

# read the trainig and test sets
setwd("/Users/somtirtha/workspace/Programs/Coursera/DataScience/PracticalMachineLearning/Week4/predictingExercises/")
train_data = read.csv("data/pml-training.csv")
test_data = read.csv("data/pml-testing.csv")

# names(train_data)
# clean data
train_data = train_data[ , colSums(is.na(train_data)) == 0]
test_data = test_data[ , colSums(is.na(test_data)) == 0]

classe = train_data$classe
train_exclude = grepl("^X|timestamp|window", names(train_data))
train_data = train_data[, !train_exclude]
length(sapply(train_data, is.numeric))
train_data = train_data[, sapply(train_data, is.numeric)]
train_data$classe = classe

test_exclude = grepl("^X|timestamp|window", names(test_data))
test_data = test_data[, !test_exclude]
if( length(sapply(test_data, is.numeric)) ) {
  test_data = test_data[, sapply(test_data, is.numeric)]
}

# belt, forearm, arm, and dumbell
# selection of features
features = names(train_data)

# split the training data into training and validation sets
set.seed(22519) # For reproducibile purpose
inTrain = createDataPartition(train_data$classe, p=0.70, list=F)
training_data = train_data[inTrain, ]
validation_data = train_data[-inTrain, ]

# training a classifier
# do cross validation
rf_model = train(classe ~ ., method="rf", trControl=trainControl(method="cv", 5), ntree=250, data=training_data)
rf_model$finalModel

# predict data on test set
validation_pred = predict(rf_model, validation_data)

# make confusion matrix of the results
confusionMatrix(validation_data$classe, validation_pred)
accuracy = postResample(validation_pred, validation_data$classe)
accuracy

# Test on the test data set
test_pred = predict(rf_model, test_data)
test_pred

corrPlot <- cor(train_data[, -length(names(train_data))])
corrplot(corrPlot, method="color")



