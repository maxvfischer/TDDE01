################# Assignment 2 #################
## The data file creditscoring.xls contains data retrieved from a database in 
## a private enterprise. Each row contains information about one customer. 
## The variable good/bad indicates how the customers have managed their loans. 
## The other features are potential predictors. Your task is to derive a prediction 
## model that can be used to predict whether or not a new customer is likely to pay 
## back the loan.

library(tree)
library(e1071)
library(MASS)

## Task 1 ##
## Import the data to R and divide into training/validation/test as 50/25/25
csvfile <- read.csv('creditscoring.csv', dec=',')

spec <- c(train = .5, validation = .25, test = .25)
size_csvfile <- dim(csvfile)[1]

set.seed(12345)
g <- sample(cut( seq( nrow(csvfile) ), nrow(csvfile)*cumsum(c(0,spec)), labels = names(spec) ))
res <- split(csvfile, g)

## Task 2 ##
## Fit a decision tree to the training data by using the following measures of impurity
## a. Deviance
## b. Gini index
## and report the misclassification rates for the training and test data. Choose
## the measure providing the better results for the following steps.

# Fitting the models for both "Deviance" and "Gini"
tree_fit_dev <- tree(good_bad ~ ., data = res$train, split = "deviance")
tree_fit_gini <- tree(good_bad ~ ., data = res$train, split = "gini")

# Predicting new data for "Deviance" and "Gini" by using both training and testing dataset
set.seed(12345)
train_dev_pred <- predict(tree_fit_dev, newdata = res$train, type="class")
train_gini_pred <- predict(tree_fit_gini, newdata = res$train, type="class")
test_dev_pred <- predict(tree_fit_dev, newdata = res$test, type="class")
test_gini_pred <- predict(tree_fit_gini, newdata = res$test, type="class")

# Get the summary to find the misclassification error rate for Deviance/Gini training data
summary(tree_fit_dev)
summary(tree_fit_gini)

# Create Confusion matrices
conf_matrix_test_dev <- table(test_dev_pred, res$test$good_bad)
conf_matrix_test_gini <- table(test_gini_pred, res$test$good_bad)

# Calculate misclassification error rate for Deviance/Gini testing data
misclass_test_deviance <- 1-sum((diag(conf_matrix_test_dev)/sum(conf_matrix_test_dev)))
misclass_test_gini <- 1-sum((diag(conf_matrix_test_gini)/sum(conf_matrix_test_gini)))

## Task 3 ##
## Use training and validation sets to choose the optimal tree depth. Present the
## graphs of the dependence of deviances for the training and the validation
## data on the number of leaves. Report the optimal tree, report it’s depth and
## the variables used by the tree. Interpret the information provided by the tree
## structure. Estimate the misclassification rate for the test data

# Deviance chosen due to lower misclassification rate on testing set
train_score <- rep(0,9)
validation_score <- rep(0,9)

for (i in 2:9) {
  prunedTree <- prune.tree(tree_fit_dev, best=i) # Tree is pruned with i leaves/terminal nodes
  set.seed(12345)
  prediction <- predict(prunedTree, newdata = res$validation, type = "tree") # Predict with the pruned tree and val set
  train_score[i] <- deviance(prunedTree) # Calculate deviance of test set
  validation_score[i] <- deviance(prediction) # Calculate deviance of val set
}

# Plot graph of Score/Leaves for training and validation
plot(2:9, train_score[2:9], type="b", col="red", ylim=c(100,800), ylab="Deviance", xlab="No. of leaves")
points(2:9, validation_score[2:9], type="b", col="blue")
legend("top", legend=c("Training score (green)", "Validation score (blue)"))

# Optimal tree, No. of leaves = 4
optPrunedTree <- prune.tree(tree_fit_dev, best=4)
plot(optPrunedTree, sub="asd")
text(optPrunedTree, pretty=0)
title("Pruned tree with 4 leaves")

# Estimate misclassification rate for test data
testpredict <- predict(optPrunedTree, newdata = res$test, type = "class")
testConfusionMatrix <- table(testpredict, res$test$good_bad)
testMisClass <- 1-sum(diag(testConfusionMatrix)/sum(testConfusionMatrix))

## Task 4 ##
## Use training data to perform classification using Naïve Bayes and report the
## confusion matrices and misclassification rates for the training and for the
## test data. Compare the results with those from step 3.

# Train Naive Bayes on train set
naiveFit <- naiveBayes(good_bad~., data=res$train)
print(naiveFit)

# Predict with train and test set
set.seed(12345)
naivePredTrain <- predict(naiveFit, newdata = res$train)
set.seed(12345)
naivePredTest <- predict(naiveFit, newdata = res$test)

# Confusion matrix of Naive Bayes Train/Test set
naiveTrainConfMatrix <- table(naivePredTrain, res$train$good_bad)
naiveTestConfMatrix <- table(naivePredTest, res$test$good_bad)

# Misclassification rate Naive Bayes Train/Test set
naiveTrainMisClass <- 1-sum(diag(naiveTrainConfMatrix)/sum(naiveTrainConfMatrix))
naiveTestMisClass <- 1-sum(diag(naiveTestConfMatrix)/sum(naiveTestConfMatrix))

## Task 5 ##
## Repeat Naïve Bayes classification but use the following loss matrix:
## L = 
## good (  0  1 )
## bad  ( 10  0 )
##
## and report the confusion matrix for the training and test data. Compare the
## results with the results from step 4 and discuss how the rates has changed
## and why. 

# Predict the Naive Bayes with train/test set
naiveTrainLoss <- predict(naiveFit, newdata = res$train, type = "raw")
naiveTestLoss <- predict(naiveFit, newdata = res$test, type = "raw")

# Calculate confusion matrices with loss
naiveTrainLossConfMat <- table(res$train$good_bad, naiveTrainLoss[,2]/naiveTrainLoss[,1] > 10)
naiveTestLossConfMat <- table(res$test$good_bad, naiveTestLoss[,2]/naiveTestLoss[,1] > 10)

# Calculate misclassification rates for Naive Bayes classifier with loss matrix
naiveTrainLossMisClass <- 1-sum(diag(naiveTrainLossConfMat)/sum(naiveTrainLossConfMat))
naiveTestLossMisClass <- 1-sum(diag(naiveTestLossConfMat)/sum(naiveTestLossConfMat))