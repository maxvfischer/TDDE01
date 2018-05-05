#################### Assignment 1 ####################
## The data file spambase.xlsx contains information about the frequency of various words, 
## characters etc for a total of 2740 e-mails. Furthermore, these e-mails have been 
## manually classified as spams (spam = 1) or regular e-mails (spam = 0). Your task is 
## to develop a K-nearest neighbor model that can be used as a spam filter.

library(kknn)

knearest=function(data,K,newdata) {
  
  n1=dim(data)[1] # Number of data points in train data
  n2=dim(newdata)[1] # Number of data points in test data
  p=dim(data)[2] # Numbers of features/variables in each data point
  Prob=numeric(n2) # Make a vector with the same dimension as number of test data points
  X=as.matrix(data[,-p]) # Create new matrix from data, without last column
  Y=as.matrix(newdata[,-p]) # Create new matrix from newdata, without last column
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Y=Y/matrix(sqrt(rowSums(Y^2)), nrow=n2, ncol=p-1)
  
  C = X%*%t(Y)
  M = matrix(1, nrow=dim(C)[1], ncol=dim(C)[2])
  D = M - C
  
  for (i in 1:n2 ){
    
    row = D[,i]
    minindices = which(row %in% sort(row)[1:K])
    
    noOfOnes = 0
    
    for (j in 1:length(minindices)){
      if (data[minindices[j],p] == 1) {
        noOfOnes = noOfOnes + 1
      }
    }
    
    Prob[i] = noOfOnes/K
  }
  return(Prob)
}

csvfile = read.csv("spambase.csv", dec=",")

n <- dim(csvfile)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train = csvfile[id,]
test = csvfile[-id,]

############## K = 5 ##############
prob_K5 = knearest(train,5,test)
pred_K5 = numeric(length(prob_K5))

for (i in 1:length(prob_K5)){
  if (prob_K5[i] > 0.5){
    pred_K5[i] = 1
  }
}

confusionMatrix_K5 = table(pred_K5, test[,49])
misclassification_rate_K5 = 1-sum(diag(confusionMatrix_K5)/sum(confusionMatrix_K5))

print("Confusion matrix K = 5")
print(confusionMatrix_K5)
print("Misclassification rate K = 5")
print(misclassification_rate_K5)

############## K = 1 ##############
prob_K1 = knearest(train,1,test)
pred_K1 = numeric(length(prob_K1))

for (i in 1:length(prob_K1)){
  if (prob_K1[i] > 0.5){
    pred_K1[i] = 1
  }
}

confusionMatrix_K1 = table(pred_K1, test[,49])
misclassification_rate_K1 = 1-sum(diag(confusionMatrix_K1)/sum(confusionMatrix_K1))

print("Confusion matrix K = 1")
print(confusionMatrix_K1)
print("Misclassification rate K = 1")
print(misclassification_rate_K1)

############## K = 5 with kknn ##############
prob_K5_kknn = kknn(Spam ~ .,train=train, test=test, k=5)
pred_K5_kknn = numeric(length(fitted(prob_K5_kknn)))

for (i in 1:length(fitted(prob_K5_kknn))){
  if (fitted(prob_K5_kknn)[i] > 0.500000) {
    pred_K5_kknn[i] = 1
  }
}

confusionMatrix_K5_kknn = table(pred_K5_kknn, test[,49])
misclassification_rate_K5_kknn = 1-sum(diag(confusionMatrix_K5_kknn)/sum(confusionMatrix_K5_kknn))

print("Compare own calculations and kknn-package")
print("K = 5: Own confusion matrix")
print(confusionMatrix_K5)
print("K = 1: Own confusion matrix")
print(confusionMatrix_K1)
print("K = 5: kknn-package confusion matrix")
print(confusionMatrix_K5_kknn)

print("K = 5: Own misclassification rate")
print(misclassification_rate_K5)
print("K = 1: Own misclassification rate")
print(misclassification_rate_K1)
print("K = 5: kknn-package misclassificationr ate")
print(misclassification_rate_K5_kknn)

ROC=function(Y, Yfit, p){
  m=length(p) # Length of 
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    if (dim(t)[1] == 2) {
      TPR[i]=t[2,2]/(t[2,2]+t[1,2])
    } else {
      TPR[i]=0
    }

    FPR[i]=t[2,1]/(t[2,1]+t[1,1])
  }
  return (list(TPR=TPR,FPR=FPR))
}

############## ROC, sensitivity and specificity ##############
p <- seq(from=0.05, to=0.95, by=0.05)
roc_kknn = ROC(test[,49], fitted(prob_K5_kknn), p)
roc_own = ROC(test[,49], prob_K5, p)

plot(roc_kknn$FPR, roc_kknn$TPR, xlab="FPR", ylab="TPR", ylim=c(0,1), xlim=c(0,1), main="ROC (K=5)", sub="Plot:ROC kknn | Line:ROC own")
lines(roc_own$FPR, roc_own$TPR, xlab="FPR", ylab="TPR", ylim=c(0,1), xlim=c(0,1))
