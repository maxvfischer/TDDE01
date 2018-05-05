################# Assignment 3 #################
## The data file State.csv contains per capita state and local public expenditures and
## associated state demographic and economic characteristics, 1960, and there are variables
## • MET: Percentage of population living in standard metropolitan areas
## • EX: Per capita state and local public expenditures ($).

library(tree)
library(boot)

df = read.csv2("State.csv")

## Task 1 ##
## Reorder your data with respect to the increase of MET and plot EX versus MET.
## Discuss what kind of model can be appropriate here. Use the reordered data in
## steps 2-5.

# Sort csvfile by MET
df.sorted <- df[order(df$MET),]

# Plot EX against MET
plot(df.sorted$MET, df.sorted$EX, main = "EX vs. MET", ylab = "EX", xlab = "MET", ylim = c(150,500))

## Task 2 ##
## Use package tree and fit a regression tree model with target EX and feature MET
## in which the number of the leaves is selected by cross-validation, use the entire
## data set and set minimum number of observations in a leaf equal to 8 
## (setting minsize in tree.control). Report the selected tree. Plot the original and the fitted
## data and histogram of residuals. Comment on the distribution of the residuals and
## the quality of the fit.

# Set nobs, fit tree
nobs <- dim(df.sorted)[1]
fit <- tree(EX ~ MET, data = df.sorted, control = tree.control(nobs, minsize = 8))

# Generate cross-validation
set.seed(12345)
cvFit <- cv.tree(fit)
plot(cvFit$size, cvFit$dev, type="b", col="red", xlab="Size", ylab="Dev")

# 3 leaves had the least deviation. Generate new optimal tree
pruneOptTree <- prune.tree(fit, best = 3)
plot(pruneOptTree)
text(pruneOptTree, pretty=0)
pruneOptTreePred <- predict(pruneOptTree, newdata = df.sorted)
title("Optimal tree")

# Plot original and fitted data
plot(df.sorted$MET, df.sorted$EX, col="blue", ylab = "EX", xlab = "MET") # Original
points(df.sorted$MET, pruneOptTreePred, col="red")
legend("top", legend = c("Original data (blue)", "Predicted data (red)"))

# Generate histogram of Residuals
summaryFit <- summary(pruneOptTree)
hist(summaryFit$residuals, xlab="Residuals", main="Freq vs. Res", breaks=20, xlim=c(-100,150))

## Task 3 ##
## Compute and plot the 95% confidence bands for the regression tree model from
## step 2 (fit a regression tree with the same settings and the same number of leaves
## as in step 2 to the resampled data) by using a non-parametric bootstrap. Comment
## whether the band is smooth or bumpy and try to explain why. Consider the width
## of the confidence band and comment whether results of the regression model in
## step 2 seem to be reliable.

# Function from lectures, replaced with tree
f1 <- function(in.data, ind) {
  data1 <- in.data[ind,] # Extract bootstrap sample
  fittedTree <- tree(EX ~ MET, data = data1, control = tree.control(nobs, minsize = 8))
  pruneTree <- prune.tree(fittedTree, best = 3)
  predData <- predict(pruneTree, newdata = in.data)
  return(predData)
}

# Non-parametric bootstrap for regression tree
nonParBootstrap <- boot(df.sorted, statistic = f1, R = 1000)

# Confidence interval
ciNonParametric <- envelope(nonParBootstrap)

# Plot confidence interval
plot(df.sorted$MET, df.sorted$EX, main = "Non-parametric confidence band (95%)", ylab = "EX", xlab = "MET", col = "red")
points(df.sorted$MET, pruneOptTreePred, type = "l", col="blue")
points(df.sorted$MET, ciNonParametric$point[1,], type = "l")
points(df.sorted$MET, ciNonParametric$point[2,], type = "l")

## Task 4 ##
## Compute and plot the 95% confidence and prediction bands the regression tree 
## model from step 2 (fit a regression tree with the same settings and the same 
## number of leaves as in step 2 to the resampled data) by using a parametric 
## bootstrap, assume Y ~ N(µi,σ^2) where µi are labels in the tree leaves and 
## σ^2 is the residual variance. Consider the width of the confidence band and 
## comment whether results of the regression model in step 2 seem to be reliable. 
## Does it look like only 5% of data are outside the prediction band? Should it be?
mle <- pruneOptTree

# Function from lectures, replaced with tree
f2 <- function(in.data) {
  fittedTree <- tree(EX ~ MET, data = in.data, control = tree.control(nobs, minsize = 8))
  pruneTree <- prune.tree(fittedTree, best = 3)
  predData <- predict(pruneTree, newdata = in.data)
  return(predData)
}

# Same function as f2, but a distribution is added for prediction band
f3 <- function(in.data) {
  n <- length(in.data$EX)
  fittedTree <- tree(EX ~ MET, data = in.data, control = tree.control(nobs, minsize = 8))
  pruneTree <- prune.tree(fittedTree, best = 3)
  predData <- predict(pruneTree, newdata = in.data)
  summaryMLE <- summary(mle)
  pred <- rnorm(n, predData, sd(summaryMLE$residual))
  return(pred)
}

rng <- function(in.data, mle) {
  data1 <- data.frame(EX = in.data$EX, MET = in.data$MET)
  n <- length(in.data$EX)
  summaryMle <- summary(mle)
  data1$EX <- rnorm(n, predict(mle, newdata = data1),
                    sd(summaryMle$residual))
  return(data1)
}

# Parametric Bootstrap and confidence band for regression tree
set.seed(12345)
parBootstrapCi <- boot(df.sorted, statistic = f2, R = 1000,
                     mle = mle, ran.gen = rng, sim = "parametric")

ciParametric <- envelope(parBootstrapCi)

# Plot confidence interval
plot(df.sorted$MET, df.sorted$EX, ylab = "EX", xlab = "MET", main = "Parametric confidence band (95%)", col="red")
points(df.sorted$MET, pruneOptTreePred, type = "l")
points(df.sorted$MET, ciParametric$point[1,], type = "l")
points(df.sorted$MET, ciParametric$point[2,], type = "l")

# Parametric Bootstrap and prediction band for regression tree
set.seed(12345)
parBootstrapPi <- boot(df.sorted, statistic = f3, R = 1000,
                     mle = mle, ran.gen = rng, sim = "parametric")

# Prediction band
PiParametric <- envelope(parBootstrapPi)

# Plot confidence interval
plot(df.sorted$MET, df.sorted$EX, ylim = c(100,450), ylab = "EX", xlab = "MET", main = "Parametric Prediction band (95%)", col="red")
points(df.sorted$MET, pruneOptTreePred, type = "l")
points(df.sorted$MET, PiParametric$point[1,], type = "l")
points(df.sorted$MET, PiParametric$point[2,], type = "l")
