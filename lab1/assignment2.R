#################### Assignment 2 ####################
## The data file machines.xlsx contains information about the lifetime of certain machines, 
## and the company is interested to know more about the underlying process in order to 
## determine the warranty time.

library(ggplot2)
set.seed(12345)

# Task 1
## Import the data to R.
csvfile = read.csv("machines.csv", dec=",")

# Task 2
## Assume the probability model p(x|θ) = θ*exp(-θx) for x=Length in which observations are independent 
## and identically distributed. What is the distribution type of x? Write a function 
## that computes the log-likelihood log(p(x|θ)) for a given θ and a given data vector x.
## Plot the curve showing the dependence of log-likelihood on θ where the entire data is 
## used for fitting. What is the maximum likelihood value of θ according to the plot?

loglikelihood = function(θ, X){
  return(dim(X)[1]*log(θ)-θ*sum(X))
}

plotloglikelihood = function(X, col, ADD){
  curve(dim(X)[1]*log(x)-x*sum(X), from=min(X), ylim=c(-80,0), col=col,  to=4, ylab="log(p(x|θ))", sub="Red: 6 obs | Blue: All obs", xlab="θ", add=ADD)
}

# Graph shows approx θ* = 1,10
# Actual Maximum-likelihood θ* = 1,126217
maximumlikelihood = function(X){
  return(dim(X)[1]/sum(X))
}

sumX <- sum(csvfile)
n <- dim(csvfile)[1]

plotloglikelihood(csvfile, "red", FALSE)

print(maximumlikelihood(csvfile))

# Task 3
## Repeat step 2 but use only 6 first observations from the data, and put the two 
## log-likelihood curves (from step 2 and 3) in the same plot. What can you say 
## about reliability of the maximum likelihood solution in each case?

csvfile_6_obs <- matrix(csvfile[1:6,1], nrow=length(csvfile[1:6,1]), ncol=1)

plotloglikelihood(csvfile_6_obs, "red", FALSE)
plotloglikelihood(csvfile, "blue", TRUE)

# Task 4
## Assume now a Bayesian model with p(x|θ) = θ*exp(-θx) and a prior p(θ) = λ*exp(-λθ), λ = 10.
## Write a function computing l(θ) = log(p(x|θ)*p(θ)). What kind of measure is actually computed 
## by this function? Plot the curve showing the dependence of l(θ) on θ computed using the entire data, 
## find an optimal θ and compare your result with the previous findings.
bayesianModel = function(θ, x, λ) {
  n <- dim(x)[1]
  sumX <- sum(x)
  return(n*log(θ) - θ*sumX + log(λ)- λ*θ)
}

plotBayesianModel = function(X, λ) {
  n <- dim(X)[1]
  sumX <- sum(X)
  curve(n*log(x) - x*sumX + log(λ)- λ*x, xlab="θ", ylab="l(θ)", from=0, to=5)
}

optimalBayesianTheta = function(X, λ) {
  n <- dim(X)[1]
  sumX <- sum(X)
  return(n/(λ + sumX))
}

plotBayesianModel(csvfile, 10)

# Task 5
## Use θ value found in step 2 and generate 50 new observations from p(x|θ) = θ*exp(-θx)
## (use standard random number generators). Create the histograms of the original and the 
## new data and make conclusions.
set.seed(12345)
theta <- maximumlikelihood(csvfile)
newdata = rexp(50, rate=theta)
olddata <- csvfile$Length

hist(olddata, col="red", xlim=c(0,5), ylim=c(0,20), xlab="x")
hist(newdata, col="blue", xlim=c(0,6), ylim=c(0,20), breaks="FD", xlab="x")