#################### Assignment 4 ####################
# The Excel file tecator.xlsx contains the results of study aimed to investigate whether a near infrared 
# absorbance spectrum can be used to predict the fat content of samples of meat. 
# For each meat sample the data consists of a 100 channel spectrum of absorbance records 
# and the levels of moisture (water), fat and protein. The absorbance is -log10 of the transmittance 
# measured by the spectrometer. The moisture, fat and protein are determined by analytic 
# chemistry.

require(ggplot2)
require(MASS)
require(glmnet)

set.seed(12345)

# Task 1
## Import data to R and create a plot of Moisture versus Protein. 
## Do you think that these data are described well by a linear model?
csvfile = read.csv("tecator.csv", dec=",")
print(csvfile)
moisture <- csvfile$Moisture
protein <- csvfile$Protein

breaks = seq(from=10, to=25, by=0.2)

hist(moisture, xlim=c(min(moisture),max(moisture)), breaks=215)

plot(moisture, protein)

# Task 2
## Consider model Mi in which Moisture is normally distributed, and the expected
## Moisture is a polynomial function of Protein including the polynomial terms 
## up to power i (i.e M1 is a linear model, M2 is a quadratic model and so on).
## Report a probabilistic model that describes Mi. Why is it appropriate to use MSE 
## criterion when fitting this model to a training data?

# M1 = w0 + w1*P1 + ε
# M2 = w0 + w1*P1 + w2*P2 + ε
# ε ~ N(0, σ2)
# MSE minimizes the error in the prediction, and is therefore fitting
print(moisture[2:length(moisture)])

combineddata <- csvfile[,103:104]

print(combineddata)
fit <- lm(formula = moisture ~ protein, data=combineddata)
summary(fit)
fitted <- predict(fit, interval = "confidence")
attach(combineddata)
plot(protein, moisture, main="Moisture vs. Protein")
lines(protein, fitted[, "fit"])

# Task 3
## Divide the data into training and validation sets( 50%/50%) and fit models
## Mi, i=1..6. For each model, record the training and the validation MSE and 
## present a plot showing how training and validation MSE depend 
## on i (write some R code to make this plot). Which model is best according 
## to the plot? How do the MSE values change and why? Interpret this picture 
## in terms of bias-variance tradeoff.

mean_squared_error = function(y, y_hat) {
  squared_error = (y - y_hat)^2
  sum_squared_error = sum(squared_error)
  n = length(squared_error)
  return(sum_squared_error/n)
}

# Divid set into 50%/50% train/test set
set.seed(12345)
n <- dim(csvfile)[1]
id <- sample(1:n, floor(n*0.5))
train <- csvfile[id,]
test <- csvfile[-id,]

moisture_train = train$Moisture
moisture_test = test$Moisture

# Create regression models for training set
i_1_fit_train <- lm(formula = Moisture ~ Protein, data=train)
i_2_fit_train <- lm(formula = Moisture ~ Protein + I(Protein^2), data=train)
i_3_fit_train <- lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3), data=train)
i_4_fit_train <- lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4), data=train)
i_5_fit_train <- lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5), data=train)
i_6_fit_train <- lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5) + I(Protein^6), data=train)

# Create predictions by using the training set
fitted_i_1_train <- predict(i_1_fit_train, newdata = train)
fitted_i_2_train <- predict(i_2_fit_train, newdata = train)
fitted_i_3_train <- predict(i_3_fit_train, newdata = train)
fitted_i_4_train <- predict(i_4_fit_train, newdata = train)
fitted_i_5_train <- predict(i_5_fit_train, newdata = train)
fitted_i_6_train <- predict(i_6_fit_train, newdata = train)

# Create predictions by using the testing set
fitted_i_1_test <- predict(i_1_fit_train, newdata = test)
fitted_i_2_test <- predict(i_2_fit_train, newdata = test)
fitted_i_3_test <- predict(i_3_fit_train, newdata = test)
fitted_i_4_test <- predict(i_4_fit_train, newdata = test)
fitted_i_5_test <- predict(i_5_fit_train, newdata = test)
fitted_i_6_test <- predict(i_6_fit_train, newdata = test)

# MSE train set
MSE_train = numeric(6)
MSE_train[1] = mean_squared_error(moisture_train, fitted_i_1_train)
MSE_train[2] = mean_squared_error(moisture_train, fitted_i_2_train)
MSE_train[3] = mean_squared_error(moisture_train, fitted_i_3_train)
MSE_train[4] = mean_squared_error(moisture_train, fitted_i_4_train)
MSE_train[5] = mean_squared_error(moisture_train, fitted_i_5_train)
MSE_train[6] = mean_squared_error(moisture_train, fitted_i_6_train)

# MSE test set
MSE_test = numeric(6)
MSE_test[1] = mean_squared_error(moisture_test, fitted_i_1_test)
MSE_test[2] = mean_squared_error(moisture_test, fitted_i_2_test)
MSE_test[3] = mean_squared_error(moisture_test, fitted_i_3_test)
MSE_test[4] = mean_squared_error(moisture_test, fitted_i_4_test)
MSE_test[5] = mean_squared_error(moisture_test, fitted_i_5_test)
MSE_test[6] = mean_squared_error(moisture_test, fitted_i_6_test)

# Plot showing how training and validation MSE depend on i
plot(1:6, MSE_test, type="l", col="red", ylab="MSE", xlab="i", main="MSE's dependence of i", 
     ylim=c(30,35), sub="Blue: MSE_train | Red: MSE_test")
par(new=TRUE)
lines(1:6, MSE_train, col="blue", type="l", ylab="", xlab="")

# Task 4
## Perform variable selection of a linear model in which Fat is response 
## and Channel1-Channel100 are predictors by using stepAIC. Comment on how 
## many variables were selected.

fat <- csvfile$Fat
predictors <- data.frame(csvfile[,1:101])
fit <- lm(fat~., data=predictors)
step <- stepAIC(fit, direction="both")
step$anova
summary(step)

# Task 5
## Fit a Ridge regression model with the same predictor and response variables. 
## Present a plot showing how model coefficients depend on the log of the penalty 
## factor Lambda and report how the coefficients change with Lambda.

# The coefficients goes towards 0, when lambda goes towards infinity
covariates = scale(csvfile[, 2:101])
response = scale(csvfile$Fat)
ridge_model = glmnet(as.matrix(covariates),
                     response, alpha = 0, family="gaussian")
plot(ridge_model, xvar="lambda", label=TRUE, main="Ridge regression \n\n")

# Task 6
## Repeat step 6 but fit LASSO instead of the Ridge regression and compare the 
## plots from steps 6 and 7. Conclusions?
lasso_model = glmnet(as.matrix(covariates),
                     response, alpha = 1, family="gaussian")
plot(lasso_model, xvar="lambda", label=TRUE, main="LASSO regression\n\n")

# Task 7
## Use cross-validation to find the optimal LASSO model (make sure that case
## λ = 0 is also considered by the procedure) , report the optimal λ
## and how many variables were chosen by the model and make
## conclusions. Present also a plot showing the dependence of the CV 
## score and comment how the CV score changes with λ.
lasso_model_cv = cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian", lambda=seq(0,1,0.001))
plot(lasso_model_cv, xvar="lamdba", label=TRUE, main="LASSO Cross-validation\n\n")
coef(lasso_model_cv, s="lambda.min")
print(lasso_model_cv$lambda.min)