################# Assignment 4 #################
## The data file NIRspectra.csv contains near-infrared spectra and viscosity levels for a
## collection of diesel fuels. Your task is to investigate how the measured spectra can be
## used to predict the viscosity.

library(fastICA)
library(pls)
df <- read.csv2("NIRspectra.csv")

## Task 1 ##
## Conduct a standard PCA by using the feature space and provide a plot
## explaining how much variation is explained by each feature. Does the plot
## show how many PC should be extracted? Select the minimal number of
## components explaining at least 99% of the total variance. Provide also a plot
## of the scores in the coordinates (PC1, PC2). Are there unusual diesel fuels
## according to this plot?

# Conduct standard PCA
data1 <- df
data1$Viscosity <- c()
res <- prcomp(data1)
lambda <- res$sdev^2 # Eigenvalues

# Display how much of the variation each feature captures
sprintf("%2.3f", lambda/sum(lambda)*100)

# Plot histogram of variance
screeplot(res)

# Plot scores of coordinates PC1 & PC2
plot(res$x[,1], res$x[,2], main = "PC1 vs. PC2", xlab = "PC1", ylab = "PC2")

## Task 2 ##
## Make trace plots of the loadings of the components selected in step 1. Is there
## any principle component that is explained by mainly a few original features?

# Trace plot the loadings of PC1 and PC2
U <- res$rotation
plot(U[,1], main="Traceplot, PC1")
plot(U[,2], main="Traceplot, PC2")

## Task 3 ##
## Perform Independent Component Analysis with the number of components
## selected in step 1 (set seed 12345). Check the documentation for the fastICA
## method in R and do the following:
##  a. Compute W' = K * W and present the columns of W' in form of the
##     trace plots. Compare with the trace plots in step 2 and make
##     conclusions. What kind of measure is represented by the matrix W'?
##  b. Make a plot of the scores of the first two latent features and compare
##     it with the score plot from step 1.

# Perform ICAs
set.seed(12345)
ICA <- fastICA(data1, n.comp = 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001,
               verbose = TRUE)

WTICK <- ICA$K %*% ICA$W

# Trace plot results from ICAs
plot(WTICK[,1], main= "Latent feature 1")
plot(WTICK[,2], main= "Latent feature 2")

# Plot of scores
plot(ICA$S[,1], ICA$S[,2], main = "Score", ylab = "Latent 2", xlab = "Latent 1")

## Task 4 ##
## Fit a PCR model in which number of components is selected by cross
## validation to the data, use seed 12345. Provide a plot showing the
## dependence of the mean-square predicted error on the number of the
## components in the model and comment how many components it is
## reasonable to select.

set.seed(12345)

# Fit PCR-model
pcr.fit <- pcr(Viscosity ~ ., data = df, validation = "CV")

# Summary of PCR-model
summaryPCR <- summary(pcr.fit)

# Validation plot of PCR-model
validationplot(pcr.fit, val.type = "MSEP")