
###############################################################
# Advanced: only if you want to see how matrix arithmetic is 
# used to compute ridge
###############################################################

library(glmnet)
library(corrplot)
library(MASS)

# First we will do a little comparison of 
# Maual regularization, the test dataset for glmnet 
# is a nice small example to start with

load("QuickStartExample.RData")

# This dataset has an x-variable set with twenty columns
# and a y-variable set with one column

head(x)
head(y)

ds = as.data.frame(x)
dim(ds)
ds$y = y

# now reorder the columns
ds = ds[, c(21, 1:20)]
corrplot(cor(ds), method="ellipse")
head(ds)

##############################################################
# First let's let R do its job automatically

fitAuto = lm(y ~ ., data=ds[1:50, ])  # train on the first 50
summary(fitAuto)
plot(fitAuto)

xTrain = ds[1:50, 2:21]
yTrain = ds[1:50, 1]
yHat = predict(fitAuto)

# Note: degrees of freedom for residuals is 29 = 50 - $variables - 1
resid = yTrain - yHat
rse = sqrt(sum(resid^2) / 29)
rse  # Same as in summary!

##############################################################
# Now, lets to it manually
# Make a training set of the first and append the column of ones
xTrain = x[1:50, ]
xTrain = cbind(rep(1, 50), xTrain)
yTrain = y[1:50, ]

# Do the same thing for the test set
xTest = x[51:100, ]
xTest = cbind(rep(1, 50), xTest)
yTest = y[51:100, ]

# Try an initial fit
beta = ginv(t(xTrain) %*% xTrain) %*% t(xTrain) %*% yTrain
beta  # Same as fitAuto above

# Residual error
yHat = xTrain %*% beta
rse = sqrt(sum((yTrain - yHat)^2) / 29) 
rse

# Now, get the prediction values for the test set
yPredict = xTest %*% beta
rse_predict = sqrt(sum((yTest - yPredict)^2) / 29)

# Notice how much bigger this is!
rse_predict

##########################################################
# Now, let's try a ridge regression, for example sake, 
# try lambda = 1
lambda = 1
Ident = diag(ncol(xTrain))

betaRidge = ginv(t(xTrain) %*% xTrain + lambda * Ident) %*% t(xTrain) %*% yTrain
betaRidge  

# Residual error
yHat = xTrain %*% betaRidge
rseRidge = sqrt(sum((yTrain - yHat)^2) / 29)  
rseRidge

# Now, get the prediction values for the test set
yPredict = xTest %*% betaRidge
rse_predict_ridge = sqrt(sum((yTest - yPredict)^2) / 29)

# Notice how the value is slightly better
rse_predict_ridge
rse_predict

# Just need to do a search for the best cross validated fit!
l = (1:100) / 10
b = array(0, c(21, 100))
rse = array(0, 100)
for (i in 1:length(l)) 
{
  b[,i] = ginv(t(xTrain) %*% xTrain + (l[i] * Ident)) %*% (t(xTrain) %*% yTrain)
  yPredict = xTest %*% b[, i]
  rse[i] = sqrt(sum((yTest - yPredict)^2) / 29)
}

library(ggplot2)
qplot(l, rse)
rse

# Which lambda gives the minimum prediction rmse?
minI = which(rse == min(rse), arr.ind=T)
l[minI]
rse[minI]

# Best model
b[, minI]

# We could do more diagnositics ...
