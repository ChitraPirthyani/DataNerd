
library(glmnet)
library(corrplot)
library(MASS)

#################################################################
# Now, let's analyze this dataset with a more powerful tool
load("QuickStartExample.RData")

# This dataset has an x-variable set with twenty columns
# and a y-variable set with one column

head(x)
head(y)

# Defaults to lasso, but can also do a version of ridge
rows = sample(1:100, 50)
xTrain = x[rows, ]
yTrain = y[rows, ]
xTest = x[-rows, ]
yTest = y[-rows, ]

# Put them in a data.frame for lm
dsTrain = as.data.frame(cbind(yTrain, xTrain))
dsTest = as.data.frame(cbind(yTrain, xTrain))

names(dsTrain) = c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20")
names(dsTest) = c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20")
head(dsTrain)
head(dsTest)

library(leaps)

fitNull = lm(y ~ 1, dsTrain)
fitFull = lm(y ~ ., dsTrain)

fitForward = step(fitNull, scope = list(lower=fitNull, upper=fitFull), direction="forward")
fitBackward = step(fitFull, scope = list(lower=fitNull, upper=fitFull), direction="backward")
fitStepwise = step(fitNull, scope = list(lower=fitNull, upper=fitFull), direction="both")

summary(fitForward)
summary(fitBackward)
summary(fitStepwise)   # They all have 9 parameters and agree quite well

yBack = predict(fitBackward, dsTest)
rmse = sqrt(mean((yBack - yTest)^2)) 
rmse

###########################################################################################
# Now, let's apply LASSO
###########################################################################################

# Remember, alpha = 1 gives LASSO
fit = glmnet(xTrain, yTrain)  # note there is no ~ here and y comes after the x's.
plot(fit, label=T)

summary(fit)  # not much help :)
print(fit)    # R^2 of .9 with 9 parameters and a lambda of .13 

# Let's look at the coefficients for the model at a specific lambda
coef(fit, s=.13)    # Why s and not lambda ... sigh

yLasso = predict(fit, xTrain, s=.13)
rmse = sqrt(mean((yLasso - yTest)^2))  
rmse  # A small reduction

#############################################################
# One of the nice things about this package is its 
# Ability to test with cross validation to choose the lamba
#############################################################
cvfit = cv.glmnet(xTrain, yTrain)
par(mar=c(3, 3, 3, 3))
plot(cvfit)
cvfit$lambda.min

coef(cvfit, s="lambda.min")  # Note we are getting a lot more contributions here

yPredict = predict(cvfit, newx=xTest, s="lambda.min")
rmsePredict = sqrt(mean((yTest - yPredict)^2))
rmsePredict  # Quite a bit better

