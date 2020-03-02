
library(glmnet)
library(corrplot)
library(MASS)

#################################################################
# Now, let's analyze this dataset with a more powerful tool
load("QuickStartExample.RData")

# This dataset has an x-variable set with twenty columns
# and a y-variable set with one column. 
#
# The datasets here need to be matrices
head(x)
head(y)

s = sample(1:100, 60)  
xTrain = x[s, ]
yTrain = y[s, ]
xTest = x[-s, ]
yTest = y[-s, ]

# Let's run a quick regression on this
ds = data.frame(yTrain, xTrain)
names(ds)[1] = "Y"
head(ds)
fitOld = lm(Y ~ ., data=ds)
summary(fitOld)
yHat = predict(fitOld, data.frame(xTest))
c = fitOld$coefficients
dof = length(yTest) - length(c)
rsePredict = sqrt(sum((yTest - yHat)^2) / dof)
rsePredict   

# The glmnet function 
# Defaults to lasso, but can also do a version of ridge
fit = glmnet(xTrain, yTrain)  # note there is no ~ here and y comes after the x's.
plot(fit, label=T)

summary(fit)  # not much help :)
print(fit)
names(fit)

# Let's look at the coefficients for the model at a specific lambda
# Note: It selected all of the variables with some contribution
coef(fit, s=.35)    # Why s and not lambda ... sigh

c = as.matrix(coef(fit, s=.35))
yHat = predict(fit, xTrain, s=.35)
dof = length(yTrain) - length(c)
rse = sqrt(sum((yTrain - yHat)^2) / dof)  # Still 20 variables in the regression
rse
head(yHat)
head(yTrain)

yPredict = predict(fit, xTest, s=.35)
dof = length(yTest) - length(c)
rsePredict = sqrt(sum((yTest - yPredict)^2) / dof)
rsePredict   

head(yHat)
head(yTrain)

#########################################################
# One of the nice things about this function is its 
# Ability to test with cross validation to choose the lamba

cvfit = cv.glmnet(xTrain, yTrain)
par(mar=c(3, 3, 3, 3))
plot(cvfit)

print(cvfit$lambda.min)
coef(cvfit, s="lambda.min")  # Note we are getting a lot more contributions here

yPredict = predict(cvfit, newx=xTest, s="lambda.min")
dof = length(yTest) - length(c)
rsePredict = sqrt(sum((yTest - yPredict)^2) / dof)
rsePredict
