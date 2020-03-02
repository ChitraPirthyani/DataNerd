
library(foreign)  # Allows us to read spss files!
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)

# Read in the hbat spss "hbat" dataset from the book by Hair, et. al.
hbat = read.spss("HBAT.sav", to.data.frame=T)
head(hbat)

names(hbat) = c("id", "CType", "IndType", "Size", "Region", "DistSystem", "ProdQual", "ECom", "Tech", "Complaint", "Advert", "PLine", "SFImage", "CompPric", "War", "NewProd", "OrderBill", "PFlex", "DelSpd", "CustSatis", "Recommend", "FPurchase", "PPercent", "FRelation")
names(hbat)

# Unfortunately, the numeric columns get read in with some labels for the
# ends of the scale and R interprets them as factors.  Worse, we cannot
# just use "as.numeric" on the subset of columns, it gives an error, so the
# only way to do it is to go through the columns one-by-one
for(i in 7:20)
{
  hbat[ , i] = as.numeric(hbat[ , i])
}

# Pull out just the numeric fields and place customer satisfaction 
# at the front because it will be our "Y".  It makes it easier to 
# interpret correlation matrices!
hbatNumeric = hbat[, c(20, 7:19)]

# Build an 80/20 split
nrow(hbatNumeric)   # returns 100
s = sample(1:100, 80)
hbatTrain = hbatNumeric[s, ]  # only the rows in "sample", but all the columns
hbatTest = hbatNumeric[-s, ]

# Try a fit of the full set of parameters.  Note that the . in 
# the regression formula gives all the rest of the parameters.
fullFit = lm(CustSatis ~ ., data=hbatTrain)
summary(fullFit)

# Now, fit with a single parameter (the one with the highest correlation)
round(cor(hbatNumeric), 2)
fit1 = lm(CustSatis ~ Complaint, data=hbatTrain)
summary(fit1)

# Try adding another parameter (this one has the most correlation with the residuals of the last fit)
# Question: Why don't we use the next highest correlation from the last correlation matrix?
tmpData = hbatTrain
tmpData$lastResidual = fit1$residuals
tmpData = tmpData[ ,c(15, 2:14)]   # Reorder to put the residuals in the first column and remove the original parameter of interest 
round(cor(tmpData), 2)  # Note that ProdQual has the highest absolute value in the first column
fit2 = lm(CustSatis ~ Complaint + ProdQual, data=hbatTrain)
summary(fit2)

###################################################################
# Automated fitting
###################################################################

# The R function "step" can perform stepwise regression, but to get going, we need to feed 
# it the two "bounding" models so that it knows what to work with
null = lm(CustSatis ~ 1, data=hbatTrain)
full = lm(CustSatis ~ ., data=hbatTrain)

# First we do a forward search
hbatForward = step(null, scope = list(lower=null, upper=full), direction="forward")
# Next do a backward search
hbatBackward = step(full, direction="backward")
# Finally we do a "stepwise" search combining the two
hbatStep = step(null, scope = list(upper=full), direction="both")

# Compare the results
summary(hbatStep)
summary(hbatForward)
summary(hbatBackward)

# Now, let's look at how the model do on the test data
PStep = predict(hbatStep, newdata=hbatTest)
PForward = predict(hbatForward, newdata=hbatTest)
PBackward = predict(hbatBackward, newdata=hbatTest)

Y = hbatTest$CustSatis

step.RMSE = sqrt(mean((Y - PStep)^2))
step.RMSE

forward.RMSE = sqrt(mean((Y - PForward)^2))
forward.RMSE

backward.RMSE = sqrt(mean((Y - PForward)^2))
backward.RMSE

#############################################
# Try a ridge regression
#############################################

fitRidge1 = lm.ridge(CustSatis ~ ., data=hbatTrain)
print(fitRidge1)
select(fitRidge1) # Ridge doesn't detect any value from 
                  # regularization

#############################################
# Set up the data for working with glmnet.
# Remember, it requires matrices and/or vectors
# not data.frame
#############################################

library(glmnet)

xTrain = as.matrix(hbatTrain[, 2:14])  # Only the x's
yTrain = as.matrix(hbatTrain$CustSatis)

#############################################
# Try ridge again
#############################################

cvRidge = cv.glmnet(xTrain, yTrain, alpha=0)  # Alpha = 0 gives ridge
print(cvRidge$lambda.min)
coef(cvRidge, s="lambda.min")  # Note we are getting a lot more contributions here
plot(cvRidge)

# Now, use it to predict
xTest = as.matrix(hbatTest[, 2:14])
yTest = as.matrix(hbatTest$CustSatis)
yRidge = predict(cvRidge, newx=xTest, s="lambda.min")

# Now compute the new RSE 
rmseRidge = sqrt(mean((yTest - yRidge)^2))
rmseRidge   # Going the wrong direction!

#############################################
# Now let's do a lasso regression
#############################################

# Compute the cross-validated lasso fit
cvLasso = cv.glmnet(xTrain, yTrain)
print(cvLasso$lambda.min)
coef(cvLasso, s="lambda.min")  # Note we are getting a few more contributions here
plot(cvLasso)

# Now, use it to predict
xTest = as.matrix(hbatTest[, 2:14])
yTest = as.matrix(hbatTest$CustSatis)
yPredict = predict(cvfit, newx=xTest, s="lambda.min")

rmseLasso = sqrt(mean((yTest - yPredict)^2))
rmseLasso   # That's better!
