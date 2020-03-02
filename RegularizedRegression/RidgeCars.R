
# Load libraries, get data & set seed for reproducibility ---------------------
library(glmnet)  # for ridge & lasso regression
library(MASS)    # for a simpler ridge regression function

set.seed(3915)    # seef for reproducibility
data("mtcars")
head(mtcars)

# First, we will try the lm.ridge function, it takes a series
# of lambdas to try.  It uses a method called generalized cross 
# validation and also gives a couple of other estimates for the 
# best lambda
lambdas_to_try = seq(0, 20, .1)
fitRidge = lm.ridge(mpg ~ ., mtcars, lambda=lambdas_to_try)
print(fitRidge)
select(fitRidge)   # The GCV selects quite a large lambda
plot(fitRidge)     # But the HKB and L-W estimators are a lot smaller
                   # Again, consensus is good, but here we have a large
                   # Disagreement.  Let's see what the HKB estimator gives

fitRidge = lm.ridge(mpg ~ ., mtcars, lambda=c(2.58))
print(fitRidge)

# Unfortunately, lm.ridge does not have a corresponding "predict" 
# 
# To predict with an lm.ridge model, we need to manually apply it
# this can be done with matrix multiplication if we bind a column 
# of ones!
# 
# We have to take the parameter of interest (mpg) off to do it!
y.pred = as.matrix(cbind(const=1,mtcars[, -1])) %*% coef(fitRidge)
head(y.pred)
head(mtcars$mpg)

# We can then calculate an rmse very easily
rmseRidge = sqrt(mean((y.pred - mtcars$mpg)^2))
rmseRidge

#################################################################
# The glmnet function in the glmnet package has a more complete
# set of features, and allows us to explore more options and even
# to combine ridge and lasso techniques
#################################################################

# First, to use glmnet, we must convert the data to matrices.
# Unfortunately, glmnet does not use the more familiar syntax 
# from lm.  We make a set of X variables by removing the parameter
# of interest "mpg", and we create the y varaible by pulling just
# the mpg variable
X = as.matrix(mtcars[, -1])
y = as.matrix(mtcars[, 1])
head(X)
head(y)

# The glmnet function comes in two versions, the first
# allows us to explore the range of lambdas and their 
# effects on the models.  The one thing that is confusing
# about this function is that 
ridgeFit = glmnet(X, y, alpha=0)
print(ridgeFit)
plot(ridgeFit)   # This plot shows the shrinkage with respect to
                 # The overall size of the coefficients.  
                 # They all shrink to 0 but they don't
                 # do so uniformly.  You will also notice 
                 # That some increase in value before falling
                 # off to 0.  That often indicates a helpful
                 # lambda level.  Unfortunately, the plot doesn't
                 # Show the values of lambda

# The glmnet package also has generally more helpful function
# that uses k-fold cross validation to choose the best lambda
#
# We can give it a set of lambdas but the function 
# does pretty well at figuring out the range to test
# on its own.
ridge_cv = cv.glmnet(X, y, alpha = 0, nfolds = 10)
ridge_cv$lambda.min   # Very close to the one we selected earlier

# Plot cross-validation error vs. lambda
plot(ridge_cv)

# Now, predict with it 
predCVRidge = predict(ridge_cv, X, s="lambda.min")
rmseCVRidge = sqrt(mean((predCVRidge - y)^2))
rmseCVRidge

# If you want to specify a list of lambdas, you can do it 
# with
lambdas_to_try = seq(0, 10, .1)
ridge_cv = cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try, nfolds = 10)
ridge_cv$lambda.min
plot(ridge_cv)


#################################################################
# Now, let's look at lasso. The nice thing is that the alpha 
# parameter allows us to select and even blend between ridge
# and lasso.  Alpha = 1 gives us LASSO
#################################################################

lassoFit = glmnet(X, y, alpha=1)
print(lassoFit)
plot(lassoFit)   # You will notice the difference here in the 
                 # Plot is that many of the coefficients get set
                 # set exactly to zero ... i.e. it selects them out

lasso_cv = cv.glmnet(X, y, alpha = 1, nfolds = 10)
lasso_cv$lambda.min

# Plot cross-validation error vs. lambda
plot(lasso_cv)

predCVLasso = predict(lasso_cv, X, s="lambda.min")
rmseCVLasso = sqrt(mean((predCVLasso - y)^2))

rmseCVLasso
rmseCVRidge
rmse






