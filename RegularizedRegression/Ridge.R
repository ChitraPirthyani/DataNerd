# RIDGE REGRESSION

# We pre-compute a data-set with a relationship that we know
# Model is E(Y) = 0 + X1 + X2 + e   with e~N(0,1)
#
# The data contain three variables are measured: x1,x2,x3.  
#  x1 and x1 are U(0,1); x3=10 * X1 + unif(0,1).  
#   This causes corr(X1,X3)=sqrt(100/101)=0.995.
#
# We will fit OLS and ridge regressions to these data, 
#  use the data to select the "best" constant to add, 
#  and then evaluate the two regressions on a new test set.

# Ridge regression function, ridge.lm(), is on MASS package

library(MASS)

#######################################################################
# Generating the data
#######################################################################

set.seed(558562316)   # Set a seed so that we get the same results every time
N = 20      # Sample size

x1 = runif(N)
x2 = runif(N)
x3 = 10 * x1 + runif(N)
rError = rnorm(N)
y = x1 + x2 + rError 

ds = data.frame(y, x1, x2, x3)
plot(ds)

# Now, let's make a test set with the same model
N = 10

x1 = runif(N)
x2 = runif(N)
x3 = 10 * x1 + runif(N)
rError = rnorm(N)
y = x1 + x2 + rError 

dsTest = data.frame(y, x1, x2, x3)

############################################################################
# Now, let's try ordinary regression
############################################################################

# OLS fit of 3-variable model using independent x3
ols = lm(y ~ x1 + x2 + x3, data = ds)
summary(ols)   # Only one significant contributor.  Estimates not terribly large
rmseTrain = sqrt(sum(ols$residuals^2)/nrow(ds))
rmseTrain

# Notice how much greater the rmse is on the test set compared to the 
# training set.
p.ols = predict(ols, dsTest)
rmsePred = sqrt(mean((p.ols-dsTest$y)^2))
rmsePred   # A lot larger

# Now, let's try ridge regression
# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec = lm.ridge (y ~ x1+x2+x3, data = dsTest, lambda = seq(0, 1, .1))
plot(ridgec)
select(ridgec)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec = lm.ridge (y ~ x1+x2+x3, data = dsTest, lambda = seq(0, 20, .01))
plot(ridgec)
select(ridgec)

# Final model uses lambda=4.
ridge.final = lm.ridge (y ~ x1+x2+x3, lambda = 13.56)
ridge.final

# Compute the rse on the dataset.  Our regression has 4 parameters
# Including the intercept, so the degrees of freedom = #dataRows - 4 = 20-4 = 16
head(ds)
p.ridge = coef(ridge.final)[1] + coef(ridge.final)[2]*dsTest$x1 + 
  coef(ridge.final)[3]*dsTest$x2 + coef(ridge.final)[4]*dsTest$x3
rmseRidge = sqrt(mean((p.ridge - dsTest$y)^2))

rmseRidge    # Significant improvement
rmsePred

#######################################################################
# Now, if we want to compare the residual standard error we need the 
# degrees of freedom
#
# Note that "coef(ridge.final)" lists all the coefficients
# including the intercept.  So this length is #var + 1
#######################################################################

length(coef(ridge.final))

dof = length(y) - length(coef(ols))
dofRidge = length(y) - length(coef(ridge.final))
rse.ols = sqrt(sum((y - p.ols)^2) / dof)
rse.ridge = sqrt(sum((y - p.ridge)^2) / dofRidge) 

rse.ols
rse.ridge   # Fairly significant improvement

plot(olsc, pch=16)

# build a predicted vs residual plot
resid.ridge = y - p.ridge
plot(p.ridge, resid.ridge, pch=16)

# Get the equivalent normal qq-plot
qqnorm(resid.ridge)
qqline(resid.ridge, col="red")   # Notice a little asymetry here ... a symptom of the bias

# Notice that we do not get much in the way of diagnostics out 
# of lm.ridge ... sigh
#
# There is another package called "penalized" which implements both lasso (L1) and ridge (L2)
# regression, but its computation is more complicated as an optimization 
# technique and its output is not comparable.
# 
# I provide this initial example to get you started if you want to explore
# this package in more depth.
install.packages("penalized")
library(penalized)
pfit = penalized(y ~ x1 + x2 + x3, lambda2 = 4)
summary(pfit)
print(pfit)
coefficients(pfit, "all")
penalty(pfit)
head(residuals(pfit))

plot(fitted(pfit), residuals(pfit))

