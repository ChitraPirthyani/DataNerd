# RIDGE REGRESSION

# Model is E(Y) = 0 + 1 X1 + 1 X2 + e   with e~N(0,1)
# Three variables are measured: x1,x2,x3.  
#  x1 and x1 are U(0,1); x3=10 * X1 + unif(0,1).  
#   This causes corr(X1,X3)=sqrt(100/101)=0.995.
# We will fit OLS and ridge regressions to these data, 
#  use the data to select the "best" constant to add, 
#  and then evaluate the two regressions on a new test set.

# Ridge regression function, ridge.lm(), is on MASS package

library(MASS)

# Generating the data

set.seed(558562316)
N = 20      # Sample size

x1 = runif(N)
x2 = runif(N)
x3 = runif(N)
x3c = 10*x1 + x3 # New variable
ep = rnorm(N)
y = x1 + x2 + ep 

ds = data.frame(y, x1, x2, x3, x3c)
plot(ds)

# OLS fit of 3-variable model using independent x3
ols = lm(y ~ x1 + x2 + x3)
summary(ols)

# OLS fit of 3-variable model using correlated x3.
olsc = lm(y ~ x1 + x2 + x3c)
summary(olsc)

# Ridge regression using correlated variables
ridgec = lm.ridge (y ~ x1+x2+x3c, lambda = seq(0, .1, .001))
plot(ridgec)
select(ridgec)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec = lm.ridge (y ~ x1+x2+x3c, lambda = seq(0, 1, .1))
plot(ridgec)
select(ridgec)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec = lm.ridge (y ~ x1+x2+x3c, lambda = seq(0, 10, .01))
plot(ridgec)
select(ridgec)

# Final model uses lambda=4.
ridge.final = lm.ridge (y ~ x1+x2+x3c, lambda = 4)
ridge.final

# Compute the rse on the dataset.  Our regression has 4 parameters
# Including the intercept, so the degrees of freedom = #dataRows - 4 = 20-4 = 16
head(ds)
p.ols = predict(ols,newdata=ds[, c(2, 3, 4)])   # y ~ X1 + X2 + X3
p.olsc = predict(olsc,newdata=ds[, c(2, 3, 5)]) # y ~ X1 + X2 + X3c
p.ridge = coef(ridge.final)[1] + coef(ridge.final)[2]*ds$x1 + 
  coef(ridge.final)[3]*ds$x2 + coef(ridge.final)[4]*ds$x3c

# Note that "coef(ridge.final)" lists all the coefficients
# including the intercept.  So this length is #var + 1
length(coef(ridge.final))

dof = length(y) - length(coef(ridge.final))
rse.ols = sqrt(sum((y - p.ols)^2) / dof)
rse.olsc = sqrt(sum((y - p.olsc)^2) / dof)
rse.ridge = sqrt(sum((y - p.ridge)^2) / dof) 

rse.ols
rse.olsc
rse.ridge   # Doesn't seem to be doing much for us

plot(olsc, pch=16)

# build a predicted vs residual plot
resid.ridge = y - p.ridge
plot(p.ridge, resid.ridge, pch=16)

# Get the equivalent normal qq-plot
qqnorm(resid.ridge)
qqline(resid.ridge, col="red")

# Create test data and compute predicted values for OLS and ridge.
#  There's no predict() method for "ridgelm" objects
test = expand.grid(x1 = seq(.05,.95,.1), x2 = seq(.05,.95,.1), x3=seq(.05,.95,.1))
mu = test$x1 + test$x2
test$x3c = 10*test$x1 + test$x3
pred.ols = predict(ols,newdata=test)   # y ~ X1 + X2 + X3
pred.olsc = predict(olsc,newdata=test) # y ~ X1 + X2 + X3c
pred.ridge = coef(ridge.final)[1] + coef(ridge.final)[2]*test[,1] + 
  coef(ridge.final)[3]*test[,2] + coef(ridge.final)[4]*test[,4]

coef(ridge.final)

# Compute the mean square prediction error
pdof = length(mu) - length(coef(ridge.final))
RSPE.ols = sqrt(sum((pred.ols - mu)^2)/dof)
RSPE.olsc = sqrt(sum((pred.olsc - mu)^2)/dof)
RSPE.ridge = sqrt(sum((pred.ridge - mu)^2)/dof)

# Print them out
RSPE.ols
RSPE.olsc
RSPE.ridge  # A bit better here

# Notice that we do not get much in the way of diagnostics out 
# of lm.ridge ... sigh

# There is another package called "penalized" which implements both lasso (L1) and ridge (L2)
# regression, but its computation is more complicated as an optimization 
# technique and its output is not comparable.
# 
# I provide this initial example to get you started if you want to explore
# this package in more depth.
install.packages("penalized")
library(penalized)
pfit = penalized(y ~ x1 + x2 + x3c, lambda2 = 4)
summary(pfit)
print(pfit)
coefficients(pfit, "all")
penalty(pfit)
head(residuals(pfit))

plot(fitted(pfit), residuals(pfit))

