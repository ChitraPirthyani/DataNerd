
library(car)
library(corrplot)
library(ggplot2)

#############################################################################################
# A useful funciton for visualizing the first two principal components
#############################################################################################

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

#############################################################################################
# First Example
#
# Two x-variables that are highly correlated and one y-variable
#############################################################################################

# Make a simple correlated dataset
x1 = rnorm(100)
x2 = 2 * x1 + rnorm(100, 0, .7)

# Make a data.frame out of it and explore it
d = data.frame(x1, x2)
cov(d)
plot(d, pch=16, col="red", xlim=c(-6, 6), ylim=c(-6, 6))

# Transform the variables and plot
z1 = .41*x1 + .91*x2
z2 = -.91*x1 + .41*x2
dTrans = data.frame(z1, z2)
plot(dTrans, pch=16, col="red", xlim=c(-6, 6), ylim=c(-6, 6))

# Check out the correlation of the data
corM = cor(d)
print(corM)

# Check out the covariance of the data
covM = cov(d)
print(covM)

# Now, compute the principal components and investigate
p = prcomp(d)
print(p)
summary(p)

# Grab the matrix of eigenvectors and multiply the data by it and plot!
R = as.matrix(p$rotation)
R
dPrin = as.matrix(d) %*% R
plot(dPrin, xlim=c(-6, 6), ylim=c(-6, 6), col="red", pch=16)

#############################################################################################
# Second Example
#
# Simple eigenvalue eigenvector calculation
#############################################################################################

# Create a simple symmetric covariance matrix
covMat = matrix(c(2, -1, -1, 2), nrow=2, ncol=2)

# Compute its eigenvalues/vectors
eigen(covMat)

# Build a dataset with our simple covariance matrix
library(MASS)
xVars = mvrnorm(1000,rep(0,2),Sigma=covMat)
plot(xVars, pch=16, cex=.5, col="red", xlim=c(-5, 5), ylim=c(-5, 5))

# Compute the principal components
prcomp(xVars)   # Note that there is a small difference in the
                # covariance matrix from what we built it as
                # This is normal given that we are working with
                # a sample, not the population

#############################################################################################
# Second Example
#
# Low dimensional data hiding in many variables, significance for regression
#############################################################################################

# Build a dataset that has essentially two underlying "factors" A and B
A = rnorm(100, 0, 1)
B = rnorm(100, 0, 1)

# We use A and B to build 6 x-variables.  The first three are correlated to A and the second three to B
x1 = 2.5 * A + rnorm(100, 0, .1)
x2 = 1.3 * A + rnorm(100, 0, .1)
x3 = -2.5 * A + rnorm(100, 0, .1)
x4 = 5.6 * B + rnorm(100, 0, .1)
x5 = 2.3 * B + rnorm(100, 0, .1)
x6 = -4.1 * B + rnorm(100, 0, .1)

# Now build a dependent y variable from A and B
y = 5 * A - 3 * B

# Build a data frame
d = data.frame(y, x1, x2, x3, x4, x5, x6)

# Take a look at the summary statistics
c = cor(d)
print(round(c, 2))
library(corrplot)
corrplot(c)
plot(x1, x2)

plot(d)

# Try an initial full-fit
fit = lm(y ~ ., data=d)
summary(fit)

# Take a look at the variance inflation factors

vif(fit)

# Compute principal components and plot (gives a scree plot of the eigenvalues)
pComp = prcomp(d[,2:7])
summary(pComp)

# Gives a scree plot 
plot(pComp)

# The PCA_Plot funciton defined above plots the first two principal components by 
# Placing the labels x1 - x6 in the plane according to their "loadings" on each component
# The loadings are how much each x contributes to each component
PCA_Plot(pComp)

# Now, grab the matrix of eigenvectors (the "rotation" part of the principal component analysis)
# And multiply the x-data by the rotation matrix (notice ... on the right)
R = as.matrix(pComp$rotation)
dTmp = as.matrix(d[2:7])
dPrin = dTmp %*% R
head(dPrin)

# Now let's plot it ... a LOT simpler
plot(dPrin)

# Now we try creating a new data.frame with this rotated data and run a regression ... compare!
d2 = data.frame(dPrin)
d2$y = y
plot(d2)

fit2 = lm(y ~ ., data=d2)
summary(fit2)
summary(fit)    # Compare to the original

# Note how many significant variables achieve the same R^2!

###############################################################
# Advanced: Only for those who want to dig a little deeper
#
# Making some datasets with specific covariance matrices
###############################################################

library(MASS)
library(clusterGeneration)

covMat = matrix(c(1, 0, 0, 1), nrow=2, ncol=2)
xVars = mvrnorm(1000,rep(0,2),Sigma=covMat)
plot(xVars, pch=16, cex=.5, col="red", xlim=c(-5, 5), ylim=c(-5, 5))

covMat = matrix(c(.3333, 0, 0, 2), nrow=2, ncol=2)
xVars = mvrnorm(1000,rep(0,2),Sigma=covMat)
plot(xVars, pch=16, cex=.5, col="red", xlim=c(-5, 5), ylim=c(-5, 5))

covMat = matrix(c(5, -1, -1, 3), nrow=2, ncol=2)
xVars = mvrnorm(1000,rep(0,2),Sigma=covMat)
plot(xVars, pch=16, cex=.5, col="red", xlim=c(-7, 7), ylim=c(-7, 7), xlab="x1", ylab="x2")

