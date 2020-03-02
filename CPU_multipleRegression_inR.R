# Example of regression analysis in R using CPU data

mydata = read.table("cpudat.txt")
# create variables
time = mydata[,1]
line = mydata[,2]
step =mydata[,3]
device =mydata[,4]

linet=line/1000

# To create a scatterplot use basic function plot(x, y), 
# where x and y are numeric vectors denoting the (x,y) points to plot.
plot(linet,time, main="Scatterplot of linet vs CPU time")

# To compute correlations use the cor( ) function 
# A simplified format is cor(x, method="pearson") where 
# x is the dataframe
# method = pearson specifies the type of correlation.

# computes pairwise correlation values on the entire dataset
cor(da, method="pearson") 
# computes pairwise correlation values for the two variables
cor(linet, time)

# Multiple Linear Regression Example
fit <- lm(time ~ linet+step+device, data=mydata)
summary(fit) # show results
#compute standardized coefficients
library(QuantPsyc)
lm.beta(fit)

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals

# To add regression line y = a+b*x, use abline(lm(y~x), col="red") 
# after plot(x,y) statement.
plot(linet,time, main="Scatterplot of linet vs CPU time")
abline(lm(time~linet), col="red")
