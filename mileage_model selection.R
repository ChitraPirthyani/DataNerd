# import dataset mileage.txt. 
# na.string = define characters used for missing values. Dataset has 2 missing values 
# for torque defined by '.'.
myd = read.table("mileage.txt", header=T, na.string='.')
# create dummy variable
transm = (myd$trans=="M")*1
# attach it to myd dataset
myd=cbind(myd, transm)
#drop trans variable from myd
myd$trans=NULL

#compute correlations (except for trans)
cor(myd, use="pairwise.complete.obs")

#create scatterplot matrix for quantitative variables
pairs(~myd$miles+myd$displ+myd$hpw+myd$torque+myd$compr+myd$rearaxle+myd$carb+ myd$speeds+myd$length+myd$width+myd$weight+myd$transm, main="Simple Scatterplot Matrix") 

#correlograms
library(corrgram)
corrgram(myd[, -1], order=F, lower.panel=panel.shade,
         upper.panel=panel.pts, text.panel=panel.txt)

#fit full model
full = lm(miles~displ+hpw+torque+compr+rearaxle+carb+ speeds+length+width+weight+transm, data=myd)
summary(full)

#compute VIF scores using vif() function in CAR package (for collinearity analysis)
library(car)
vif(full)

# Selected model n.1 
fit1 = lm(miles ~ rearaxle + weight, data=myd)
summary(fit1)


#residual plots
#residuals vs fitted values plot
plot( fitted(fit1), rstandard(fit1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit1))
qqline(rstandard(select1), col = 2)

# compute influential points statistics
influence.measures(fit1)
# print out only observations that may be influential 
summary(influence.measures(fit1))
# plot of deleted studentized residuals vs hat values
plot(rstudent(fit1)~hatvalues(fit1))



# import dataset mileage.txt   
# na.string = define characters used for missing values. Dataset has 2 missing values 
# for torque defined by '.'.
myd = read.table("mileage.txt", header=T, na.string='.')
# create dummy variable
transm = (myd$trans=="M")*1
# attach it to myd dataset
myd=cbind(myd, transm)
#drop trans variable from myd
myd$trans=NULL

#compute correlations (except for trans)
cor(myd, use="pairwise.complete.obs")

#create scatterplot matrix for quantitative variables
#pairs(~myd$miles+myd$displ+myd$hpw+myd$torque+myd$compr+myd$rearaxle+myd$carb+ myd$speeds+myd$length+myd$width+myd$weight+myd$transm, main="Simple Scatterplot Matrix") 

#fit full model
full = lm(miles~displ+hpw+torque+compr+rearaxle+carb+ speeds+length+width+weight+transm, data=myd)
summary(full)

#compute VIF scores using vif() function in CAR package (for collinearity analysis)
library(car)
vif(full)

 

#Apply model selection methods using functions in library leaps
# load library leaps
library(leaps)

#omit missing values. This is necessary to use leaps function.
newmyd <- na.omit(myd)

# best subset model selection according to Cp statistics
# x= A matrix of predictors
# y = A response vector
# names = vector of names for columns of x
leapmodels=leaps(x=newmyd[,3:13], y=newmyd[,2], names=names(myd)[3:13], method="Cp")
mat=cbind(leapmodels$size,leapmodels$which, leapmodels$Cp )
#display results in increasing order of Cp.
# first element is # of vars in the model
# values of 1 in row indicates selected variables
# last column shows Cp values
mat[order(mat[,dim(mat)[2]]),]
#plot results on a scatterplot. 
# Best model is selected by Cp value close to number of parameters.
plot(leapmodels$size, leapmodels$Cp)
abline(0,1)

# best subset model selection according to Adj-R2 statistics
leapmodels=leaps(x=newmyd[,3:13], y=newmyd[,2], names=names(newmyd)[3:13], method="adjr2")
mat=cbind(leapmodels$size,leapmodels$which, leapmodels$adjr2)
#display results in increasing order of adjr2
# first element is # of vars in the model
# values of 1 in row indicates selected variables
# last column shows adjr2 values
mat[order(mat[,dim(mat)[2]], decreasing=TRUE),]

#plot results on a scatterplot. 
# Best model is selected by largest adjr2 values.
plot(leapmodels$size, leapmodels$adjr2)
abline(0,1)


#Backward selection
# start with the full model "full" fitted above
#fit full model on dataset with no missing values
full = lm(miles~displ+hpw+torque+compr+rearaxle+carb+ speeds+length+width+weight+transm, data=newmyd)
summary(full)

step(full, direction = "backward")

#Forward selection
# start with Base model that has no variables:
Base = lm(miles~1, data=myd)
# in scope: the value of upper should be a linear model already defined in your code, e.g. the "full" model fitted above. 
# lower defines the simpler model
step(Base, scope = list( upper=full, lower=~1 ), direction = "forward")

# Stepwise selection
step(Base, scope = list( upper=full, lower=~1 ), direction = "both", trace=FALSE)

# Selected model n.1 (backward)
select1 = lm(miles ~ rearaxle + length + weight, data=myd)
summary(select1)
# compute VIF statistics for collinearity
library(car)
vif(select1)

#residual plots
#residuals vs fitted values plot
plot( fitted(select1), rstandard(select1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#normal probability plot of residuals
qqnorm(rstandard(select1))
qqline(rstandard(select1), col = 2)

# Selected model n.1 (stepwise)
select2 = lm(miles ~ displ+compr, data=myd)
summary(select2)
# compute VIF statistics for collinearity
#library(car)
vif(select2)

#residual plots
#residuals vs fitted values plot
plot( fitted(select2), rstandard(select2), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#normal probability plot of residuals
qqnorm(rstandard(select2))
qqline(rstandard(select2), col = 2)


# compute influential points statistics
influence.measures(select1)
# print out only observations that may be influential 
summary(influence.measures(select1))
# plot of deleted studentized residuals vs hat values
plot(rstudent(select1)~hatvalues(select1))


# Selected Model N.2
select2 = lm(miles ~ displ, data=myd)
summary(select2)

#residual plots
#residuals vs fitted values plot
plot( fitted(select2), rstandard(select2), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#normal probability plot of residuals
qqnorm(rstandard(select2))
qqline(rstandard(select2), col = 2)


# compute influential points statistics
influence.measures(select2)
# print out only observations that may be influential 
summary(influence.measures(select2))
# plot of deleted studentized residuals vs hat values
plot(rstudent(select2)~hatvalues(select2)) 


# split samples (75% for training and 25% for testing)
select.myd <- sample(1:nrow(myd), 0.75*nrow(myd))
#Selecting 75% of the data for training purpose
train.myd <- myd[select.myd,]
#Selecting 25% (remaining) of the data for testing
test.myd <- myd[-select.myd,]


