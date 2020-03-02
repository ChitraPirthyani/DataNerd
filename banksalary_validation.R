
myd <- read.table("bankdata.txt", header=T)
attach(myd)
# create dummy variables
myd$dsr2=(sexrace==2)*1
myd$dsr3=(sexrace==3)*1
myd$dsr4=(sexrace==4)*1

myd$djc2=(jobcat==2)*1;
myd$djc3=(jobcat==3)*1;
myd$djc4=(jobcat==4)*1;
myd$djc5=(jobcat==5)*1;
myd$djc6=(jobcat==6)*1;
myd$djc7=(jobcat==7)*1;
detach(myd)

#drop id variable (first column);
myd=myd[,-1]

#compute log increase;
myd$lngrowth=log(myd$salnow/myd$salbeg)

# Selected model 
fit = lm(lngrowth~age+work+djc3+djc4, data=myd)
summary(fit)


# Create training and testing set
# split samples (75% for training and 25% for testing)
select.myd <- sample(1:nrow(myd), 0.75*nrow(myd))
train.myd <- myd[select.myd,]  #Selecting 75% of the data for training purpose
test.myd <- myd[-select.myd,]  #Selecting 25% (remaining) of the data for testing purpose

# fit model using training set
# Model 1: 
# Selected model
fit = lm(lngrowth~age+work+djc3+djc4, data=train.myd)
summary(fit)
#Create fitted values using test.myd data
y_pred <- predict.glm(fit, test.myd)
y_obs<-test.myd[,"lngrowth"]
# Compute RMSE of prediction errors
rmse_m1 <- sqrt((y_obs - y_pred)%*%(y_obs - y_pred)/nrow(test.myd))
rmse_m1
# Compute mean absolute error
mae_m1<-mean(abs(y_obs - y_pred))
mae_m1

# Compute mean percentage absolute error
mape_m1<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m1

# compute cross-validated R^2_pred
r2_pred = cor(cbind(y_obs,y_pred))**2
r2_train = summary(fit)$r.squared
diffr2_m1=abs(r2_train-r2_pred)
#print difference of cross-validate R2 and R2
diffr2_m1[1,2]

#Model 2: Includes variables age djc2 and djc4 
fit = lm(lngrowth ~ age + djc2 + djc4, na.action=na.omit, data=train.myd)
summary(fit)
#Create fitted values using test.myd data
y_pred <- predict.glm(fit, test.myd)
y_obs<-test.myd[,"lngrowth"]
# Compute RMSE of prediction errors
rmse_m2 <- sqrt((y_obs - y_pred)%*%(y_obs - y_pred)/nrow(test.myd))
rmse_m2
# Compute mean absolute error
mae_m2<-mean(abs(y_obs - y_pred))
mae_m2

# Compute mean percentage absolute error
mape_m2<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m2

# compute compute cross-validated R^2_pred
r2_pred = cor(cbind(y_obs,y_pred))**2
r2_train = summary(fit)$r.squared
diffr2_m2=abs(r2_train-r2_pred)
#print difference of cross-validate R2 and R2
diffr2_m2[1,2]

# apply k-fold crossvalidation procedure
# m=5 indicates that this is a 5-fold cross-validation
# The mean squared error for the 5 folds is displayed 
# under the MS value at the bottom of the output.

library(DAAG)
fit <- lm(lngrowth~time+age+work+djc3+djc4+djc6, data=myd) 
results = cv.lm(data=myd, form.lm=fit, m= 10, plotit = F) # 5-fold cross-validation 
head(results)
# Root Mean Squared Error (RMSE)  value
rmse_cv <- sqrt((results$lngrowth - results$cvpred)%*%(results$lngrowth - results$cvpred)/nrow(results))
rmse_cv
# Another way to compute k-fold crossvalidation
library(bootstrap)
# define functions 
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
# matrix of xvariables
X <- as.matrix(myd[c("time","age","work","djc3","djc4","djc6")])
# response variable
y <- as.matrix(myd[c("lngrowth")]) 

results <- crossval(X,y,theta.fit,theta.predict,ngroup=5)
# Root Mean Squared Error (RMSE)  value
# results$cv.fit contains the predicted values on testing sets.
rmse_cv <- mean(sqrt((myd$lngrowth - results$cv.fit)%*%(myd$lngrowth - results$cv.fit)))
rmse_cv


cor(y, fit$fitted.values)**2 # Model R2 
cor(y,results$cv.fit)**2 # cross-validated R2
#difference:
abs(cor(y,results$cv.fit)**2-cor(y, fit$fitted.values)**2)
