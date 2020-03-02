---
title: "Avocado Price Trend"

---

#1. Firstly load csv file 

read.csv("avocado_chicago.csv")
myd=read.csv("avocado_chicago.csv")
summary(myd)


#2. Simple Data Exploratory
#2.1 check missing values, TRUE means NA

is.na(myd)

#2.2 check data type

str(myd) 

#2.3 Renaming column 4046,4225,4770 for better understanding

colnames(myd)[5] <- "SmallMedium.Volume"
colnames(myd)[6] <- "Large.Volume"
colnames(myd)[7] <- "XL.Volume"


#3. Create variables

Avgprice=myd$AveragePrice
Tvolum=myd$Total.Volume
Svolum=myd$SmallMedium.Volume
Lvolum=myd$Large.Volume
LXvolum=myd$XL.Volume
Tbags=myd$Total.Bags
Sbags=myd$Small.Bags
Lbags=myd$Large.Bags
XLbags=myd$XLarge.Bags
year=myd$year
type=myd$type


#4. Creating Dummy variables for Type column

numtype= (type=="organic")*1


#5. Anlysis major features
#5.1 AveragePrice

library(psych)
describe(Avgprice)
hist(Avgprice, breaks=30, border=F,xlab="Average Price", col="light blue", main="distribution of average price")
box()

#5.2 TotalVolume

describe(Tvolum)
hist(Tvolum, breaks=30, border=F,xlab="Total Volume", col="light blue", main="distribution of total volume")
box()
boxplot(Tvolum, main = "Boxplot of Avocado Total Volume with outliers", cex.main=1, ylab = "count",cex.axis=1, xlab="Total Volume", cex.lab=1, horizontal = F, outcol = "orange", col= "orange") 

#5.3 Type

boxplot(Avgprice~numtype,data=myd, col="light blue",main="Boxplot of averageprice by type", 
        xlab="Type of avocado", ylab="averageprice Per type")


#6. Simple Multipul Linear Regression Analysis
#6.1 reset dataset frame

newmyd= data.frame(Avgprice,Tvolum,Svolum,Lvolum,LXvolum,Tbags,Sbags,Lbags,XLbags,numtype)

#6.2 correlation 

cor(newmyd, method = "pearson")

#6.3 Full Model

fit= lm(Avgprice~Tvolum+Svolum+Lvolum+LXvolum+Tbags+Sbags+Lbags+XLbags+numtype, data=newmyd)
summary(fit) 

#6.4 Backward selection: M1

newmyd <- na.omit(newmyd)
step(fit, direction = "backward")

# M1 model
M1= lm(formula = Avgprice ~ Tvolum + Lvolum + LXvolum + Tbags + Sbags + numtype, data = newmyd)
summary(M1)

#6.5 Forward selection: M2

# start with Base model that has no variables:
Base = lm(Avgprice~1, data=newmyd)
step(Base, scope = list( upper=fit, lower=~1 ), direction = "forward")
# M2 model
M2= lm(formula = Avgprice ~ Lvolum + numtype + Svolum + Lbags, data = newmyd)
summary(M2)

#6.6 Stepwise selection: M3

step(Base, scope = list( upper=fit, lower=~1 ), direction = "both", trace=FALSE)
# M3 model
M3= lm(formula = Avgprice ~ Lvolum + numtype + Svolum + Lbags, data = newmyd)
summary(M3)

#6.7 Model fitting Analysis 
#6.7.1  

library(car)
vif(fit)
vif(M1)
vif(M2)
vif(M3)
# select M2

#6.7.2 residuals plots

#residual vs fitted 
plot( fitted(M2), rstandard(M2), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')
#normal probability plot of residuals
qqnorm(rstandard(M2))
qqline(rstandard(M2), col = 2)

#7. Interaction Linear Regression Analysis
#7.1 Full Interaction Model

full1 = lm(Avgprice~(Tvolum+Svolum+Lvolum+LXvolum+Tbags+Sbags+Lbags+XLbags+numtype)^2, data=newmyd)
summary(full1) 

#7.2 Backward selection

newmyd <- na.omit(newmyd)
step(full1, direction = "backward")

#7.3 Forward selection

Base = lm(Avgprice~1, data=newmyd)
step(Base, scope = list( upper=full1, lower=~1 ), direction = "forward")

#7.3 Stepwise selection

step(Base, scope = list( upper=full1, lower=~1 ), direction = "both", trace=FALSE)

#7.3 Final Interaction Model: M4

# both Forward and Stepwise give the same model formula
formula = Avgprice ~ Lvolum + numtype + Svolum + Lbags + numtype:Svolum + 
    Lvolum:Svolum + Lvolum:numtype + Lvolum:Lbags + numtype:Lbags

# M4
M4= lm(formula, data = newmyd)
summary(M4)

#7.4 Analysis of M4
#7.4.1 residuals plots

#residual vs fitted 
plot( fitted(M4), rstandard(M4), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')
qqnorm(rstandard(M4))

#7.4.2 influential 

# compute influential points statistics
influence.measures(M4)
# print out only observations that may be influential 
summary(influence.measures(M4))
# plot of deleted studentized residuals vs hat values
plot(rstudent(M4)~hatvalues(M4)) 

#7.4.3 Compute the standardized coefficients

# find the most important variable
library(QuantPsyc)
lm.beta(M4) 

#7.4.4 run model diagnostics

vif(M4)# multicollinearity

#8. validation
#8.1 5 fold cross-validation

# 5 fold cross-validation
library(DAAG)
results_m4=cv.lm(data=newmyd, M4, m=5, printit=T)
head(results_m4)

#Create fitted values using test.myd data
y_pred <- results_m4[,"cvpred"]
y_obs<-newmyd[,"Avgprice"]

# Compute mean percentage absolute error
mape_cv<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_cv


#7.3 Final Interaction Model: M5

# both Forward and Stepwise give the same model formula
formula = Avgprice ~ Lvolum + numtype + Lbags + numtype:Svolum + 
    Lvolum:Svolum + Lvolum:numtype + Lvolum:Lbags
# M5
M5= lm(formula, data = newmyd)
summary(M5)

anova(M5)


#7.4.1 residuals plots

#residual vs fitted 
plot( fitted(M5), rstandard(M5), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')
qqnorm(rstandard(M5))
qqline(rstandard(M5), col = 2)


#6.7.3 Separate training data and test data sets

# Splitting dataset to 70% training and 30% testing 
split <-sample(nrow(newmyd), floor(nrow(newmyd)*0.7))
train_data <-newmyd[split,]
test_data  <-newmyd[-split,]


# fit model using training set: M4


#Create fitted values using test.myd data
formula = Avgprice ~ Lvolum + numtype + Svolum + Lbags + numtype:Svolum + 
    Lvolum:Svolum + Lvolum:numtype + Lvolum:Lbags + numtype:Lbags
# M4
M4= lm(formula, data = train_data)
summary(M4)

y_pred <- predict.glm(M4, train_data)
y_obs<-test_data[,"Avgprice"]

# Compute RMSE of prediction errors
rmse_m1 <- sqrt((y_obs - y_pred)%*%(y_obs - y_pred)/nrow(test_data))
rmse_m1

# Compute mean absolute error
mae_m1<-mean(abs(y_obs - y_pred))
mae_m1

# Compute mean percentage absolute error
mape_m1<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m1


#7.3 Final Interaction Model: M5

# both Forward and Stepwise give the same model formula
formula = Avgprice ~ Lvolum + numtype + Lbags + numtype:Svolum + Lvolum:Svolum + Lvolum:numtype + Lvolum:Lbags
# M4
M5= lm(formula, data = train_data)
summary(M5)

y_pred <- predict.glm(M5, test_data)
y_obs<-test_data[,"Avgprice"]

# Compute RMSE of prediction errors
rmse_m2 <- sqrt((y_obs - y_pred)%*%(y_obs - y_pred)/nrow(test_data))
rmse_m2

# Compute mean absolute error
mae_m2<-mean(abs(y_obs - y_pred))
mae_m2

# Compute mean percentage absolute error
mape_m2<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m2

