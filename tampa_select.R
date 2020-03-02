# import data for tampa.txt

myd=read.table("tampa4.txt", header=T)
myd[1,]
d1= (myd$area=="CWDVILL")*1
d2 = (myd$area=="TNC")*1
d3 = (myd$area=="DAVISLE")*1
price = myd$price
value = myd$value
improv = myd$improv

#remove qualitative variable area and include dummy variables
myd = cbind(myd[,1:3], d1, d2, d3)
myd[1,]

# scatterplot
pairs(~price+value+improv+d1+d2+d3, main="Simple Scatterplot Matrix")

# Model 1 (M1): Simple linear model
fit1 = lm (price ~ (value+improv+d1+d2+d3), data=myd)
summary(fit1)

#residual plots
#residuals vs fitted values plot
plot( fitted(fit1), rstandard(fit1), main="Predicted vs residuals plot for Model M1")
abline(a=0, b=0, col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit1))
qqline(rstandard(fit1), col = 2)

#Model 2: fit model with main effects and interaction terms only.
full = lm (price ~ (value+improv+ d1+d2+d3)^2, data=myd)
summary(full)

# define matrix of covariates in full model
X.var = model.matrix(full)[,-1]

# leaps package includes model selection procedures;
library(leaps)

#backward selection
bward=step(full, direction = "backward", trace=TRUE )

#forward selection
step(full, direction = "forward", trace=TRUE )

# Model 3: Fit selected model from backward selection 
fit2 = lm(price ~ value + improv + d1 + d2 + d3 + value:improv + value:d1 + value:d2 + value:d3 + improv:d1, data=myd)
summary(fit2) # test significance of individual variables
#residuals vs fitted values plot
plot( fitted(fit2), rstandard(fit2), main="Predicted vs residuals plot for Model M3")
abline(a=0, b=0, col='red')
#normal probability plot of residuals
qqnorm(rstandard(fit2))
qqline(rstandard(fit2), col = 2)


# Model 3a: Fit selected model from backward selection (after removing additional non significant variables)
fit3 =lm(price ~ value +improv+ d2+value:improv+value:d1+value:d2+value:d3 +improv:d1, data=myd)
summary(fit3)
#residuals vs fitted values plot
plot( fitted(fit3), rstandard(fit3), main="Predicted vs residuals plot for Model M3a")
abline(a=0, b=0, col='red')
#normal probability plot of residuals
qqnorm(rstandard(fit3))
qqline(rstandard(fit3), col = 2)

#note that we can use key shortcuts in R 
# E.G. x1 *x2 indicates main terms + interaction term: x1+x2+x1*x2
fit3 =lm(price ~ value*improv+ d2+value:d1+ value:d2+value:d3 +improv:d1, data=myd)
summary(fit3)
fit3 = lm (price ~ (value + improv+ d1+d2+d3)^2 -d1-d3-d2:improv -d3:improv, data=myd)
summary(fit3)

# Model 4: Fit selected model from forward selection 
fit4 = lm(price ~ value + improv + d1 + d2 + d3 + value:improv + value:d1 + value:d2 + value:d3 + improv:d1 + improv:d2 + improv:d3, data=myd)
summary(fit4) # test significance of individual variables
#residuals vs fitted values plot
plot( fitted(fit4), rstandard(fit4), main="Predicted vs residuals plot for Model M4")
abline(a=0, b=0, col='red')
#normal probability plot of residuals
qqnorm(rstandard(fit4))
qqline(rstandard(fit4), col = 2)

#PREDICTIONS
# Estimate the average price for houses  with 
#	value=$60,000, improvement = $70,000 in TAMPA PALMS, (default base)
#	value=$60,000, improvement = $70,000 in Carrollwood Village, 
#	value=$60,000, improvement = $70,000 in Town & Country, 
#	value=$60,000, improvement = $70,000 in Davis Isle.

# create the new dataset containing values for predictions
new=data.frame(value=c(rep(60000,4)), improv=c(rep(70000,4)), d1=c(1,0,0,0), d2=c(0,1,0,0),d3= c(0,0,1,0))
new


# compute average response value and confidence interval
predict(fit3, new, se.fit = T, interval="confidence",level=0.95)

#compute predictions and prediction intervals

predict(fit3, new, se.fit = T, interval="prediction", level=0.95)

