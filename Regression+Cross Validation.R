#PROBLEM1


#import txt data in R
dat <- read.table("Bankingfull.txt",header=T, sep="\t")

#defining variables
balance = dat$Balance
age = dat$Age
education = dat$Education
income = dat$Income
home = dat$HomeVal
wealth = dat$Wealth

plot(age,balance, main="Scatterplot between Blanace and Age", xlab="Age", ylab="Balance")
abline(lm(balance~age), col="red")

plot(education,balance, main="Scatterplot between Blanace and Education", xlab="Education", ylab="Balance")
abline(lm(balance~education), col="red")

plot(income,balance, main="Scatterplot between Blanace and Income", xlab="Income", ylab="Balance")
abline(lm(balance~income), col="red")

plot(home,balance, main="Scatterplot between Blanace and HomeVal", xlab="HomeVal", ylab="Balance")
abline(lm(balance~home), col="red")

plot(wealth,balance, main="Scatterplot between Blanace and Wealth", xlab="Wealth", ylab="Balance")
abline(lm(balance~wealth), col="red")



#compute correlations
cor(dat)

#fit full model
M1 = lm(balance~age+education+income+home+wealth, data=dat)
summary(M1)


#create FIV statistics
library(car)
vif(M1)

#refit full model
M2 = lm(balance~age+education+home+wealth, data=dat)
summary(M2)

#refit full model
M3 = lm(balance~age+education+home+income, data=dat)
summary(M3)


#residuals vs fitted values plot
plot(fitted(M2), rstandard(M2), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')


#residuals vs predictors plot
plot(age, rstandard(M2), main="Age vs residuals plot")
abline(a=0, b=0, col='red')

plot(education, rstandard(M2), main="Education vs residuals plot")
abline(a=0,b=0, col='red')


plot(home, rstandard(M2), main="Home vs residuals plot")
abline(a=0,b=0, col='red')

plot(wealth, rstandard(M2), main="Wealth vs residuals plot")
abline(a=0,b=0, col='red')

# normal probability plot of residuals
qqnorm(rstandard(M2))
qqline(rstandard(M2), col = 2)

#compute influential points statistics
influence.measures(M2)
summary(influence.measures(M2))

dfbeta(M2)
covratio(M2)
diffits(M2)
cooks.ditance(M2)

# # plot of deleted studentized residuals vs hat values
plot(rstudent(M2)~hatvalues(M2)) 

# to find the influential point
rstudent(M2)

#standarized
library(QuantPsyc)
lm.beta(M2)


#create the new dataset containing values for predictions
new = data.frame(age=c(34), education=c(13),home=c(140000), wealth=c(160000))

# compute predicted value and prediction interval
predict(M2, new, interval="prediction", level=0.95)

exp(10.74555)
exp(9.407982)
exp(12.08312)




#PROBLEM2


data=read.csv("pgatour2006_small.csv")

#defining variables
money=data$PrizeMoney
driving=data$DrivingAccuracy
gir=data$GIR
putting=data$PuttingAverage
birdie=data$BirdieConversion
putts=data$PuttsPerRound

#problem2.a
plot(driving,money, main="Scatterplot between Prize Money and Driving Accuracy", xlab="Driving Accuracy", ylab="Prize Money")
abline(lm(money~driving), col="red")

plot(gir,money, main="Scatterplot between Prize Money and GIR", xlab="GIR", ylab="Prize Money")
abline(lm(money~gir), col="red")

plot(putting,money, main="Scatterplot between Prize Money and Putting Average", xlab="Putting Average", ylab="Prize Money")
abline(lm(money~putting), col="red")

plot(birdie,money, main="Scatterplot between Prize Money and Birdie Conversion", xlab="Birdie Conversion", ylab="Prize Money")
abline(lm(money~birdie), col="red")

plot(putts,money, main="Scatterplot between Prize Money and Putts Per Roundy", xlab="Putts Per Round", ylab="Prize Money")
abline(lm(money~putts), col="red")

#2.b
hist(money, main="Histogram of Prize Money" , xlab="Prize Money", col="gold")

library(psych)
describe(money)
quantile(money)
ln_prizemoney=log(money) 
ln_prizemoney

hist(ln_prizemoney, col="gold")

library(psych)
describe(ln_prizemoney)

fit <- lm(money~driving+gir+putting+birdie+putts, data=dat)
summary(fit)

fit1 <- lm(ln_prizemoney~driving+gir+putting+birdie+putts, data=dat)
summary(fit1)

fit2 <- lm(ln_prizemoney~gir+putting+birdie+putts, data=dat)
summary(fit2)

fit3 <- lm(ln_prizemoney~gir+birdie+putts, data=dat)
summary(fit3)

cor(ln_prizemoney,gir)
cor(ln_prizemoney,birdie)
cor(ln_prizemoney,putts)


plot(ln_prizemoney~gir)
abline(lm(ln_prizemoney~gir), col="red")


plot(ln_prizemoney~putts)
abline(lm(ln_prizemoney~putts), col="red")


plot(ln_prizemoney~birdie)
abline(lm(ln_prizemoney~birdie), col="red")

#residuals vs fitted values plot
plot(fitted(fit3), rstandard(fit3), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#residuals vs predictors plot
plot(gir, rstandard(fit3), main="GIR vs residuals plot")
abline(a=0, b=0, col='red')

plot(birdie, rstandard(fit3), main="Birdie vs residuals plot")
abline(a=0,b=0, col='red')


plot(putts, rstandard(fit3), main="Putts vs residuals plot")
abline(a=0,b=0, col='red')

# normal probability plot of residuals
qqnorm(rstandard(fit3))
qqline(rstandard(fit3), col = 2)

#compute influential points statistics
influence.measures(fit3)
summary(influence.measures(fit3))

dfbeta(fit3)
covratio(fit3)
diffits(fit3)
cooks.ditance(fit3)

# # plot of deleted studentized residuals vs hat values
plot(rstudent(fit3)~hatvalues(fit3)) 
 
# to find the influential point
rstudent(fit3)


#create the new dataset containing values for predictions
new = data.frame(gir=c(67), driving=c(64),putting=c(1.77), birdie=c(28), putts=c(29.16))

# compute predicted value and prediction interval
predict(fit3, new, interval="prediction", level=0.95) 


#create the new dataset containing values for predictions
new = data.frame(gir=c(67), birdie=c(28), putts=c(29.16))

# compute predicted value and prediction interval
predict(fit3, new, interval="prediction", level=0.95) 


