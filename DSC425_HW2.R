# PROBLEM 2

read.csv("NAPM.csv")

library(magrittr)

library(magrittr)
library(forecast)
library(fpp2)
library(astsa)

df = read.csv('NAPM.csv')
df %>% head()

#2.a
str(df)
tsdf = ts(df$index, start=c(1980,1), frequency = 12)
str(tsdf)
frequency(tsdf)
summary(tsdf)

#2.b
autoplot(tsdf)
tsdf %>% autoplot()
tsdf %>% acf()
acf(tsdf)
dataacf = acf(tsdf, type = "correlation")
dataacf
tsdf %>% pacf()
tsdf %>% acf2()

#2.c Ljung Box
Box.test(tsdf, lag=5,type = 'Ljung-Box')
Box.test(tsdf, lag=3,type = 'Ljung-Box')

#2.d
diff(tsdf) %>% autoplot()
tsdf %>% diff() %>% acf2()

#2.e
sarima(tsdf, 3,0,0)

m = Arima(tsdf, order = c(3,0,0))
m
library(lmtest)
coeftest(m)
plot(m)


m2 = Arima(tsdf, order=c(3,0,0), fixed=c(NA,0,NA,NA))
m2
coeftest(m2)
plot(m2)

m3 = Arima(tsdf, order=c(4,0,0), fixed=c(NA,0,NA,NA, NA))
m3
coeftest(m3)
plot(m3)

#2.e (b) residuals

acf(m$residuals, main= "ACF of Residuals")
acf(m$residuals, lag=10, main= "ACF of Residuals")
acf(m2$residuals,lag=15, main= "ACF of Residuals")

residdata = acf(m$residuals, type = "correlation")
residdata
#2.e (c) qqplot and histo
qqnorm(m$residuals, main="QQplot of residuals")
 qqline(m$residuals,col="blue")
 
 hist(m$residuals, main = "Histogram of residuals")
 
 Box.test(m$residuals, lag=10, type = "Ljung", fitdf = 3)


#2.h 
 forecast(m, h = 5, level = c(0.70, 0.95)) %>% plot()
 lines(xtest)
 forecast(m, h = 5, level = c(0.70, 0.95))
 
 
 forecast(m, h = 10) %>% plot()
 lines(xtest)
 forecast(m, h = 10, level = c(0.70, 0.95))
 
###########################################################################################
 
 
 # PROBLEM 3
 
 myd = read.csv("groceries.csv")
 head(myd)
 
 #creating time series
 date1 = myd[,2]
 
 gts = ts(date1, start=c(2008,1), freq=52)
 
 #3.a
 gts %>% autoplot()
 gts %>% acf()
 gts %>% pacf()
 gts %>% acf2()
 
 myacf = acf(gts, type="correlation")
myacf
print(acf(gts))

myacf = acf(gts, type='correlation', title(main = "ACF"))
myacf

#3.b

sarima(gts,1,0,0)

M = Arima(tsdf, order = c(1,0,0))
M
library(lmtest)
coeftest(M)

plot(M)


M1 = Arima(gts, order=c(2,0,0))
M1
coeftest(M1)
plot(M1)


M2 = arima(gts, order = c(3,0,0))
M2
coeftest(M2)

#3.c
# residual analysis

acf(M1$residuals, main= "ACF of Residuals")
acf(M1$residuals, lag=20,main= "ACF of Residuals 20 lag")


residdata = acf(M$residuals, type = "correlation")
residdata

Box.test(M1$residuals, lag=10, type = "Ljung", fitdf = 2)

qqnorm(M1$residuals, main="QQplot of residuals")
qqline(M1$residuals,col="blue")

hist(M1$residuals,  main = "Histogram of residuals")
xfit<-seq(min(M$residuals),max(M$residuals),length=30)
yfit<-dnorm(xfit,mean=mean(M$residuals),sd=sd(M$residualse))
lines(xfit, yfit, col="blue", lwd=2) 

checkresiduals(M1$residuals, lag=10, df=NULL, plot = TRUE)


#3.d

forecast(M1, h = 5) %>% plot()
lines(xtest)
lines(gts)
forecast(M1, h = 5)

forecast(m, h = 10) %>% plot()
lines(xtest)
forecast(m, h = 10, level = c(0.70, 0.95))
