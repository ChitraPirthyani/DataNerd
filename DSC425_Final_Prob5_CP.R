library(lubridate)
library(magrittr)

library(magrittr)
library(forecast)
library(fpp2)
library(astsa)

#Problem 5

# 5.1 Importing data and converting to time series

vis = read.csv("australia_visitors.csv")
vis
visitors = ts(vis, start = c(1990,1), frequency = 12)

str(visitors)
visitors %>% head() 

# 5.2 time series plot

autoplot(visitors)
seasonplot(visitors)

visitors %>% autoplot() 
visitors %>% log %>% autoplot()
visitors %>% log %>% acf2()
visitors %>% log %>% diff() %>%autoplot()
visitors %>% diff() %>% autoplot()
visitors %>% diff() %>% diff()%>% autoplot()
acf(visitors)
visitors %>% diff() %>% acf()
visitors %>% diff() %>% pacf()

# 5.4 linear trend
visitors %>% autoplot()
visittrend = tslm(visitors ~ trend)
visittrend
summary(visittrend
        )
autoplot(visitors)+
  autolayer(visittrend$fitted.values)
coeftest(visittrend)
forecast(visittrend, h=12) %>% plot()
plot(visitors)
lines(visittrend$fitted.values)

# 5.5 Trend and seasonality
seastrend= tslm(visitors ~ trend + season)
seastrend
summary(seastrend)
autoplot(visitors)+
autolayer(seastrend$fitted.values)
lines(seastrend$fitted.values)


# 5.6 

#subsetting data
r = window(visitors, start=1990, end=2000)
r
str(r)

# linear model
rmodel = tslm(r ~ trend + season)
rmodel
summary(rmodel)

rmodel$residuals
rmodel$residuals %>% autoplot()
autoplot(rmodel$residuals)
qqnorm(rmodel$residuals)
qqline(rmodel$residuals)

acf(rmodel$residuals, type = "correlation")

Box.test(rmodel$residuals, lag=20, type = "Ljung", fitdf = 2)

hist(rmodel$residuals,  main = "Histogram of residuals")
xfit<-seq(min(rmodel$residuals),max(rmodel$residuals),length=30)
yfit<-dnorm(xfit,mean=mean(rmodel$residuals),sd=sd(rmodel$residualse))
lines(xfit, yfit, col="blue", lwd=2) 

checkresiduals(rmodel$residuals
               , lag=10, df=NULL, plot = TRUE)

rf= forecast(rmodel, h=63) %>% plot()
rf
head(rf)
autoplot(r) + 
  autolayer(rmodel$fitted.values) +
  autolayer(rf)
 

# 5.7

kpss.test(visitors, null = c("Level", "Trend"), lshort = T)

kpss.test(visitors)

diff1=visitors %>% diff()
diff2=visitors %>% log %>% diff()
diff3 = visitors %>% diff() %>% diff(12)
diff5 = visitors %>% diff(12) %>% diff()
diff4= visitors %>% diff() %>% diff() 
autoplot(diff2)

diff3
diff5



kpss.test(diff1)
kpss.test(diff2)
kpss.test(diff3)
kpss.test(visitors %>% diff(5))

diff27=visitors %>% diff(27) 
acf2(diff27)

diff28=visitors %>% diff(lag = 12)
acf2(diff28)


# 5.8

acf2(diff1)
acf2(diff2)
acf2(diff3)
acf2(diff4)

#5.9

a=auto.arima(visitors)
a
coeftest(a)
c=auto.arima(r)
c

m1=Arima(visitors, order = c(1,2,2), seasonal = c(0,2,2))
m1
coeftest(m1)

sarima_M1 <- sarima(visitors,p=1,d=2,q=2, P=0, D=2, Q=2, S=12)
Box.test(m1$residuals, lag=10, type = "Ljung", fitdf = 2)
hist(m1$residuals,  main = "Histogram of residuals")


sarima_a <- sarima(visitors,p=1,d=0,q=1, P=0, D=1, Q=2, S=12)

m2=Arima(r, order = c(1,2,2), seasonal = c(0,2,2))
m2
coeftest(m2)




#5.10

m1f= forecast(m1, h=5)
m1f
head(rf)
autoplot(r) + 
  autolayer(rmodel$fitted.values) +
  autolayer(rf)

m2f= forecast(m2, h=63) %>% plot()
m2f
autoplot(r) + 
  autolayer(m2f)

#5.12

# RMSE
sqrt(mean(rmodel$residuals^2))

sqrt(mean(m2$residuals^2))

Linearrmse= sqrt(mean((504.2920-409.5)^2))
Linearrmse

sarimarmse= sqrt(mean((311.1301-409.5)^2))
sarimarmse
504.2920-409.5

94.792^2

sqrt(mean(8985.523))

rmse(409.5, 504.2)
rmse(409.5, 311.1)

linearmape=mape(409.5, 504.2)
linearmape
sarimamape=mape(409.5, 311.1)
sarimamape



library(lubridate)
library(magrittr)

library(magrittr)
library(forecast)
library(fpp2)
library(astsa)

# Problem6

# 6.1
murders = read.csv("women_murders.csv")
seasonplot(murders)
murder= ts(murders, start = c(1960), frequency = 1)
str(murder)

# 6.2
murder %>% autoplot()
plot(murder, main= "Time plot of murders")

#6.4
kpss.test(murder)

dif=murder %>% diff() 
dif1=murder %>% diff() %>% diff() 
dif2=murder %>% diff() %>% diff(5) 

dif=murder %>% diff()
dif1=murder %>% diff() %>% diff()
dif2=murder %>% diff() %>% diff(5)

autoplot(dif)
autoplot(dif1)
dif1 %>% autoplot()
kpss.test(murder %>% diff(35))

#6.5
acf2(murder)
acf2(murder%>% diff()%>%diff())
acf2(dif)
acf2(dif1)
acf2(dif2)
acf2(murder)
acf2((murder %>% diff(35)))

#6.6
b1=auto.arima(murder)
b1
coeftest(b1)

b=Arima(murder, order = c(0,2,2))
coeftest(b)
coeftest(b1)
b
b1

b2= Arima(murder, order= c(1,0,0))
b2
coeftest(b2)

b3 = Arima(murder, order=c(0,1,2))
b3
coeftest(b3)

b4 = Arima(murder, order= c(1,1,1))
b4
coeftest(b4)

#6.7
sarima(murder, p=1,d=0,q=0)
sarima(murder, p=0,d=2,q=1)
sarima(murder, p=1,d=1,q=1)


Box.test(b1$residuals, lag=20, type = "Ljung", fitdf = 2)
Box.test(b3$residuals, lag=20, type = "Ljung", fitdf = 2)
Box.test(b4$residuals, lag=20, type = "Ljung", fitdf = 2)

#6.8
bf = forecast(b4, h=6) %>% autoplot()
bf

#PRoblem4
#January 10
187.95 - 0.04(10)
187.95- 0.4

#February 10
187.95 - 0.04(40)
187.95-1.6

