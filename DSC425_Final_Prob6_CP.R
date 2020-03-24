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
