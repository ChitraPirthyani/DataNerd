comed <- read.csv("COMED_hourly.csv", header = TRUE)
str(comed)
library(zoo)
library(lubridate)
library(reprex)


comed$Datetime <- lubridate::ymd_hms(comed$Datetime)
dplyr::arrange(comed, Datetime)

comed_hourlyTS <- ts(comed$COMED_MW, start = c(2011,1))
autoplot(comed_hourlyTS)
plot(comed_hourlyTS, main= "Hourly Time Plot")
acf(comed_hourlyTS)
pacf(comed_hourlyTS)
########################################################################################


###################
#Comed Daily
###################
library(dplyr)
comed_daily <- comed %>%
  mutate(day = as.Date(Datetime, format = "%Y-%m-%d")) %>%
  group_by(day) %>% # group by the day column
  summarise(DAILY_COMED=mean(COMED_MW))

head(comed_daily)

comed_dailyTS <- ts(comed_daily$DAILY_COMED, start = c(2011,1))
plot(comed_dailyTS, main= "Daily Time Plot")
acf(comed_dailyTS)
pacf(comed_dailyTS)
###################
#Comed Monthly
##################

comed_monthly <- comed %>%
  mutate(month = as.yearmon(Datetime, format = "%Y-%m")) %>%
  group_by(month) %>% # group by the day column
  summarise(MONTHLY_COMED=mean(COMED_MW))


comed_monthlyTS2 <- ts(comed_monthly$MONTHLY_COMED, start = c(2011,1), frequency = 12)
plot(comed_monthlyTS2, main= "Monthly Time Plot")
acf(comed_monthlyTS2)

acf

pacf(comed_monthlyTS2)


##################
#COMED Yearly
##################
comed_yrly2 <- comed %>%
  mutate(year = format(Datetime, "%Y")) %>%
  group_by(year) %>%
  summarise(YEARLY_COMD = mean(COMED_MW))

comed_yrly2TS <- ts(comed_yrly2$YEARLY_COMD, start = c(2011))

plot(comed_yrly2TS, main= "Yearly Time Plot")
acf(comed_yrly2TS)
pacf(comed_yrly2TS)


###########################
#Performing Auto ARIMA for the different Time Series data
###########################

library(forecast)
arima_yearly <- auto.arima(comed_yrly2TS)
arima_yearly


arima_monthly <- auto.arima(comed_monthlyTS2)
arima_monthly
coeftest(arima_monthly)


arima_daily <- auto.arima(comed_dailyTS)
arima_daily

#arima_hourly
arima_hourly <- auto.arima(comed_hourlyTS)
arima_hourly

arima_hourly2 <- auto.arima(comed_hourlyTS , allowdrift = TRUE, method="ML")
arima_hourly2
coeftest(arima_hourly2)

############################
#Checking the coefficients
############################
library(lmtest)
coeftest(arima_yearly)
coeftest(arima_monthly)
coeftest(arima_daily)
coeftest(arima_hourly)


M1 <- Arima(comed_monthlyTS2,
            order = c(0,0,1),
          seasonal = c(0,1,1), lambda=12)
M1



mm1 <- Arima(comed_monthlyTS2,
             order = c(1,1,0),
             seasonal = c(0,1,1), lambda=12)
mm1
coeftest(mm1)
coeftest(M1)

M1
fitARIMA_M1 <- arima(comed_monthlyTS2, order=c(0,0,1),seasonal = list(order = c(0,1,1), period = 12))
library(lmtest)
coeftest(fitARIMA_M1)
autoplot(forecast(fitARIMA_M1, h=10))



#Manual ARIMA(1,0,0)(0,1,1)[12] for monthly M2 from autoarima
M2 <- Arima(comed_monthlyTS2,
                        order = c(1,0,0),
                        seasonal = c(0,1,1), lambda=12)

M21 <- Arima(comed_monthlyTS2,
             order = c(1,0,0),
             seasonal = c(1,1,0), lambda=12)
M21


M2
fitARIMA_M2 <- arima(comed_monthlyTS2, order=c(1,0,0),seasonal = list(order = c(0,1,1), period = 12))
library(lmtest)
coeftest(fitARIMA_M2)
autoplot(forecast(fitARIMA_M2, h=10))



M3 <- Arima(comed_monthlyTS2,
                        order = c(0,0,0),
                        seasonal = c(0,1,1), lambda=12)
M3
coeftest(M3)
fitARIMA_M3 <- arima(comed_monthlyTS2, order=c(0,0,0),seasonal = list(order = c(0,1,1), period = 12))
library(lmtest)
coeftest(fitARIMA_M3)
autoplot(forecast(fitARIMA_M3, h=10))



#Sarima(1,0,0)(0,1,1)[12] for monthly
library(xts)
library(forecast)
library(zoo)
library(dplyr)
library(magrittr)
library(forecast)
library(fpp2)
library(astsa)

sarima_M2 <- sarima(comed_monthlyTS2,p=1,d=0,q=0, P=0, D=1, Q=1, S=12)

sarima_M1 <- sarima(comed_monthlyTS2,p=0,d=0,q=1, P=0, D=1, Q=1, S=12)

sarima_M3 <- sarima(comed_monthlyTS2,p=0,d=0,q=0, P=0, D=1, Q=1, S=12)

sarima_hourly <- sarima(comed_hourlyTS, p=3, d=1, q=2)

#############################
#Ljung box test
#############################

Box.test(M1$residuals, lag=5)
Box.test(M2$residuals, lag=5)
Box.test(M3$residuals, lag=5)

Box.test(arima_hourly2$residuals, lag=15)


##########################
#histograms
#########################

hist(arima_hourly2$residuals)
hist(M1$residuals)
hist(M2$residuals)

#############################
#JB Test
############################

library(normtest)

hourlyjb=jb.norm.test(comed_hourlyTS, nrepl = 2000)
hourlyjb

M1jb= jb.norm.test(M1$x, nrepl = 2000)
M1jb

M2jb= jb.norm.test(M2$x, nrepl = 2000)
M2jb

############################
#Forecasting
###########################
#fcst10<- forecast(fit_arima,h=10)
#autoplot(fcst10, include = 150)
#print(summary(fcst10))


autoplot(forecast(arima_monthly, h=10))


fcstM1 <- forecast(M1, h=10) 
autoplot(fcstM1)

fcstM2 <- forecast(M2, h=10)
autoplot(fcstM2)

fcstM3 <- forecast(M3, h=10)
autoplot(fcstM3)
M1 %>% forecast(h=12) %>% plot()

plot(forecast(arima_monthly, h=10))
library(forecast)
plot(forecast.Arima(M1, h=10))

plot(forecast(M2,h=10, c(80,95)))

plot(forecast(M3,h=10))



##############################
#Backtesting
#############################
source("backtest_v2.R")
library(stats)
library(lmtest)
backtest(M1, comed_monthlyTS2, 194, 1)


nrow(comed_monthly)
backtest(M1, comed_monthlyTS2, 78,1)

backtest(M2, comed_monthlyTS2, 78,1)

backtest(M3,comed_monthlyTS2, 78,1)


#############################
#Summary of the models
#############################


summary(M1)
summary(M2)

?backtest

head(comed_monthly)

mod<- lm(MONTHLY_COMED ~ ., data = comed_monthly)
mod
summary(mod)
plot(mod)


head(comed_yrly2)
mod_yearly <- lm(YEARLY_COMD ~.,data= comed_yrly2)
mod_yearly
summary(mod_yearly)
plot(mod_yearly)



#############################33
#AEP data
#############################

aep_data <- read.csv("AEP_hourly.csv", header = TRUE)
head(aep_data)

aep_data$Datetime <- lubridate::ymd_hms(aep_data$Datetime)
dplyr::arrange(aep_data, Datetime)



nrow(aep_data)


#Subsetting AEP data
AEP_2014 <- subset(aep_data,
                         Datetime >= as.POSIXct('2014-01-01 00:00') &
                           Datetime <= as.POSIXct('2014-04-01 23:59'))

AEP_2014$Datetime <- lubridate::ymd_hms(AEP_2014$Datetime)
dplyr::arrange(AEP_2014, Datetime)



head(AEP_2014)
nrow(AEP_2014)

#Subsetting COMED data
comed2014 <- subset(comed,
                    Datetime >= as.POSIXct('2014-01-01 00:00') &
                      Datetime <= as.POSIXct('2014-04-01 22:59'))
head(comed2014)
nrow(comed2014)


comed2_2014 <- comed2014
comed2_2014$AEP_MW <- AEP_2014$AEP_MW

head(comed2_2014)


#the regression model - this is hourly though
mod1 <- lm(AEP_MW ~ COMED_MW, data = comed2_2014)
mod2 <- lm(AEP_2014$AEP_MW ~ comed2014$COMED_MW)
mod1
summary(mod2)
plot(mod2)

plot(AEP_2014$AEP_MW ~ comed2014$COMED_MW, main= "AEP vs ComEd, for Q1 of Jan2014", xlab="ComEd Energy" , ylab="AEP Energy")
abline(mod1)

# Monthly Regression Trend + Season

comedtrend <- tslm(comed_monthlyTS2 ~ trend + season)
comedtrend
summary(comedtrend)

autoplot(comed_monthlyTS2)+ autolayer(comedtrend$fitted.values)

autoplot(comedtrend$residuals)
qqnorm(comedtrend$residuals)
qqline(comedtrend$residuals)

acf(comedtrend$residuals, type = "correlation")


Box.test(comedtrend$residuals, lag=20, type = "Ljung", fitdf = 2)

hist(comedtrend$residuals,  main = "Histogram of residuals")

checkresiduals(comedtrend$residuals
               , lag=10, df=NULL, plot = TRUE)

 cf = forecast(comedtrend, h=24) %>% plot() 

 
rmse_comedtrend= sqrt(mean(comedtrend$residuals^2))
 mape_comedtrend= mean(abs(comedtrend$residuals))
rmse_comedtrend 
mape_comedtrend 
 







#Correlation

correlation = cor.test(comed2014$COMED_MW, AEP_2014$AEP_MW)
correlation

library(psych)
cor.plot(x=comed2014$COMED_MW, y=AEP_2014$AEP_MW)

cor.plot()

corr.test(x=comed2014$COMED_MW, y=AEP_2014$AEP_MW)

library(polyPK)
corrplot::corrplot(corr,x=comed2014$COMED_MW, y=AEP_2014$AEP_MW)

corrplot::corrplot(comed2014, AEP_2014, method = "circle", type="full")
