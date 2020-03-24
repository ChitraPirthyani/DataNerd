getwd()
library(magrittr)
library(forecast)
library(fpp2)
library(astsa)


AirPassengers

AirPassengers %>% autoplot()
lm_trend_lin = tslm(AirPassengers~trend)
summary(lm_trend_lin)

autoplot(AirPassengers) + 
  autolayer(lm_trend_lin$fitted.values)

fcast = forecast(lm_trend_lin, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_trend_lin$fitted.values) +
  autolayer(fcast)



# Multiplicative trend
AirPassengers %>% autoplot()
lm_trend_log = tslm(AirPassengers ~ trend, lambda = 0)

summary(lm_trend_log)

fcast = forecast(lm_trend_log, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_trend_lin$fitted.values) +
  autolayer(lm_trend_log$fitted.values)



autoplot(AirPassengers) + 
  autolayer(lm_trend_log$fitted.values) +
  autolayer(fcast)
  

BoxCox.lambda(AirPassengers)

lm_trend_log = tslm(AirPassengers ~ trend, 
                    lambda = BoxCox.lambda(AirPassengers))

summary(lm_trend_log)

fcast = forecast(lm_trend_log, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_trend_log$fitted.values) +
  autolayer(fcast)

autoplot(BoxCox(AirPassengers,lambda=1))+
  autolayer(AirPassengers)
autoplot(BoxCox(AirPassengers,lambda=0.5))
autoplot(BoxCox(AirPassengers,lambda=0.2))
autoplot(BoxCox(AirPassengers,lambda=0))
autoplot(BoxCox(AirPassengers,lambda=-0.2))
autoplot(BoxCox(AirPassengers,lambda=-0.5))
autoplot(BoxCox(AirPassengers,lambda=-1))



# Quadratic
lm_trend_quad = tslm(AirPassengers ~ trend + I(trend^2))

summary(lm_trend_quad)

fcast = forecast(lm_trend_quad, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_trend_quad$fitted.values) +
  autolayer(fcast)

plot(as.vector(AirPassengers))

# Seasonality
lm_season <- tslm(AirPassengers~season)

summary(lm_season)

fcast = forecast(lm_season, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)

# Seasonality + Trend
lm_season <- tslm(AirPassengers~trend+season)

summary(lm_season)

fcast = forecast(lm_season, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)

# Seasonality + Trend
lm_season <- tslm(AirPassengers~trend+season, lambda=0)

summary(lm_season)

fcast = forecast(lm_season, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)

# Seasonality + Trend + Trend^2
lm_season <- tslm(AirPassengers~trend+I(trend^2)+season)

summary(lm_season)

fcast = forecast(lm_season, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)

# Make predictions using trend+trend^2+season model
# Prediction for January
88.5+1.62*145+0.007*145^2 # 470.575
# Prediction for July
88.5+1.62*151+0.007*151^2 + 93.83*1 # 586.557
fcast


# Seasonality + Trend + Trend^2
lm_season <- tslm(AirPassengers~trend+I(trend^2)+season, lambda=0)

summary(lm_season)

fcast = forecast(lm_season, h=12)

autoplot(AirPassengers) + 
  autolayer(lm_season$fitted.values) +
  autolayer(fcast)

?easter()
?bizdays()

# Multiple regression
uschange %>% head()
autoplot(uschange, facets = T)

uschange_tr = window(uschange, start=1970, end=c(1999,4))
uschange_tst = window(uschange, start=2000)

lm_multiple <- tslm(Consumption~trend+season+Income+Production, 
                    data=uschange_tr)

summary(lm_multiple)
fcast = forecast(lm_multiple, newdata=as.data.frame(uschange_tst))

autoplot(fcast)


lm_multiple <- tslm(Consumption~Income+Production, 
                    data=uschange_tr)

summary(lm_multiple)
fcast = forecast(lm_multiple, newdata=as.data.frame(uschange_tst))

autoplot(fcast) + autolayer(uschange_tst[,'Consumption'])



AirPassengers

coeftest(m)
forecast(m, h=2) %>% plot()
plot(AirPassengers)
lines(m$fitted.values)
sqrt(mean(m$residuals^2))
m$residuals


m = tslm(AirPassengers ~ trend, lambda = 0)
m
sqrt(mean(m$residuals^2))
mean(abs(m$residuals))
plot(AirPassengers)
lines(m$fitted.values)

m$residuals
