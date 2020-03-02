# Problem 1
# 1.e


percept
percept$Error <- percept$Response-percept$TrueValue
head(percept$Error)
percept$AbsoluteError <- abs(percept$Error)
head(percept$AbsoluteError)

plot(Test, AbsoluteError, xlab="Test", ylab= "Absolute Error", main="Distribution of error", color="Red")


library(ggplot2)

# Density Plot
ggplot(percept) + geom_density(aes(x = AbsoluteError, fill= Test)) + facet_grid()


# Problem 4
#4.b

read.csv("MessierData1.csv")
messy=read.csv("MessierData1.csv")

View(messy)

# 4.b-Density Plot
distancelog10 = log10(Dist)
ggplot(messy) + geom_density(aes(x=distancelog10, fill =Kind))

# Random exploration, to be ignored

library(MESS)

panel.hist(messy$Distance..LY., "blue")

panel.hist(messy$Distance..LY., col.bar = "light blue")



pairs(~ Ozone + Temp + Wind + Solar.R, data=airquality,
      lower.panel=panel.smooth, diag.panel=panel.hist,
      upper.panel=panel.r2)


pairs(~ messy$Distance..LY., data = messy, lower.panel=panel.hist, diag.panel=panel.hist,upper.panel=panel.hist)


library(ggplot2)

ggplot(messy, aes(x= messy$Distance..LY., y= messy$Kind)) + geom_histogram(aes(y=..density..)) + stat_function(fun = dnorm, colour= "red",
)

ggplot(airquality, aes(x = Ozone)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, colour = "red",
                arg = list(mean = mean(airquality$Ozone, na.rm = TRUE),
                           sd = sd(airquality$Ozone, na.rm = TRUE)))


Dist = messy$Distance..LY.
Kind = messy$Kind


distancelog10 = log10(Dist)
scale_y_log10()
ggplot(messy, mapping = aes(Dist, Kind))

ggplot(messy) + geom_density(aes(x=distancelog10, fill =Kind))
ggplot(percept) + geom_density(aes(x = AbsoluteError, fill= Test, scale_y_log10())) + facet_g


# Problem 5
#5.b

read.csv("PortlandWaterLevel.csv")
pwl=read.csv("PortlandWaterLevel.csv")
plot(pwl$Time,pwl$WL, xlab("Time") )
plot(pwl$Date,pwl$WL )

#calculating moving avg
movavg(pwl$WL,7, type = c("s"))

movingavg <- movavg(pwl$WL,7, type = c("s"))

pwl$movingavg = movingavg <- movavg(pwl$WL,7, type = c("s"))

head(movingavg)

plot(pwl$Time, movingavg)

#tukey box plot of WL and Time
Time= pwl$Time
WL = pwl$WL
plot(Time,WL, col= "light pink", xlab="Time", ylab="WL", main="")









