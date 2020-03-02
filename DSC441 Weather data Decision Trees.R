########################################
# SECTION 1 - Install Packages        ##
########################################
getwd()
if (!(require(dplyr))) install.packages ("dplyr")
library(dplyr)
if (!(require(tidyverse))) install.packages ("tidyverse")

library(tidyverse)
if (!(require(purrr))) install.packages ("purrr")
library(purrr)
if (!(require(lubridate))) install.packages ("lubridate")
library(lubridate)
if (!(require(ggplot2))) install.packages ("ggplot2")
library(ggplot2)
# https://cran.r-project.org/web/packages/riem/riem.pdf
if (!(require(riem))) install.packages ("riem")
library(riem)
         
########################################
# SECTION 2 - Infile Weather Data     ##
########################################

# Hourly Weather Data CSV 
# gDrive location: https://drive.google.com/open?id=1tr9ao5vGEUy43JzQd-rrYIshFvzjaMrZ
#hw60 = read.csv('/Users/michaelsmith/Desktop/data/hw60.csv', sep = ',')
hrWsmall = read.csv("hrWsmall.csv", sep = ',')


# hrWsmall(hourlyWeathersmall) for Regression code ####
#set.seed(1234)
#hrWsmall = sample_n(hw60, (0.1*nrow(hw60)))


# wType(weatherType) dataset for Classification code ####
wType = hrWsmall %>% 
  mutate(is_OVC = as.integer(skyc1 == 'OVC')) %>%
  mutate(is_SCT = as.integer(skyc1 == 'SCT')) %>%
  mutate(is_FEW = as.integer(skyc1 == 'FEW')) %>%
  mutate(is_BKN = as.integer(skyc1 == 'BKN')) %>%
  mutate(is_VV = as.integer(skyc1 == 'VV')) 

write.csv(wType,"wType.csv", row.names = FALSE)

wType
hrWsmall

wType1 <- wType %>% select(-skyc1)
wType1
########################################
# SECTION 3 - Descriptive Stats       ##
######################################## 
summary(hw60)

#Descriptive statistics on two variables (station by temp example)
n    <- table(hw60$station)
mean <- tapply(hw60$tmpf,hw60$station,mean)
med  <- tapply(hw60$tmpf,hw60$station,median)
sd   <- tapply(hw60$tmpf,hw60$station,sd)

min  <- tapply(hw60$tmpf,hw60$station,min)
max  <- tapply(hw60$tmpf,hw60$station,max)
dhw60 <- cbind(n,mean,med,sd,min,max)
dhw60

# Data mining - Normalization
#view(hrWsmall)
#n_dfw = hrWsmall[,-1,-2,-16] # need to remove the categocial variables
#n_dfw_funct = function(x) { ((x-min(x))/(max(x)-min(x)))}
#n_dfw = as.data.frame(lapply(hrWsmall[,-1,-2], n_dfw_funct))'''
########################################
# SECTION 4 - ggplot                  ##
######################################## 
# Visutalization of distribution for categorical vairables 
ggplot(data = hrWsmall) +
  geom_bar(mapping = aes(x = skyc1))
# Get the counts
hrWsmall %>% 
  count(skyc1)

# Temp by station box plots rotate statename 45 degrees
p = ggplot(data = hrWsmall, mapping = aes(x = station, y = tmpf)) + geom_boxplot()

p + theme(axis.text.x = element_text(angle = 45))

# Flip the axis
ggplot(data = hrWsmall) +
  geom_boxplot(mapping = aes(x = reorder(station, tmpf, FUN = median), y = tmpf)) +
  coord_flip()

# distribution of data by year, month, day
ggplot(data=hw60, aes(x=factor(year))) + # notice you can change to factor
  geom_bar(color='red', fill ='royalblue') +
  theme_bw()

ggplot(data=hw60, aes(x=factor(month))) + # notice you can change to factor
  geom_bar(color='red', fill ='royalblue') +
  theme_bw()

ggplot(data=hw60, aes(x=factor(day))) + # notice you can change to factor
  geom_bar(color='red', fill ='royalblue') +
  theme_bw()


ggplot(data=hw60, aes(x=factor(skyc1))) + # notice you can change to factor
  geom_bar(color='red', fill ='royalblue') +
  theme_bw()

# temp and dew point 
ggplot(data=hw60, aes(x=tmpf,y=dwpf)) + geom_point()

# plot geo does not work

# library(ggplot2)
# if (!(require(viridis))) install.packages ("viridis")
# library(viridis)


# locDf = read.csv("/Users/michaelsmith/Desktop/data/airCodes.csv", sep = ",")

# ggplot() + geom_raster(data = locDf, aes(x=lon, y=lat)) + 
#   coord_fixed(ratio = 1) +
#   scale_fill_viridis(direction = -1) +
#   theme_bw()

########################################
# SECTION 5 - Corr & Regression       ##
########################################
#corr ####
# source: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

corTest = cbind.data.frame(year = train$year, month = train$month, day = train$day, hour = train$hour, minute = train$minute, 
                temp = train$tmpf, feel = train$feel, dewPoint = train$dwpf, relHumid = train$relh, windDrct = train$drct,
                windSpeed = train$sknt, oneHourPrecip = train$p01i, skyLevelAlt = train$skyl1)
res = cor(corTest)
round(res,2)

if (!(require(corrplot))) install.packages ("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Use heatmap()
col<- colorRampPalette(c("blue", "white", "red"))(40)
heatmap(x = res, col = col, symm = TRUE)

# Regression ####
source("http://condor.depaul.edu/jlee141/econdata/R/func_lib.R")
library("hts")
library("stargazer")

#Create Sample Size
#hrWsmall = sample_n(hw60, (0.1*nrow(hw60)))
smp_size = floor(0.75 * nrow(hrWsmall))
#write.csv(hrWsmall,"/Users/michaelsmith/Desktop/data/hrWsmall.csv", row.names = FALSE)
## set the seed to make your partition reproducible
set.seed(1234)
train_ind <- sample(seq_len(nrow(hrWsmall)), size = smp_size)

train <- hrWsmall[train_ind, ]
test <-  hrWsmall[-train_ind, ]

# Run stepwise calc to find best model
# reg1 = step(lm(tmpf ~ year+month+day+hour+minute+feel+dwpf+relh+drct+sknt+p01i+vsby+skyl1, train))
#summary(reg1)

model1 = tmpf ~ year + month + hour + minute + feel + dwpf + relh + drct + 
  sknt + p01i + vsby + skyl1
reg1 = lm(model1, data=train)
summary(reg1)

# Regression using train data 
reg1 = lm(model1, data=train)
ytrue <- train$tmpf
eval1 <- fit_eval(reg1$fitted.values,ytrue,"TRAIN: OLS1")

# Out of Sample Forecasting Results
ytest <- test$tmpf
test1 <- fit_eval(predict(reg1, newdata=test),ytest,"TEST: OLS1")

# Chceking different regresion fits

reg1 <- lm(model1,data=train)
reg2 <- lm(tmpf~dwpf,data=train)
ytrue <- train$tmpf
eval1 <- fit_eval(reg1$fitted.values,ytrue,"TRAIN: OLS1")
eval2 <- fit_eval(reg2$fitted.values,ytrue,"TRAIN: OLS2")
stargazer(reg1,reg2,type='text',no.space=TRUE,column.sep.width = "1pt")
model_comp <- rbind(eval1,eval2)
model_comp

# Out of Sample Forecasting Results
ytest <- test$tmpf
test1 <- fit_eval(predict(reg1, newdata=test),ytest,"TEST: OLS1")
test2 <- fit_eval(predict(reg2, newdata=test),ytest,"TEST: OLS2")
test_comp <- rbind(test1,test2)
test_comp 

# indidvudal distribution of errors vs predictive value - regression models ####
# Temp & Year ####
r1 <- lm(tmpf~year, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)

# Temp & month ####
r1 <- lm(tmpf~month, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1, col = "red")
#hist(e1, breaks=40)

# Temp & day ####
r1 <- lm(tmpf~day, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1, col = "red")
#hist(e1, breaks=40)

# Temp & hour ####
r1 <- lm(tmpf~hour, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1, col = "red")
#hist(e1, breaks=40)

# Temp & minute ####
r1 <- lm(tmpf~minute, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1, col = "red")
#hist(e1, breaks=40)

# Temp & feel ####
r1 <- lm(tmpf~feel, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1, col = "red")
#hist(e1, breaks=40)

# Temp & dwpf ####
r1 <- lm(tmpf~dwpf, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1, col = "red")
#hist(e1, breaks=40)

# Temp & relative humidity ####
r1 <- lm(tmpf~relh, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)

# Temp & wind direction ####
r1 <- lm(tmpf~drct, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)

# Temp & wind in knots ####
r1 <- lm(tmpf~sknt, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)

# Temp & percipitation in previous hour ####
r1 <- lm(tmpf~p01i, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)

# Temp & visability ####
r1 <- lm(tmpf~skyl1, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)

# Temp & sky level in altitude ####
r1 <- lm(tmpf~vsby, data = train)
summary(r1)
e1 <- residuals(r1)
p1 <- predict(r1)
plot(p1,train$tmpf)
abline(r1)
#hist(e1, breaks=40)


### Classification

# wType(weatherType) dataset for Classification code ####
wType = hrWsmall %>% 
  mutate(is_OVC = as.integer(skyc1 == 'OVC')) %>%
  mutate(is_SCT = as.integer(skyc1 == 'SCT')) %>%
  mutate(is_FEW = as.integer(skyc1 == 'FEW')) %>%
  mutate(is_BKN = as.integer(skyc1 == 'BKN')) %>%
  mutate(is_VV = as.integer(skyc1 == 'VV')) 
write.csv(wType,"wType.csv", row.names = FALSE)

wType
hrWsmall

wType1 <- wType %>% select(-skyc1)
view(wType1)

wType_ovc <- wType1 %>% select(-is_SCT, -is_FEW, -is_BKN, -is_VV)
wType_ovc
# Decision Tree

library(rpart)
# install.packages('rattle')
library(rattle)

classmodel = rpart(is_OVC ~ hour+month+station+tmpf+feel+dwpf+relh+drct+sknt+p01i+vsby+skyl1, method = 'class', data=wType_ovc)
summary(classmodel)
fancyRpartPlot(classmodel)

classmodel1 = rpart(is_OVC ~ relh+station+tmpf+dwpf+vsby+skyl1, method = 'class', data=wType_ovc)
summary(classmodel1)
fancyRpartPlot(classmodel1)
