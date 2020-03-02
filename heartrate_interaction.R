myd = read.table("stepping.txt", header=F)

# define variables
height = myd[,3]
freq = myd[,4]
hr = myd[,6]

# create dummy variables
f_med=(freq==1)*1
f_high=(freq==2)*1
h_high = (height==1)*1

# attach it to myd dataset
myd=cbind(myd, f_med, f_high, h_high)

#create boxplots
boxplot(hr~freq, xlab = "step frequency level", ylab = "heart rate")
boxplot(hr~height, xlab = "height level", ylab = "heart rate")

#create scatterplots
plot(hr~freq)

#fitted anova model
fit_anova = lm(hr~f_med+f_high)
summary(fit_anova)

#residual analysis on fitted anova model
plot(fitted(fit_anova),rstandard(fit_anova), main="Predicted vs Studentized Residuals")
qqnorm(rstandard(fit_anova))
qqline(rstandard(fit_anova), col = 2)


#compute analysis of variance, removing f_med as it is not significant
fit=aov(hr~f_high, data=myd)
summary(fit)

#visualizing means - Plot Means with Error Bars
library(gplots)
plotmeans(hr~factor(f_high),main="Mean Plot\nwith 95% CI")
          
#pairwise comparisons using Tukey HSD tests
TukeyHSD(aovfit)
          
          
#full regression model with interaction effects
fit=lm(hr~f_med+f_high+h_high+f_med*h_high+f_high*h_high)
summary(fit)
anova(fit)

#backward selection procedure
library(MASS)
step <- stepAIC(fit, direction="backward")

#model selected by backward procedure
fit=lm(hr~f_med+f_high+h_high)
summary(fit)

#FINAL MODEL with additional variable removed
fit=lm(hr~f_high+h_high)
summary(fit)

#Tukey's multiple comparisons methods
TukeyHSD(aovfit)

#other visualizations
# line plot with error bars
library(gplots)
plotmeans(hr~freq,xlab="Frequency",
          ylab="heart rate", main="Mean Plot\nwith 95% CI")
# Two-way Interaction Plot
# defined by 
# interaction.plot(factor1, factor2, yvar, fun = mean, type = c("b"), etc...)
# where factor1 and factor2 are the two factors in analysis
# and yvar is measurement variable;

interaction.plot(height, freq, hr, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, 	
                 trace.label="Frequency",
                 xlab="Height", 
                 ylab="Heart Rate", 
                 main="Interaction Plot")
