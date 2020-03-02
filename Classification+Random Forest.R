#problem1

read.csv("churn_train.csv")
mytab=read.csv("churn_train.csv")

#define variables
Gender=mytab$GENDER
edu=mytab$EDUCATION
lastprice=mytab$LAST_PRICE_PLAN_CHNG_DAY_CNT
activeservices=mytab$TOT_ACTV_SRV_CNT
changesms=mytab$PCT_CHNG_IB_SMS_CNT
changebill=mytab$PCT_CHNG_BILL_AMT
churn=mytab$CHURN
complaint=mytab$COMPLAINT
age=mytab$AGE

sex.m=(Gender=='M')*1
mytab=cbind(mytab, sex.m)
print(mytab)

#1.a
#boxplots
boxplot(age~churn, ylab="age")
boxplot(changebill~churn, ylab="Pecentage change of bill amount")



#1.b
# logistic regression model fitted using glm() function with family=binomial
full <- glm(churn ~ edu + age + lastprice + activeservices + changesms + changebill +
              complaint + sex.m, data=mytab, family=binomial())

summary(full)

#1.b
# logistic regression model fitted using glm() function with family=binomial
full <- glm(churn ~ edu + age + lastprice + activeservices + changesms + changebill +
              complaint + sex.m, data=mytab, family=binomial())

summary(full)

# Run backward selection procedure for variable selection
# step() function works for both lm and glm objects
step.mod=step(full, direction=c("both")) 

#1.b and 1.c
M1= glm(churn ~ age + activeservices + changesms + changebill + complaint, data=mytab, family = binomial())
summary(M1)

predict(M1, type="response") # predicted values
residuals(M1, type="deviance") # residuals

#likelihood test
library(lmtest)
lrtest(M1)


predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals
plot(residuals(M1, type= "deviance"), pch=churn)

#plot of predicted probabilities vs months of experience
plot(age, fitted(fit), main="Logistic regression for churn vs. age")
#boxplot of predicted probabilities by task success
# useful visualization for classification purposes
boxplot(fitted(fit)~age)


#compute predicted probabilities given new values for x
newd = data.frame(age=c(43), lastprice=c(0), activeservices=c(4), changesms=c(1.04), changebill=c(1.19), complaint=c(1))
predict(M1,newdat=newd,type="response", se.M1=T) #type="response" for probabilities
exp(0.2251105)

confint(M1) # 95% CI for the coefficients
exp(coef(M1)) # compute exp(coefficients) to analyze change in odds for changes in X
exp(confint(M1)) # 95% CI for exp(coefficients), that is change in odds Waiting for profiling to be done...

# using functions in Classify_functions.R file to compute classification metrics
# file must be in same work directory.
source("Classify_functions.R")

# create variable y.train = observed values of Y in training set
y.train=churn

#compute predicted outcome based on probability threshold equal to 0.5
yc = classify(fitted(M1), 0.5)

# compares predicted oucomes with actual values in training set
m=compare(classify(fitted(M1), 0.50), y.train)
#classification matrix
m
#classification metrics
sensitivity(m)
accuracy(m)

###################################################
# the following code creates a list of thresholds and 
# computes classification metrics for each threshold
###################################################
#list of thresholds
probs=seq(0.3, 0.6, by= 0.05)
#list of predicted Y for each threshold in probs
predlist=lapply(probs, classify, plist=fitted(M1))
#list of classification matrices
listmat=lapply(predlist, compare, yvar=y.train)
#list of classification measures
mprecision=as.vector(lapply(listmat, precision), mode="numeric")
mrecall=as.vector(lapply(listmat, sensitivity), mode="numeric")
maccuracy=as.vector(lapply(listmat, accuracy), mode="numeric")
fmetric=2*mrecall*mprecision/(mrecall+mprecision)
cmat=cbind(probs,mprecision, mrecall, fmetric, maccuracy) 
colnames(cmat)=c("probs", "sensitivity", "precision", "f-metric","accuracy")
#summary of classification metrics by threshold values
cmat
#plot fmetric vs probability values
plot(cmat[,1], fmetric)

test.myd= read.csv("churn_test.csv")

colnames(test.myd)[4] <- "active_srv"
colnames(test.myd)[5] <- "age"
colnames(test.myd)[6] <- "PCT_CHNG_IB_SMS_CNT"
colnames(test.myd)[7] <- "PCT_CHNG_BILL_AMT"
colnames(test.myd)[8] <- "CHURN"
colnames(test.myd)[9] <- "COMPLAINT"

##############################################
## analysis suggests threshold equal to 0.40##
##############################################
# Apply classification techniques on testing set
################################################

#predicted outcomes in testing set
preds=as.vector(predict(M1,test.myd, type="response"))
#compute predicted outcome based on probability threshold equal to 0.35
ypred = classify(preds, 0.45)
ypred

# define y.test= observed values of Y in test set
y.test = test.myd$CHURN

# compares predicted oucomes with actual values in test set
m=compare(ypred, y.test)
#classification matrix
m
#classification metrics
sensitivity(m)
accuracy(m)
precision(m)






