#Lodaing the dataset
churn_data <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
str(churn_data)

#converting categorical to numeric
churn_data <- churn_data[, -1]
churn_data$gender <- as.numeric(churn_data$gender)
churn_data$Partner <- as.numeric(churn_data$Partner)
churn_data$Dependents <- as.numeric(churn_data$Dependents)
churn_data$PhoneService <- as.numeric(churn_data$PhoneService)
churn_data$MultipleLines <- as.numeric(churn_data$MultipleLines)
churn_data$InternetService <- as.numeric(churn_data$InternetService)
churn_data$OnlineSecurity <- as.numeric(churn_data$OnlineSecurity)
churn_data$OnlineBackup <- as.numeric(churn_data$OnlineBackup)
churn_data$DeviceProtection <- as.numeric(churn_data$DeviceProtection)
churn_data$TechSupport <- as.numeric(churn_data$TechSupport)
churn_data$StreamingTV <- as.numeric(churn_data$StreamingTV)
churn_data$StreamingMovies <- as.numeric(churn_data$StreamingMovies)
churn_data$Contract <- as.numeric(churn_data$Contract)
churn_data$PaperlessBilling <- as.numeric(churn_data$PaperlessBilling)
churn_data$PaymentMethod <- as.numeric(churn_data$PaymentMethod)
churn_data$Churn <- ifelse(churn_data$Churn == 'Yes', 1, 0)

#splitting into training and test set
set.seed(100)
row_train <- sample(nrow(churn_data), 0.7*nrow(churn_data), replace = FALSE)
train_Set <- churn_data[row_train,]
test_Set <- churn_data[-row_train,]

#logistic regression
logis_regression <- glm(Churn ~ ., data = train_Set, family = 'binomial')

#Predicting the test set
logis_pred <- predict.glm(logis_regression, test_Set[, 1:19])
logis_pred <- ifelse(logis_pred <= 0, 0, 1)

#Calculating the accuracy
logistic_confusionmatrix <- table(test_Set$Churn, logis_pred)
accuracy_logisticmodel <- logistic_confusionmatrix[2, 2] / (logistic_confusionmatrix[2, 1] + logistic_confusionmatrix [2, 2])
print(accuracy)

#forward stepwise
forward1 <- step(logis_regression)
formula(forward1)
#predicting the test set for 2nd model
logis_pred2 <- predict.glm(forward1, test_Set[, 1:19])
logis_pred2 <- ifelse(logis_pred2 <= 0, 0, 1)
#Calculating the accuracy for the 2nd model
table(test_Set$Churn, logis_pred2)
logistic_confusionmatrix2 <- table(test_Set$Churn, logis_pred2)
accuracy_logisticmodel2 <- logistic_confusionmatrix2[2, 2] / (logistic_confusionmatrix2[2, 1] +
                                                                logistic_confusionmatrix2 [2, 2])
print(accuracy_logisticmodel2)

#Plotting the first logistic regression model
plot(logis_regression)

#LINEAR DISCRIMINANT ANALYSIS
n= nrow(churn_data)
i= sample(1:n, size = round(0.70*n), replace = FALSE)
trainchurn = churn_data[i,]
testchurn = churn_data[-i,]
View(churn_data)

ncol(testchurn)
nrow(testchurn)

library(MASS)

lda2=lda(Churn~., data= churn_data)
lda2
plot(lda2, col= " red")

churn_data.lda.p1 <- predict(lda2, newdata = churn_data[,c(1:19)])
churn_data.lda.p1
table(churn_data.lda.p1$class, churn_data[,20])
lda1=lda(Churn ~., data = trainchurn)
lda1
churn_data.lda.p <- predict(lda1, newdata = testchurn[,c(1:19)])
churn_data.lda.p

table(churn_data.lda.p$class, testchurn[,20])

## Random Forest Model

randfor_model <- randomForest(Churn ~ ., data = train_Set, ntree = 100)
randfor_model
randfor_model_predictedValues <- predict(randfor_model, test_Set[, 1:19])
confusion_matrix <- as.matrix(table(test_Set$Churn, randfor_model_predictedValues))
acuuracy_randfor <- confusion_matrix[2, 2] / (confusion_matrix[2, 1] + confusion_matrix[2, 2])

