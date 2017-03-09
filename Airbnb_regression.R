train <- read.csv("data/Airbnb_train.csv")
test <- read.csv("data/Airbnb_test.csv")


indx <- sapply(train, is.integer)
train[indx] <- lapply(train[indx], function(x) as.numeric(x))

indx <- sapply(test, is.integer)
test[indx] <- lapply(test[indx], function(x) as.numeric(x))


target <- ifelse(train$country_destination == "NDF", 0, 1)


# Logistic regression
set.seed(415)
model_lr <- glm(target ~., data=train[, -c(1, 432)], family=binomial())

# predicting on train data
pred_lr <- predict(model_lr, train[, -c(1, 432)], type="response")
prediction <- ifelse(pred_lr > 0.5,1,0)

#accuracy
misClasificError <- mean(prediction != target)
print(paste('Accuracy',1-misClasificError))


# ROC
library(ROCR)
pr <- prediction(prediction, target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


train_lr <- data.frame(id = train$id, pred_lr = prediction)
write.csv(train_lr, file = "processed/train_lr.csv", row.names = FALSE)



# predicting on test data
pred_lr <- predict(model_lr, test[,-1], type="response")
prediction <- ifelse(pred_lr > 0.5,1,0)


test_lr <- data.frame(id = test$id, pred_lr = prediction)
write.csv(test_lr, file = "processed/test_lr.csv", row.names = FALSE)


