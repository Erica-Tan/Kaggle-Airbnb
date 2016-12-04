train <- read.csv("data/Airbnb_train.csv")
test <- read.csv("data/Airbnb_test.csv")
train_lr <- read.csv("data/train_lr.csv")
test_lr <- read.csv("data/test_lr.csv")

indx <- sapply(train, is.integer)
train[indx] <- lapply(train[indx], function(x) as.numeric(x))

indx <- sapply(test, is.integer)
test[indx] <- lapply(test[indx], function(x) as.numeric(x))

train$pred_lr <- train_lr$pred_lr
test$pred_lr <- test_lr$pred_lr

#move country_destination to the end
targets <- train$country_destination
train$country_destination <- NULL
train$country_destination <- targets


target <- as.numeric(as.factor(train$country_destination)) - 1

library(xgboost)
set.seed(415)
model_xgb <- xgboost(data.matrix(train[, -c(1, 433)]), 
                     data.matrix(target), 
                     objective="multi:softprob", 
                     num_class=12, 
                     nrounds=60, 
                     eta=0.05, 
                     max_depth=5, 
                     subsample=0.9, 
                     colsample_bytree=0.5, 
                     min_child_weight=5, 
                     eval_metric='mlogloss')


# predict test
pred <- predict(model_xgb, data.matrix(test[,-1]))

# extract the 5 classes with highest probabilities
pred_matrix <- as.data.frame(matrix(pred, nrow=12))
rownames(pred_matrix) <- c("AU","CA","DE","ES","FR","GB","IT","NDF","NL","other","PT","US")
predictions_top5  <- as.vector(apply(pred_matrix, 2, function(x) names(sort(x)[12:8])))


# create submission 
ids <- NULL
for (i in 1:NROW(test)) {
    idx <- as.character(test$id[i])
    ids <- append(ids, rep(idx,5))
}


submission <- NULL
submission$id <- ids
submission$country <- predictions_top5
submission <- as.data.frame(submission)


# saving submission
write.csv(submission, "submit.csv", row.names=F)

