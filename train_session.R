library(dplyr)
library(data.table)

train_users <- read.csv("data/X_train.csv", stringsAsFactors = FALSE)
test_users <- read.csv("data/X_test.csv", stringsAsFactors = FALSE)
sessions_action_type <- read.csv("sessions_action_type.csv", stringsAsFactors = FALSE)
sessions_action_detail <- read.csv("sessions_action_detail.csv", stringsAsFactors = FALSE)
sessions_action <- read.csv("sessions_action.csv", stringsAsFactors = FALSE)


# keeping users present in sessions data
X_train <- subset(train_users, id %in% unique(sessions$id), select=c("id"))
X_train1 <- subset(train_users, id %in% unique(sessions$id))
X_test <- subset(test_users, id %in% unique(sessions$id), select=c("id"))
X_test1 <- subset(test_users, id %in% unique(sessions$id))


#merge train
X_train <- merge(X_train, sessions_action, all.x=T, by="id")
X_train <- merge(X_train, sessions_action_type, all.x=T, by="id")
X_train <- merge(X_train, sessions_action_detail, all.x=T, by="id")
X_train <- merge(X_train, sessions_duplicate, all.x=T, by="id")
X_test <- merge(X_test, sessions_action, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_type, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_detail, all.x=T, by="id")
X_test <- merge(X_test, sessions_duplicate, all.x=T, by="id")


X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0

# removing variables with less than 4 occurrences
train_ids <- X_train$id
X_train$id <- NULL
X_train <- as.data.frame(subset(X_train, select=c(names(X_train)
                                                  [which(colSums(X_train) > 4)])))
X_train$id <- train_ids


X_test <- X_test[, names(X_train)]

#merge with train and test dataset
X_train <- merge(X_train1, X_train, all.x=T, by="id")
X_test <- merge(X_test1, X_test, all.x=T, by="id")
X_train$id <- NULL
X_test$id <- NULL

targets <- X_train$country_destination
formatTargets <- ifelse(X_train$country_destination == "NDF" , "NDF", "others")
X_train$country_destination <- NULL
X_train$country_destination <- formatTargets


write.csv(X_train, file = "data/Train_binary.csv", row.names = FALSE)


X_train$country_destination <- NULL
X_train$country_destination <- targets

write.csv(X_train, file = "data/Train.csv", row.names = FALSE)
write.csv(X_test, file = "data/Test.csv", row.names = FALSE)