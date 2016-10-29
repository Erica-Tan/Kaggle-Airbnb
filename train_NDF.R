library(dplyr)
library(data.table)

sessions <- read.csv("data/sessions.csv", stringsAsFactors = FALSE)
train_users <- read.csv("data/X_train.csv", stringsAsFactors = FALSE)
test_users <- read.csv("data/X_test.csv", stringsAsFactors = FALSE)

#session
names(sessions)[1] <- "id"
sessions <- subset(sessions, id %in% c(unique(train_users$id), unique(test_users$id)))
sessions$count <- 1

#handle duplicate columns
action_types <- unique(sessions$action_type)
action_types <- action_types [action_types != '' & action_types != '-unknown-']


actions <- unique(sessions$action)
actions <- actions [actions != '' & actions != '-unknown-' 
                    & actions != '10' & actions != '11'
                    & actions != '12' & actions != '15']


action_details <- unique(sessions$action_detail)
action_details <- action_details [action_details != '' & action_details != '-unknown-' 
                    & action_details != '10' & action_details != '11'
                    & action_details != '12' & action_details != '15']

duplicate <- action_details[action_details %in% actions]

actions <- actions[!(actions %in% duplicate)]
action_details <- action_details[!(action_details %in% duplicate)]


sessions_action_type <- select(sessions, id, action_type, count)
sessions_action_type <- subset(sessions_action_type, action_type %in% action_types)

sessions_action <- select(sessions, id, action, count)
sessions_action <- subset(sessions_action, action %in% actions)


sessions_action_detail <- select(sessions, id, action_detail, count)
sessions_action_detail <- subset(sessions_action_detail, action_detail %in% action_details)

sessions_duplicate <- select(sessions, id, action, count)
sessions_duplicate <- subset(sessions_duplicate, action %in% duplicate)

write.csv(sessions_action_type, file = "sessions_action_type.csv", row.names = FALSE)
write.csv(sessions_action, file = "sessions_action.csv", row.names = FALSE)
write.csv(sessions_action_detail, file = "sessions_action_detail.csv", row.names = FALSE)
write.csv(sessions_duplicate, file = "sessions_duplicate.csv", row.names = FALSE)

# reshape data
sessions_action_type <- dcast(sessions_action_type, id ~ action_type, mean, value.var="count")
sessions_action <- dcast(sessions_action, id ~ action, mean, value.var="count")
sessions_action_detail <- dcast(sessions_action_detail, id ~ action_detail, mean, value.var="count")
sessions_duplicate <- dcast(sessions_duplicate, id ~ action, mean, value.var="count")

sessions_action_type[is.na(sessions_action_type)] <- 0
sessions_action[is.na(sessions_action)] <- 0
sessions_action_detail[is.na(sessions_action_detail)] <- 0
sessions_duplicate[is.na(sessions_duplicate)] <- 0


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

targets <- ifelse(X_train$country_destination == "NDF" , "NDF", "others")
X_train$country_destination <- NULL
X_train$country_destination <- targets


write.csv(X_train, file = "data/Train_DNF.csv", row.names = FALSE)
write.csv(X_test, file = "data/Test_DNF.csv", row.names = FALSE)





