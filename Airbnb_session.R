library(dplyr)
library(data.table)

sessions <- read.csv("data/sessions.csv")

#session
X_train <- read.csv("data/train_users_2.csv")
X_test <- read.csv("data/test_users.csv")

names(sessions)[1] <- "id"
sessions <- subset(sessions, id %in% c(as.character(unique(X_train$id)), as.character(unique(X_test$id))))
sessions$count <- 1

length(c(as.character(unique(X_train$id)), as.character(unique(X_test$id))))

write.csv(sessions, file = "processed/X_sessions.csv", row.names = FALSE)

sessions <- read.csv("processed/X_sessions.csv", stringsAsFactors = FALSE)

#handle outliers
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


sessions_action_type <- select(sessions, id, action_type, count)
sessions_action_type <- subset(sessions_action_type, action_type %in% action_types)

sessions_action <- select(sessions, id, action, count)
sessions_action <- subset(sessions_action, action %in% actions)

sessions_action_detail <- select(sessions, id, action_detail, count)
sessions_action_detail <- subset(sessions_action_detail, action_detail %in% action_details)

write.csv(sessions_action_type, file = "processed/sessions_action_type.csv", row.names = FALSE)
write.csv(sessions_action, file = "processed/sessions_action.csv", row.names = FALSE)
write.csv(sessions_action_detail, file = "processed/sessions_action_detail.csv", row.names = FALSE)

sessions_action_type <- read.csv("processed/sessions_action_type.csv")
sessions_action <- read.csv("processed/sessions_action.csv")
sessions_action_detail <- read.csv("processed/sessions_action_detail.csv")

# reshape data
sessions_action_type <- dcast(sessions_action_type, id ~ action_type, mean, value.var="count")
sessions_action <- dcast(sessions_action, id ~ action, mean, value.var="count")
sessions_action_detail <- dcast(sessions_action_detail, id ~ action_detail, mean, value.var="count")

sessions_action_type[is.na(sessions_action_type)] <- 0
sessions_action[is.na(sessions_action)] <- 0
sessions_action_detail[is.na(sessions_action_detail)] <- 0

write.csv(sessions_action_type, file = "processed/sessions_action_type_dcast.csv", row.names = FALSE)
write.csv(sessions_action, file = "processed/sessions_action_dcast.csv", row.names = FALSE)
write.csv(sessions_action_detail, file = "processed/sessions_action_detail_dcast.csv", row.names = FALSE)

