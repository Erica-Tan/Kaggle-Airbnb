X_train <- read.csv("data/train_users_2.csv")
X_test <- read.csv("data/test_users.csv")

# clean age
X_train$age[is.na(X_train$age)] <- -1
X_test$age[is.na(X_test$age)] <- -1

X_train$age[X_train$age > 1900] <- 2016 - X_train$age[X_train$age > 1900]
X_test$age[X_test$age > 1900] <- 2016 - X_test$age[X_test$age > 1900]

X_train$age[which(X_train$age > 110)] <- -1
X_train$age[which(X_train$age < 18)] <- -1
X_test$age[which(X_test$age > 110)] <- -1
X_test$age[which(X_test$age < 18)] <- -1

# combine browsers
X_train$first_browser <- as.character(X_train$first_browser)
X_test$first_browser <- as.character(X_test$first_browser)
X_train$first_browser[X_train$first_browser %in% c("Arora","Avant Browser","Camino",
                                                   "CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic",
                                                   "Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron",
                                                   "Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb",
                                                   "Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser",
                                                   "RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless",
                                                   "TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser",
                                                   "Android Browser","AOL Explorer","BlackBerry Browser","Silk", "Apple Mail")] <- "Other"
X_test$first_browser[X_test$first_browser %in% c("Arora","Avant Browser","Camino",
                                                 "CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic",
                                                 "Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron",
                                                 "Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb",
                                                 "Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser",
                                                 "RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless",
                                                 "TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser",
                                                 "Android Browser","AOL Explorer","BlackBerry Browser","Silk", "Apple Mail")] <- "Other"

X_train$first_browser[X_train$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
X_test$first_browser[X_test$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"

X_train$first_browser[X_train$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
X_test$first_browser[X_test$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"

X_train$first_browser[X_train$first_browser %in% c("IE","IE Mobile")] <- "IE"
X_test$first_browser[X_test$first_browser %in% c("IE","IE Mobile")] <- "IE"

X_train$first_browser[X_train$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
X_test$first_browser[X_test$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"

X_train$first_browser[X_train$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Firefox"
X_test$first_browser[X_test$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Firefox"

X_train$first_browser <- factor(X_train$first_browser)
X_test$first_browser <- factor(X_test$first_browser)


#format date
X_train$date_account_created <- as.Date(X_train$date_account_created)
X_train$timestamp_first_active <- as.Date(strptime(X_train$timestamp_first_active, "%Y%m%d%H%M%S"), format = "%Y-%m-%d")
X_test$date_account_created <- as.Date(X_test$date_account_created)
X_test$timestamp_first_active <- as.Date(strptime(X_test$timestamp_first_active, "%Y%m%d%H%M%S"), format = "%Y-%m-%d")


X_train$account_created_year <- format(X_train$date_account_created,'%Y')
X_train$account_created_month <- format(X_train$date_account_created,'%m')
X_train$account_created_day <- format(X_train$date_account_created,'%d')
X_train$first_active_year <- format(X_train$timestamp_first_active,'%Y')
X_train$first_active_month <- format(X_train$timestamp_first_active,'%m')
X_train$first_active_day <- format(X_train$timestamp_first_active,'%d')

X_test$account_created_year <- format(X_test$date_account_created,'%Y')
X_test$account_created_month <- format(X_test$date_account_created,'%m')
X_test$account_created_day <- format(X_test$date_account_created,'%d')
X_test$first_active_year <- format(X_test$timestamp_first_active,'%Y')
X_test$first_active_month <- format(X_test$timestamp_first_active,'%m')
X_test$first_active_day <- format(X_test$timestamp_first_active,'%d')


X_train$account_created_year <- factor(X_train$account_created_year)
X_train$account_created_month <- factor(X_train$account_created_month)
X_train$first_active_year <- factor(X_train$first_active_year)
X_train$first_active_month <- factor(X_train$first_active_month)


X_test$account_created_year <- factor(X_test$account_created_year)
X_test$account_created_month <- factor(X_test$account_created_month)
X_test$first_active_year <- factor(X_test$first_active_year)
X_test$first_active_month <- factor(X_test$first_active_month)

X_train$date_account_created <- NULL
X_train$timestamp_first_active <- NULL
X_train$date_first_booking <- NULL

X_test$date_account_created <- NULL
X_test$timestamp_first_active <- NULL
X_test$date_first_booking <- NULL


#load session datasets
#These three datasets are created at Airbnb_session.R file
sessions_action_type <- read.csv("Session/sessions_action_type_dcast.csv")
sessions_action_detail <- read.csv("Session/sessions_action_detail_dcast.csv")
sessions_action <- read.csv("Session/sessions_action_dcast2.csv")

sessions <- read.csv("Session/X_sessions.csv")

# keeping users present in sessions data
X_train1 <- subset(X_train, id %in% unique(sessions$id))
X_train <- subset(X_train, id %in% unique(sessions$id), select=c("id"))
X_test1 <- X_test
X_test <- subset(X_test, select=c("id"))

#merge with session
X_train <- merge(X_train, sessions_action, all.x=T, by="id")
X_test <- merge(X_test, sessions_action, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_type, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_type, all.x=T, by="id")

X_train <- merge(X_train, sessions_action_detail, all.x=T, by="id")
X_test <- merge(X_test, sessions_action_detail, all.x=T, by="id")

X_train[is.na(X_train)] <- 0
X_test[is.na(X_test)] <- 0

# remove duplicate columns
X_train <- X_train[,colnames(unique(as.matrix(X_train), MARGIN=2))]


# removing variables with less than 4 occurrences
train_ids <- X_train$id
X_train$id <- NULL
X_train <- as.data.frame(subset(X_train, select=c(names(X_train)
                                                  [which(colSums(X_train) > 4)])))
X_train$id <- train_ids


X_test <- X_test[, colnames(X_train)]

#combine others attributes
X_train <- merge(X_train1, X_train, all.x=T, by="id")
X_test <- merge(X_test1, X_test, all.x=T, by="id")

#move country_destination to the end
targets <- X_train$country_destination
X_train$country_destination <- NULL
X_train$country_destination <- targets

#save final dataset
write.csv(X_train, file = "data/Final_train.csv", row.names = FALSE)
write.csv(X_test, file = "data/Final_test.csv", row.names = FALSE)

#save binary dataset
X_train$country_destination <- ifelse(X_train$country_destination == "NDF" , "0", "1")
write.csv(X_train, file = "data/Binary_train.csv", row.names = FALSE)