X_train <- read.csv("data/train_users_2.csv")
X_test <- read.csv("data/test_users.csv")

X_test$country_destination <- "NDF"

#cleaning age
X_train$age[is.na(X_train$age)] <- -1
X_test$age[is.na(X_test$age)] <- -1

X_train$age[X_train$age > 1900] <- 2016 - X_train$age[X_train$age > 1900]
X_test$age[X_test$age > 1900] <- 2016 - X_test$age[X_test$age > 1900]

X_train$age[which(X_train$age > 110)] <- -1
X_test$age[which(X_test$age < 18)] <- -1


# combining browsers
train_users$first_browser <- as.character(train_users$first_browser)
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

X_train$first_browser <- factor(X_train$first_browser)
X_test$first_browser <- factor(X_test$first_browser)

#format date
X_train$date_account_created <- as.Date(X_train$date_account_created)
X_train$timestamp_first_active <- as.Date(strptime(X_train$timestamp_first_active, "%Y%m%d%H%M%S"), format = "%Y-%m-%d")

X_train$account_created_year <- format(X_train$date_account_created,'%Y')
X_train$account_created_month <- format(X_train$date_account_created,'%b')
X_train$first_active_year <- format(X_train$timestamp_first_active,'%Y')
X_train$first_active_month <- format(X_train$timestamp_first_active,'%b')

X_train$account_created_year <- factor(X_train$account_created_year)
X_train$account_created_month <- factor(X_train$account_created_month)
X_train$first_active_year <- factor(X_train$first_active_year)
X_train$first_active_month <- factor(X_train$first_active_month)

X_train$date_account_created <- NULL
X_train$timestamp_first_active <- NULL
X_train$date_first_booking <- NULL

X_test$date_account_created <- as.Date(X_test$date_account_created)
X_test$timestamp_first_active <- as.Date(strptime(X_test$timestamp_first_active, "%Y%m%d%H%M%S"), format = "%Y-%m-%d")

X_test$account_created_year <- format(X_test$date_account_created,'%Y')
X_test$account_created_month <- format(X_test$date_account_created,'%b')
X_test$first_active_year <- format(X_test$timestamp_first_active,'%Y')
X_test$first_active_month <- format(X_test$timestamp_first_active,'%b')

X_test$account_created_year <- factor(X_test$account_created_year)
X_test$account_created_month <- factor(X_test$account_created_month)
X_test$first_active_year <- factor(X_test$first_active_year)
X_test$first_active_month <- factor(X_test$first_active_month)

X_test$date_account_created <- NULL
X_test$timestamp_first_active <- NULL
X_test$date_first_booking <- NULL

#filter NA age and gender
train.f <- subset(train_users, !is.na(gender) & !is.na(age))

train.f <- train.f[, c(1,2,3,4,5,6,7,8,9,10,11,12,14,15
                           ,16,17,18,19,13)]

#save csv
write.csv(X_train, file = "data/X_train.csv", row.names = FALSE)
write.csv(X_test, file = "data/X_test.csv", row.names = FALSE)
