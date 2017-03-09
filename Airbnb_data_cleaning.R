X_train <- read.csv("data/train_users_2.csv")
X_test <- read.csv("data/test_users.csv")

X_test$country_destination <- NA
combi  <- rbind(X_train, X_test)


# clean age
combi$age[is.na(combi$age)] <- -1
combi$age[combi$age > 1900] <- 2016 - combi$age[combi$age > 1900]
combi$age[which(combi$age > 110)] <- -1
combi$age[which(combi$age < 18)] <- -1

# combine browsers
combi$first_browser <- as.character(combi$first_browser)
combi$first_browser[combi$first_browser %in% c("Arora","Avant Browser","Camino",
                                               "CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic",
                                               "Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron",
                                               "Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb",
                                               "Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser",
                                               "RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless",
                                               "TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser",
                                               "Android Browser","AOL Explorer","BlackBerry Browser","Silk", "Apple Mail")] <- "Other"
combi$first_browser[combi$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
combi$first_browser[combi$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
combi$first_browser[combi$first_browser %in% c("IE","IE Mobile")] <- "IE"
combi$first_browser[combi$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
combi$first_browser[combi$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Firefox"
combi$first_browser <- factor(combi$first_browser)


#format date
combi$date_account_created <- as.Date(combi$date_account_created)
combi$timestamp_first_active <- as.Date(strptime(combi$timestamp_first_active, "%Y%m%d%H%M%S"), format = "%Y-%m-%d")

combi$account_created_year <- format(combi$date_account_created,'%Y')
combi$account_created_month <- format(combi$date_account_created,'%m')
combi$account_created_day <- format(combi$date_account_created,'%d')
combi$first_active_year <- format(combi$timestamp_first_active,'%Y')
combi$first_active_month <- format(combi$timestamp_first_active,'%m')
combi$first_active_day <- format(combi$timestamp_first_active,'%d')

combi$account_created_year <- factor(combi$account_created_year)
combi$account_created_month <- factor(combi$account_created_month)
combi$first_active_year <- factor(combi$first_active_year)
combi$first_active_month <- factor(combi$first_active_month)


combi$date_account_created <- NULL
combi$timestamp_first_active <- NULL
combi$date_first_booking <- NULL


#load session datasets
#These three datasets are created at Airbnb_session.R file
sessions_action_type <- read.csv("processed/sessions_action_type_dcast.csv")
sessions_action_detail <- read.csv("processed/sessions_action_detail_dcast.csv")
sessions_action <- read.csv("processed/sessions_action_dcast2.csv")

results <- combi$country_destination

combi1 <- combi
combi <- subset(combi, select=c("id"))

combi$order <- seq(1, nrow(combi))

#merge train
combi <- merge(combi, sessions_action, all.x=T, by="id")
combi <- merge(combi, sessions_action_type, all.x=T, by="id")
combi <- merge(combi, sessions_action_detail, all.x=T, by="id")

combi[is.na(combi)] <- 0

# removing variables with less than 4 occurrences
combi_ids <- combi$id
combi$id <- NULL
combi <- as.data.frame(subset(combi, select=c(names(combi)
                                              [which(colSums(combi[1:213451,]) > 4)])))
combi$id <- combi_ids

# remove duplicate columns
combi <- combi[,colnames(unique(as.matrix(combi), MARGIN=2))]


#combine other attributes
combi2 <- merge(combi1, combi, all.x=T, by="id")
combi2 <- combi2[order(combi2$order),]
combi2$order <- NULL


# one-hot-encoding features
library(caret)
ohe.feats = c('language', 'first_browser', 'gender', 'signup_method', 'affiliate_channel', 'affiliate_provider', 
              'first_affiliate_tracked', 'signup_app', 'first_device_type')
dummies <- dummyVars(~ language + first_browser + gender + signup_method + affiliate_channel + affiliate_provider 
                     + first_affiliate_tracked + signup_app + first_device_type, 
                     data = combi2)
df.ohe <- as.data.frame(predict(dummies, newdata = combi2))
combi3 <- cbind(combi2[,-c(which(colnames(combi2) %in% ohe.feats))], df.ohe)


#move country_destination to the end
targets <- combi3$country_destination
combi3$country_destination <- NULL
combi3$country_destination <- targets


write.csv(combi3, file = "processed/Airbnb.csv", row.names = FALSE)


train <- combi3[1:213451,]
test <- combi3[213452:275547,]



#save final dataset
write.csv(train, file = "processed/Airbnb_train.csv", row.names = FALSE)
test$country_destination <- NULL
write.csv(test, file = "processed/Airbnb_test.csv", row.names = FALSE)

