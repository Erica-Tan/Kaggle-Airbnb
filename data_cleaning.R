train_users <- read.csv("data/train_users_2.csv")

#gender
train_users$gender <- as.character(train_users$gender)
train_users$gender[train_users$gender == '-unknown-'] <- NA
train_users$gender <- factor(train_users$gender)

#missing gender percentage
(sum(is.na(train_users$gender))) / nrow(train_users) * 100

#age
train_users$age[which(train_users$age > 1900)] <- 2016 - train_users$age[which(train_users$age > 1900)]
train_users$age[which(train_users$age > 110)] <- NA
train_users$age[which(train_users$age < 18)] <- NA

#date
train_users$date_account_created <- as.Date(train_users$date_account_created)
train_users$date_first_booking <- as.Date(train_users$date_first_booking, format = "%Y-%m-%d")
train_users$timestamp_first_active <- as.Date(strptime(train_users$timestamp_first_active, "%Y%m%d%H%M%S"), format = "%Y-%m-%d")

train_users$account_created_year <- format(train_users$date_account_created,'%Y')
train_users$account_created_month <- format(train_users$date_account_created,'%B')

train_users$first_active_year <- format(train_users$timestamp_first_active,'%Y')
train_users$first_active_month <- format(train_users$timestamp_first_active,'%B')

train_users$first_booking_year <- format(train_users$date_first_booking,'%Y')
train_users$first_booking_month <- format(train_users$date_first_booking,'%B')

train_users$account_created_year <- factor(train_users$account_created_year)
train_users$account_created_month <- factor(train_users$account_created_month)
train_users$first_active_year <- factor(train_users$first_active_year)
train_users$first_active_month <- factor(train_users$first_active_month)
train_users$first_booking_year <- factor(train_users$first_booking_year)
train_users$first_booking_month <- factor(train_users$first_booking_month)

#delete date attribues
train_users$date_account_created <- NULL
train_users$date_first_booking <- NULL
train_users$timestamp_first_active <- NULL

# combining browsers
train_users$first_browser <- as.character(train_users$first_browser)

train_users$first_browser[train_users$first_browser %in% c("Arora","Avant Browser","Camino",
                                                           "CometBird","Comodo Dragon","Conkeror","CoolNovo","Crazy Browser","Epic",
                                                           "Flock","Google Earth","Googlebot","IBrowse","IceDragon","IceWeasel","Iron",
                                                           "Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb",
                                                           "Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser",
                                                           "RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless",
                                                           "TenFourFox","TheWorld Browser","UC Browser","wOSBrowser","Yandex.Browser",
                                                           "Android Browser","AOL Explorer","BlackBerry Browser","Silk", "Apple Mail")] <- "Other"
train_users$first_browser[train_users$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
train_users$first_browser[train_users$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
train_users$first_browser[train_users$first_browser %in% c("IE","IE Mobile")] <- "IE"
train_users$first_browser[train_users$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
train_users$first_browser[train_users$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Opera"

train_users$first_browser[train_users$first_browser == '-unknown-'] <- NA
train_users$first_browser <- factor(train_users$first_browser)

#filter NA age and gender
train.f <- subset(train_users, !is.na(gender) & !is.na(age))

train.f <- train.f[, c(1,2,3,4,5,6,7,8,9,10,11,12,14,15
                           ,16,17,18,19,13)]

#save csv
write.csv(train.f, file = "Data.csv")
