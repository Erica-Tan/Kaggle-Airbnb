train_data <- read.csv("Data.csv")

library(ggplot2)
library(plyr)
library(dplyr)

#--gender plot 1
ggplot(train_data, aes(gender, fill=country_destination)) +
    geom_bar(position="identity") 


#--gender plot2
# Summarize a dataset by country_destination and gender
train.f1 <- group_by(train_data, country_destination, gender) %>%
    summarise(sum = n())

#percentage
train.f2 <- filter(train.f1, gender == 'FEMALE' | gender == 'MALE')

totalFemale <- sum(train.f2[train.f2$gender == 'FEMALE',]$sum)
totalMale <- sum(train.f2[train.f2$gender == 'MALE',]$sum)

train.female <- train.f2 %>% 
    filter(gender == 'FEMALE')%>% 
    mutate(percentage = sum / totalFemale*100)    

train.male <- train.f2 %>% 
    filter(gender == 'MALE')%>% 
    mutate(percentage = sum / totalMale*100)    


train.f2 <- rbind(train.female, train.male)

ggplot(train.f2, aes(x=reorder(country_destination, percentage), 
                     y=percentage, fill=gender)) + 
    geom_bar(position='dodge', stat='identity') +
    xlab('Destination country') +
    guides(fill=guide_legend(title=NULL))



#--age plot1
ggplot(train_data, aes(age, fill=country_destination)) +
    geom_bar(position="identity")

#--age plot2
train.f3 <- mutate(train_data, agegroup = factor (age > 45, labels = c("younger", "older")))

# Summarize a dataset by country_destination and agegroup
train.f4 <- group_by(train.f3, country_destination, agegroup) %>%
    summarise(sum = n())

totalYounger <- sum(train.f4[train.f4$agegroup == 'younger',]$sum)
totalOlder <- sum(train.f4[train.f4$agegroup == 'older',]$sum)

#percentage
train.young <- train.f4 %>% 
    filter(agegroup == 'younger')%>% 
    mutate(percentage = sum / totalYounger*100)    

train.old <- train.f4 %>% 
    filter(agegroup == 'older')%>% 
    mutate(percentage = sum / totalOlder*100)    

train.f4 <- rbind(train.young, train.old)

ggplot(train.f4, aes(x=reorder(country_destination, percentage), 
                     y=percentage, fill=agegroup)) + 
    geom_bar(position='dodge', stat='identity') +
    xlab('Destination country') +
    guides(fill=guide_legend(title=NULL))


#--compare destination defore and now
train.f5 <- mutate(train_data, first_active_peroid = factor (first_active_year > 2014, labels = c("Before 2014", "After 2014")))

# Summarize a dataset by country_destination and agegroup
train.f6 <- group_by(train.f5, country_destination, first_active_peroid) %>%
    summarise(sum = n())

totalBefore <- sum(train.f6[train.f6$first_active_peroid == 'Before 2014',]$sum)
totalAfter <- sum(train.f6[train.f6$first_active_peroid == 'After 2014',]$sum)

#percentage
train.before <- train.f6 %>% 
    filter(first_active_peroid == 'Before 2014')%>% 
    mutate(percentage = sum / totalBefore*100)    

train.after <- train.f6 %>% 
    filter(first_active_peroid == 'After 2014')%>% 
    mutate(percentage = sum / totalAfter*100)    

train.f7 <- rbind(train.before, train.after)

ggplot(train.f7, aes(x=reorder(country_destination, percentage), 
                     y=percentage, fill=first_active_peroid)) + 
    geom_bar(position='dodge', stat='identity') +
    xlab('Destination country') +
    guides(fill=guide_legend(title=NULL))






