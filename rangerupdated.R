data <- read.csv("~/Downloads/rentlala2021/analysisData.csv", stringsAsFactors=TRUE)
dim(data)
scoringData = read.csv('~/Downloads/rentlala2021/scoringData.csv')
scoringData$zipcode <- as.character(scoringData$zipcode)

library(dplyr)
## combining data to jointly deal with factor labels and missing data
combinedData <- bind_rows(data, scoringData)


# data cleaning

## zip code
hist(as.numeric(data$zipcode))
combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
combinedData$zipcode <- as.factor(combinedData$zipcode)
combinedData$zipcode <- forcats::fct_lump_n(combinedData$zipcode, 120)
#209

##property type
combinedData$property_type <- as.factor(combinedData$property_type)
combinedData$property_type <- forcats::fct_lump_n(combinedData$property_type, 35)

## neighbourhood_cleansed
combinedData$neighbourhood_cleansed <- as.factor(combinedData$neighbourhood_cleansed)
combinedData$neighbourhood_cleansed <- forcats::fct_lump_n(combinedData$neighbourhood_cleansed, 150)


combinedData <- combinedData %>% select(price, cleaning_fee, zipcode, property_type, guests_included, extra_people, 
                                        accommodates, neighbourhood_cleansed, review_scores_location, room_type, minimum_nights, 
                                        review_scores_cleanliness, maximum_nights, maximum_minimum_nights)

## split back
train <- combinedData[!is.na(combinedData$price),]
scoringData <- combinedData[is.na(combinedData$price),]


library(ranger);library(rpart); library(rpart.plot);library(caret)



# boosting model
library(vtreat)
library(missForest)

## filling out the missing value 
train <- train %>% select(price, cleaning_fee, zipcode, property_type, guests_included, extra_people, 
                          accommodates, neighbourhood_cleansed, review_scores_location, room_type, minimum_nights, 
                          review_scores_cleanliness, maximum_nights, maximum_minimum_nights)


train$zipcode <- as.factor(train$zipcode)
train$property_type <- as.factor(train$property_type)
train$neighbourhood_cleansed <- as.factor(train$neighbourhood_cleansed)
train$room_type <- as.factor(train$room_type)
train$cleaning_fee <- as.numeric(train$cleaning_fee)
train$minimum_nights <- as.numeric(train$minimum_nights)
train$review_scores_cleanliness <- as.numeric(train$review_scores_cleanliness)



train1 <- train %>% select(price, property_type, cleaning_fee, minimum_nights, review_scores_cleanliness, guests_included, extra_people, 
                           accommodates, room_type, maximum_nights, maximum_minimum_nights)

train1 <- missForest(train1, verbose = TRUE)
train1 <-  train1$ximp

train <- train[, -2]
train <- cbind(train, train1$cleaning_fee)
train <- train %>% rename('cleaning_fee' = 'train1$cleaning_fee')


## model feature selection

formula <- "price ~ cleaning_fee + zipcode + property_type + guests_included + extra_people + 
            accommodates+ neighbourhood_cleansed + review_scores_location + room_type"
##cleaning_fee + zipcode + property_type + accommodates+ neighbourhood_cleansed + review_scores_location + room_type
trControl <- trainControl(method="cv",number=5)
tuneGrid <- expand.grid(mtry=c(5,7,9),
                        splitrule = c('variance','extratrees','maxstat'),
                        min.node.size = c(5,15,30))

set.seed(617)

cvModel = train(as.formula(formula),
                data=train,
                method="ranger",
                num.trees = 1000, 
                trControl=trControl,
                tuneGrid=tuneGrid,
                na.action = na.exclude)

cvModel
cv_forest_ranger = ranger(as.formula(formula),
                          data=train,
                          num.trees = 1000, 
                          mtry=cvModel$bestTune$mtry, 
                          min.node.size = cvModel$bestTune$min.node.size, 
                          splitrule = cvModel$bestTune$splitrule,
                          na.action = na.pass)
cv_forest_ranger


scoringData = read.csv('~/Downloads/rentlala2021/scoringData.csv')

train2 <- scoringData %>% select(cleaning_fee, property_type, minimum_nights, review_scores_cleanliness, guests_included, extra_people, 
                           accommodates, room_type, maximum_nights, maximum_minimum_nights)

train2$property_type <- as.factor(train2$property_type)
train2$room_type <- as.factor(train2$room_type)
train2$cleaning_fee <- as.numeric(train2$cleaning_fee)
train2$minimum_nights <- as.numeric(train2$minimum_nights)
train2$review_scores_cleanliness <- as.numeric(train2$review_scores_cleanliness)


train2 <- missForest(train2, verbose = TRUE)
train2 <- train2$ximp

scoringData <- scoringData[, -50]
scoringData <- cbind(scoringData, train2$cleaning_fee)
scoringData <- scoringData %>% rename('cleaning_fee' = 'train2$cleaning_fee')

pred <- predict(cv_forest_ranger, scoringData)$predictions



# Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'pred8kag.csv',row.names = F)


















