data <- read.csv("~/Downloads/rentlala2021/analysisData.csv", stringsAsFactors=TRUE)
dim(data)
scoringData = read.csv('~/Downloads/rentlala2021/scoringData.csv')
scoringData$zipcode <- as.character(scoringData$zipcode)

library(dplyr)
## combining data to jointly deal with factor labels and missing data
combinedData <- bind_rows(data, scoringData)


hist(as.numeric(data$zipcode))
## modify zip+4
combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
combinedData$zipcode <- as.factor(combinedData$zipcode)
combinedData$zipcode <- forcats::fct_lump_n(combinedData$zipcode, 120)
#209

combinedData$property_type <- as.factor(combinedData$property_type)
combinedData$property_type <- forcats::fct_lump_n(combinedData$property_type, 35)


combinedData$neighbourhood_cleansed <- as.factor(combinedData$neighbourhood_cleansed)
combinedData$neighbourhood_cleansed <- forcats::fct_lump_n(combinedData$neighbourhood_cleansed, 150)



combinedData <- combinedData %>% mutate(gar = case_when(
  grepl("Garden", amenities) == TRUE ~ 1,
  grepl("Garden", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(wifi = case_when(
  grepl("Wifi", amenities) == TRUE ~ 1,
  grepl("Wifi", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(kit = case_when(
  grepl("Kitchen", amenities) == TRUE ~ 1,
  grepl("Kitchen", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(el = case_when(
  grepl("Elevator", amenities) == TRUE ~ 1,
  grepl("Elevator", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(gym = case_when(
  grepl("Gym", amenities) == TRUE ~ 1,
  grepl("Gym", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(tv = case_when(
  grepl("TV", amenities) == TRUE ~ 1,
  grepl("TV", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(laptop = case_when(
  grepl("Laptop", amenities) == TRUE ~ 1,
  grepl("Laptop", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(coffee = case_when(
  grepl("Coffee", amenities) == TRUE ~ 1,
  grepl("Coffee", amenities) == FALSE ~ 0
))

combinedData <- combinedData %>% mutate(am = gar + wifi + kit + el + gym + tv + laptop + coffee)

#308

# # numeric variables median imputation
# numeric_predictors <- which(colnames(combinedData) != "price" & 
#                               sapply(combinedData, is.numeric))
# library(caret)
# imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
# combinedData[,numeric_predictors] <- predict(imp_model_med, newdata=combinedData[,numeric_predictors])

library(psych)
data1 <- data
data1$price = as.numeric(data1$price)
corpredictors <- select_if(data1, is.numeric)
corpredictors <- corpredictors %>% select(price, review_scores_location, review_scores_accuracy,
                                          review_scores_cleanliness, review_scores_communication,
                                          review_scores_value, extra_people, accommodates,
                                          cleaning_fee)
corPlot(corpredictors, cex = 0.8)

combinedData <- combinedData %>% select(price, cleaning_fee, zipcode, property_type, guests_included, extra_people, 
                                        accommodates, neighbourhood_cleansed, review_scores_location, room_type, minimum_nights, 
                                        review_scores_cleanliness, maximum_nights, maximum_minimum_nights, am)

## split back
train <- combinedData[!is.na(combinedData$price),]
scoringData <- combinedData[is.na(combinedData$price),]




