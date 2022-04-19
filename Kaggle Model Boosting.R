library(ranger);library(rpart); library(rpart.plot);library(caret)

formula <- "price ~ cleaning_fee + zipcode + property_type + smart_location + 
            accommodates+ neighbourhood_cleansed + review_scores_location + room_type + 
            am"
# extra_people + guests_included

##boosting
library(vtreat)
library(missForest)


train <- train %>% select(price, cleaning_fee, zipcode, property_type, guests_included, extra_people, 
                          accommodates, neighbourhood_cleansed, review_scores_location, room_type, minimum_nights, 
                          review_scores_cleanliness, maximum_nights, maximum_minimum_nights, am)


train$zipcode <- as.factor(train$zipcode)
train$property_type <- as.factor(train$property_type)
train$neighbourhood_cleansed <- as.factor(train$neighbourhood_cleansed)
train$room_type <- as.factor(train$room_type)
train$cleaning_fee <- as.numeric(train$cleaning_fee)
train$minimum_nights <- as.numeric(train$minimum_nights)
train$review_scores_cleanliness <- as.numeric(train$review_scores_cleanliness)



train1 <- train %>% select(price, cleaning_fee, minimum_nights, review_scores_cleanliness, guests_included, extra_people, 
                           accommodates, room_type, maximum_nights, maximum_minimum_nights)

train1 <- missForest(train1, verbose = TRUE)
train1 <-  train1$ximp

train <- train[, -2]
train <- cbind(train, train1$cleaning_fee)
train <- train %>% rename('cleaning_fee' = 'train1$cleaning_fee')



# data(iris)
# summary(iris)
# 
# ## The data contains four continuous and one categorical variable.
# 
# ## Artificially produce missing values using the 'prodNA' function:
# set.seed(81)
# iris.mis <- prodNA(iris, noNA = 0.2)
# summary(iris.mis)
# 
# ## Impute missing values providing the complete matrix for
# ## illustration. Use 'verbose' to see what happens between iterations:
# iris.imp <- missForest(iris.mis, xtrue = iris, verbose = TRUE)




trt = designTreatmentsZ(dframe = train,
                        varlist = names(train)[c(2,3,4,5,6,7,8,9,14,15)])

newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = train,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt, 
                     dframe = scoringData,
                     varRestriction = newvars)
head(train_input)

library(xgboost); library(caret)
set.seed(617)
tune_nrounds = xgb.cv(data=as.matrix(train_input), 
                      label = train$price,
                      nrounds=250,
                      nfold = 5,
                      verbose = 0)

ggplot(data=tune_nrounds$evaluation_log, aes(x=iter, y=test_rmse_mean))+
  geom_point(size=0.4, color='sienna')+
  geom_line(size=0.1, alpha=0.1)+
  theme_bw()

which.min(tune_nrounds$evaluation_log$test_rmse_mean)

xgboost2= xgboost(data=as.matrix(train_input), 
                  label = train$price,
                  nrounds=which.min(tune_nrounds$evaluation_log$test_rmse_mean),
                  verbose = 0)
pred = predict(xgboost2, 
               newdata=as.matrix(test_input))




scoringData = read.csv('~/Downloads/rentlala2021/scoringData.csv')

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'predboostingkag8.csv',row.names = F)
