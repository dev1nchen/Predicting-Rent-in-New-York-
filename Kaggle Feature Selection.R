
library(caret)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(car)
library(leaps)
library(glmnet)
library(Metrics)

# Read data and construct a simple model
setwd("~/Downloads/rentlala2021")
data = read.csv('analysisData.csv')

# Data Cleaning
data$host_response_rate <- as.numeric(sub("%", "",data$host_response_rate,fixed=TRUE))/100
data[is.na(data)] = 0

data <- data %>% select(price, host_response_rate, host_is_superhost, 
                        neighbourhood_cleansed, property_type, room_type, 
                        accommodates, bedrooms, bathrooms, beds, 
                        review_scores_cleanliness, review_scores_location, cleaning_fee)

# Lasso model
model = lm(price ~ host_response_rate + host_is_superhost + 
             neighbourhood_cleansed + property_type + room_type + 
             accommodates + bedrooms + bathrooms + beds + 
             review_scores_cleanliness + review_scores_location + cleaning_fee, data)


x <- model.matrix(price~host_response_rate + host_is_superhost + 
                    neighbourhood_cleansed + property_type + room_type + 
                    accommodates + bedrooms + bathrooms + beds + 
                    review_scores_cleanliness + review_scores_location + cleaning_fee,
                  data=data)
y = data$price
set.seed(1031)
cv_lasso = cv.glmnet(x = x, 
                     y = y, 
                     alpha = 1,
                     type.measure = 'mse')

lassoModel <- glmnet(x, y, alpha=1, standardize = TRUE)
cv.lasso <- cv.glmnet(x, y, alpha=1)
coef(cv.lasso, s = cv.lasso$lambda.1se) %>%
  round(4)
cv.lasso
summary(cv.lasso)


lasso_model = lm(
  price ~ host_response_rate + neighbourhood_cleansed +
    property_type + room_type +
    accommodates + bedrooms + bathrooms +
    review_scores_cleanliness + review_scores_location + cleaning_fee,
  data
)

vif(lasso_model)


# Best subset model

subsets <- regsubsets(price ~ host_response_rate + room_type +
                        accommodates + bedrooms + bathrooms +
                        review_scores_cleanliness + 
                        review_scores_location + cleaning_fee,
                      data = data, really.big = T)
summary(subsets)
coef(subsets,which.min(summary(subsets)$cp))
subset_lm <- lm(price ~ room_type +
                  accommodates + bedrooms + bathrooms +
                  review_scores_location + cleaning_fee,
                data)


# hybrid stepwise

empty_mod <- lm(price ~ 1, data=data)
full_mod  <- lm(price ~ host_response_rate + neighbourhood_cleansed +
                  property_type + room_type +
                  accommodates + bedrooms + bathrooms +
                  review_scores_cleanliness + review_scores_location + 
                  cleaning_fee, 
                data=data)
hybridStepwise <- step(empty_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='both')
summary(hybridStepwise)$r.squared

hybrid_lm <- lm(price ~ accommodates + neighbourhood_cleansed + cleaning_fee + 
                  property_type + room_type + bedrooms + bathrooms + review_scores_cleanliness + 
                  host_response_rate, data)