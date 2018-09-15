library(data.table)
library(xgboost)
set.seed(0)

source("~/utilities/col_convert.R")


# Functions ---------------------------------------------------------------



preprocess_data <- function(adv_data) {
    adv_data <- col_convert(adv_data, "integer", "numeric")    
    return(adv_data)
}

get_predictions <- function(adv_train, adv_test) {
    adv_train <- preprocess_data(adv_train)
    adv_test <- preprocess_data(adv_test)
    
    x <- c("series_id", "consumption", "temperature")
    
    Dtrain <- xgb.DMatrix(data = as.matrix(adv_train[, x, with = FALSE]), label = adv_train[["is_test"]])
    Dtest <- xgb.DMatrix(data = as.matrix(adv_test[, x, with = FALSE]))        
    
    params <- list(objective = "binary:logistic",
                   nrounds = 100)    
    
    mdl <- xgboost(data = Dtrain,
                   params = params,
                   nrounds = params[["nrounds"]])    
    
    predict(mdl, Dtest)
}


