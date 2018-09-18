library(data.table)
library(xgboost)
library(rBayesianOptimization)

source("~/utilities/col_convert.R")


# Functions ---------------------------------------------------------------


preprocess_data <- function(adv_data) {
    adv_data <- col_convert(adv_data, "integer", "numeric")  
    
    series_data <- adv_data[, .(consumption_max = max(consumption, na.rm = T),
                                consumption_min = min(consumption, na.rm = T),
                                consumption_mean = mean(consumption, na.rm = T),
                                consumption_median = median(consumption, na.rm = T)[1],
                                consumption_sd = sd(consumption, na.rm = T),
                                temperature_max = max(temperature, na.rm = T),
                                temperature_min = min(temperature, na.rm = T),
                                temperature_mean = mean(temperature, na.rm = T),
                                temperature_median = median(temperature, na.rm = T)[1],
                                temperature_sd = sd(temperature, na.rm = T)),
                            by = series_id]
    
    series_data[, consumption_fluctuation := (consumption_max - consumption_min) / consumption_max]
    series_data[, temperature_fluctuation := (temperature_max - temperature_min) / temperature_max]
    
    adv_data[, timestamp := fasttime::fastPOSIXct(timestamp)]
    adv_data[, day := weekdays(timestamp)]
    adv_data[, hour := hour(timestamp)]
    
    by_day <- adv_data[, .(consumption = mean(consumption)), by = .(series_id, day)]
    
    by_day <- by_day[, .(daily_consumption_mean = mean(consumption, na.rm = T),
                         daily_consumption_max = max(consumption, na.rm = T),
                         daily_consumption_min = min(consumption, na.rm = T),
                         daily_consumption_sd = sd(consumption, na.rm = T)), by = series_id]
    
    by_day[, daily_consumption_fluctuation := (daily_consumption_max - daily_consumption_min) / daily_consumption_max]
    
    
    by_hour <- adv_data[, .(consumption = mean(consumption)), by = .(series_id, hour)]
    
    by_hour <- by_hour[, .(hourly_consumption_mean = mean(consumption, na.rm = T),
                           hourly_consumption_max = max(consumption, na.rm = T),
                           hourly_consumption_min = min(consumption, na.rm = T),
                           hourly_consumption_sd = sd(consumption, na.rm = T)), by = series_id]
    
    by_hour[, hourly_consumption_fluctuation := (hourly_consumption_max - hourly_consumption_min) / hourly_consumption_max]
    
    series_data <- merge(series_data, by_day, by = 'series_id')
    series_data <- merge(series_data, by_hour, by = 'series_id')
    
    return(series_data)
}


get_predictions <- function(adv_train, adv_test, get_model = FALSE) {
    y_train <- adv_train[, .(is_test = mean(is_test)), by = series_id][order(series_id)]
    
    series_train <- preprocess_data(adv_train)[order(series_id)]
    series_test <- preprocess_data(adv_test)[order(series_id)]
    
    y_train_label <- merge(series_train, y_train, sort = FALSE)[["is_test"]]
    
    Dtrain <- xgb.DMatrix(data = as.matrix(series_train[, -'series_id']), label = y_train_label)
    Dtest <- xgb.DMatrix(data = as.matrix(series_test[, -'series_id']))        
    
    params <- list(objective = "binary:logistic",
                   nrounds = 500,
                   eta = 0.239,
                   max_depth = 9)    
    
    mdl <- xgboost(data = Dtrain,
                   params = params,
                   nrounds = params[["nrounds"]])    
    
    if(get_model) {
        return(mdl)
    }
    
    predictions <- predict(mdl, Dtest)
    
    return(predictions)
}


# Bayesian optimisation ---------------------------------------------------
# 
# bayesian_eval <- function(nrounds, eta, max_depth) {
#     params <- list(objective = "binary:logistic",
#                    nrounds = nrounds,
#                    eta = eta,
#                    max_depth = max_depth)
#     
#     mdl <- xgboost(data = Dtrain,
#                    params = params,
#                    nrounds = params[["nrounds"]],
#                    verbose = 0)
#     
#     preds <- predict(mdl, Dtest)
#     
#     error <- Metrics::auc(actuals, preds)
#     list(Score = error,
#          Pred = preds)
# }
# 
# BayesianOptimization(bayesian_eval,
#                      bounds = list(nrounds = c(50L, 500L),
#                                    eta = c(0.05, 0.7),
#                                    max_depth = c(1L, 20L)),
#                      init_points = 15,
#                      n_iter = 200)