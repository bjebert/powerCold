library(data.table)
library(xgboost)
library(rBayesianOptimization)
set.seed(0)

source("~/utilities/col_convert.R")


# Functions ---------------------------------------------------------------



preprocess_data <- function(adv_data) {
    adv_data <- col_convert(adv_data, "integer", "numeric")  
    
    adv_data[, consumption_max := max(consumption), by = series_id]
    adv_data[, consumption_min := min(consumption), by = series_id]
    adv_data[, consumption_mean := mean(consumption), by = series_id]
    adv_data[, consumption_median := median(consumption)[1], by = series_id]
    adv_data[, consumption_sd := sd(consumption), by = series_id]
    adv_data[, consumption_fluctuation := (consumption_max - consumption_min) / consumption_max, by = series_id]
    
    adv_data[, temperature_max := max(temperature, na.rm = T), by = series_id]
    adv_data[, temperature_min := min(temperature, na.rm = T), by = series_id]
    adv_data[, temperature_mean := mean(temperature, na.rm = T), by = series_id]
    adv_data[, temperature_median := median(temperature, na.rm = T)[1], by = series_id]
    adv_data[, temperature_sd := sd(temperature, na.rm = T), by = series_id]
    adv_data[, temperature_fluctuation := (temperature_max - temperature_min) / temperature_max, by = series_id]
    
    adv_data[, timestamp := fasttime::fastPOSIXct(timestamp)]
    adv_data[, day := weekdays(timestamp)]
    adv_data[, hour := hour(timestamp)]
    
    by_day <- adv_data[, .(consumption = mean(consumption)), by = .(series_id, day)]
    
    by_day <- by_day[, .(daily_consumption_mean = mean(consumption),
                         daily_consumption_max = max(consumption),
                         daily_consumption_min = min(consumption),
                         daily_consumption_sd = sd(consumption)), by = series_id]
    
    by_day[, daily_consumption_fluctuation := (daily_consumption_max - daily_consumption_min) / daily_consumption_max]

    
    by_hour <- adv_data[, .(consumption = mean(consumption)), by = .(series_id, hour)]
    
    by_hour <- by_hour[, .(hourly_consumption_mean = mean(consumption),
                         hourly_consumption_max = max(consumption),
                         hourly_consumption_min = min(consumption),
                         hourly_consumption_sd = sd(consumption)), by = series_id]
    
    by_hour[, hourly_consumption_fluctuation := (hourly_consumption_max - hourly_consumption_min) / hourly_consumption_max]
    
    adv_data <- merge(adv_data, by_day, by = 'series_id')
    adv_data <- merge(adv_data, by_hour, by = 'series_id')
    
    return(adv_data)
}


get_predictions <- function(adv_train, adv_test) {
    adv_train <- preprocess_data(adv_train)
    adv_test <- preprocess_data(adv_test)
    
    x <- colnames(adv_train)
    x <- x[!x %in% c("series_id", "timestamp", "is_test", "series_N", "day")]
    
    Dtrain <- xgb.DMatrix(data = as.matrix(adv_train[, x, with = FALSE]), label = adv_train[["is_test"]])
    Dtest <- xgb.DMatrix(data = as.matrix(adv_test[, x, with = FALSE]))        
    
    params <- list(objective = "binary:logistic",
                   nrounds = 43,
                   eta = 0.4517,
                   max_depth = 2)    
    
    mdl <- xgboost(data = Dtrain,
                   params = params,
                   nrounds = params[["nrounds"]])    
    
    predict(mdl, Dtest)
}


# Bayesian optimisation ---------------------------------------------------
# 
# adv_train <- preprocess_data(adv_train)
# 
# x <- c("series_id", "consumption", "temperature")
# 
# uq <- unique(adv_train[, series_N])
# val_series <- sample(uq, round(0.3 * length(uq)))
# 
# train2 <- adv_train[!series_N %in% val_series]
# val2 <- adv_train[series_N %in% val_series]
# 
# Dtrain <- xgb.DMatrix(data = as.matrix(train2[, x, with = FALSE]), label = train2[["is_test"]])
# Dval <- xgb.DMatrix(data = as.matrix(val2[, x, with = FALSE]))    
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
#     preds <- predict(mdl, Dval)
#     
#     auc <- Metrics::auc(val2[["is_test"]], preds)
#     
#     list(Score = auc,
#          Pred = preds)
# }
# 
# BayesianOptimization(bayesian_eval,
#                      bounds = list(nrounds = c(5L, 500L),
#                                    eta = c(0.05, 0.5),
#                                    max_depth = c(1L, 20L)),
#                      init_points = 20,
#                      n_iter = 100)
# 
# 
# 
