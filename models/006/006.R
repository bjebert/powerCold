# 006
# Remove series_id from 004

library(xgboost)

# Functions ---------------------------------------------------------------

preprocess_data <- function(data) {
    data[, timestamp := fasttime::fastPOSIXct(timestamp)]
    data[, hour := hour(timestamp)]
    data[, month := month(timestamp)]
    data[, weekday := weekdays(timestamp)]
    data[, wday := wday(timestamp)]
}


get_predictions <- function(train_1, test_1, train_2, test_2, params = NULL) {
    train_1 <- preprocess_data(train_1)
    test_1 <- preprocess_data(test_1)
    
    train_2 <- preprocess_data(train_2)
    test_2 <- preprocess_data(test_2)
    
    # Create features on train, then merge on to test (TRAIN SET)
    mean_hour <- train_1[, .(mean_hour = mean(consumption)), by = .(series_id, hour)]
    mean_day <- train_1[, .(mean_day = mean(consumption)), by = .(series_id, weekday)]
    mean_day_hour <- train_1[, .(mean_day_hour = mean(consumption)), by = .(series_id, hour, weekday)]
    mean_global <- train_1[, .(mean_global = mean(consumption)), by = .(series_id)]
    
    test_1 <- merge(test_1, mean_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_1 <- merge(test_1, mean_day, by = c('series_id', 'weekday'), all.x = T, sort = F)
    test_1 <- merge(test_1, mean_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)
    test_1 <- merge(test_1, mean_global, by = c('series_id'), all.x = T, sort = F)
    
    # Create features on train, then merge on to test (TEST SET)
    mean_hour <- train_2[, .(mean_hour = mean(consumption)), by = .(series_id, hour)]
    mean_day <- train_2[, .(mean_day = mean(consumption)), by = .(series_id, weekday)]
    mean_day_hour <- train_2[, .(mean_day_hour = mean(consumption)), by = .(series_id, hour, weekday)]
    mean_global <- train_2[, .(mean_global = mean(consumption)), by = .(series_id)]
    
    test_2 <- merge(test_2, mean_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_2 <- merge(test_2, mean_day, by = c('series_id', 'weekday'), all.x = T, sort = F)
    test_2 <- merge(test_2, mean_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)
    test_2 <- merge(test_2, mean_global, by = c('series_id'), all.x = T, sort = F)
    
    x <- c('temperature', 'hour', 'wday', 'mean_hour', 'mean_day', 'mean_day_hour', 'mean_global')
    
    Dtrain <- xgb.DMatrix(data = as.matrix(test_1[, x, with = FALSE]), label = test_1[["consumption"]])
    Dtest <- xgb.DMatrix(data = as.matrix(test_2[, x, with = FALSE]))
   
    params <- list(objective = "reg:linear",
                   nrounds = 100,
                   eta = 0.12,
                   max_depth = 16)
    
    mdl <- xgboost(data = Dtrain,
                   params = params,
                   nrounds = params[["nrounds"]])
    
    predictions <- predict(mdl, Dtest)
    predictions <- pmax(0, predictions)
    
    return(predictions)
}


# Bayesian ----------------------------------------------------------------
# 
# library(rBayesianOptimization)
# 
# bayesian_eval <- function(nrounds, eta, max_depth) {
#     params <- list(objective = "reg:linear",
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
#     val <- copy(val_2)
#     val[, predictions := preds]
#     val[, m := mean(consumption), by = series_id]
# 
#     nmae <- mean(val[, abs(predictions - consumption) / m])
# 
#     list(Score = -nmae,
#          Pred = preds)
# }
# 
# BayesianOptimization(bayesian_eval,
#                      bounds = list(nrounds = c(30L, 500L),
#                                    eta = c(0.05, 0.7),
#                                    max_depth = c(1L, 20L)),
#                      init_points = 30,
#                      n_iter = 200)
# 
# 
# 
