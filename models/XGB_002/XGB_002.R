# XGB_002

library(xgboost)
library(RcppRoll)
meta <- fread("data/meta.csv")

# Functions ---------------------------------------------------------------

preprocess_data <- function(data) {
    data[, timestamp := fasttime::fastPOSIXct(timestamp)]
    data[, hour := hour(timestamp)]
    data[, month := month(timestamp)]
    data[, weekday := weekdays(timestamp)]
    data[, wday := wday(timestamp)]
    
    data <- merge(data, meta, all.x = TRUE, by = 'series_id')    
    
    data[weekday == "Monday", is_today_off := monday_is_day_off]
    data[weekday == "Tuesday", is_today_off := tuesday_is_day_off]
    data[weekday == "Wednesday", is_today_off := wednesday_is_day_off]
    data[weekday == "Thursday", is_today_off := thursday_is_day_off]
    data[weekday == "Friday", is_today_off := friday_is_day_off]
    data[weekday == "Saturday", is_today_off := saturday_is_day_off]
    data[weekday == "Sunday", is_today_off := sunday_is_day_off]
    
    data[, weekday := NULL]
    data[, c('monday_is_day_off', 'tuesday_is_day_off', 'wednesday_is_day_off', 'thursday_is_day_off', 
             'friday_is_day_off', 'saturday_is_day_off', 'sunday_is_day_off') := NULL]
    
    return(data)
}


preprocess_train <- function(train_data) {
    train_data[, c("value_last_mean_60", "value_last_mean_192", 
                          "value_last_sd_60", "value_last_sd_192",
                          "temperature_last_mean_60", "temperature_last_mean_192") :=
                          list(roll_mean(consumption, n = 60, align = "right", fill = NA, na.rm = T),
                               roll_mean(consumption, n = 192, align = "right", fill = NA, na.rm = T),
                               roll_sd(consumption, n = 60, align = "right", fill = NA, na.rm = T),
                               roll_sd(consumption, n = 192, align = "right", fill = NA, na.rm = T),
                               roll_mean(temperature, n = 60, align = "right", fill = NA, na.rm = T),
                               roll_mean(temperature, n = 192, align = "right", fill = NA, na.rm = T)),
                      by = series_id]
    
    train_data[, c("dow_mean_3", "dow_mean_2") :=
                   list(roll_mean(consumption, n = 3, align = "right", fill = NA, na.rm = T),
                        roll_mean(consumption, n = 2, align = "right", fill = NA, na.rm = T)),
               by = .(series_id, wday)]
    
    
    train_data[, c("hour_mean_7", "hour_mean_14") :=
                   list(roll_mean(consumption, n = 7, align = "right", fill = NA, na.rm = T),
                        roll_mean(consumption, n = 14, align = "right", fill = NA, na.rm = T)),
               by = .(series_id, hour)]
    
    return(train_data)
}


get_predictions <- function(train_1, test_1, train_2, test_2, params = NULL) {
    test_2[, consumption := NULL]
    
    train_1 <- preprocess_data(train_1)
    test_1 <- preprocess_data(test_1)
    train_2 <- preprocess_data(train_2)
    test_2 <- preprocess_data(test_2)
    
    train_1 <- preprocess_train(train_1)
    train_2 <- preprocess_train(train_2)
    test_1 <- preprocess_train(test_1)
    
    # TODO
    # 
    # x <- colnames(train_1)
    # 
    # params <- list(objective = "reg:linear",
    #                nrounds = 54,
    #                eta = 0.55,
    #                max_depth = 10)
    # 
    # mdl <- xgboost(data = Dtrain,
    #                params = params,
    #                nrounds = params[["nrounds"]])
    # 
    # predictions <- predict(mdl, Dtest)
    # 
    # 
    # return(predictions)
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
#     test <- copy(test_2)
#     test[, predictions := preds]
#     test[, m := mean(consumption), by = series_id]
# 
#     nmae <- mean(test[, abs(predictions - consumption) / m])
# 
#     list(Score = -nmae,
#          Pred = preds)
# }
# 
# BayesianOptimization(bayesian_eval,
#                      bounds = list(nrounds = c(30L, 500L),
#                                    eta = c(0.05, 0.7),
#                                    max_depth = c(1L, 20L)),
#                      init_points = 10,
#                      n_iter = 50)



