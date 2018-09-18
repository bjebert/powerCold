# LM_004
# Predict ratio instead (consumption / mean_global)

meta <- fread("data/meta.csv")

# Functions ---------------------------------------------------------------

preprocess_data <- function(data) {
    data[, timestamp := fasttime::fastPOSIXct(timestamp)]
    data[, hour := hour(timestamp)]
    data[, month := month(timestamp)]
    data[, weekday := weekdays(timestamp)]
    
    data <- merge(data, meta, all.x = TRUE, sort = FALSE, by = 'series_id')    
    
    data[weekday == "Monday", is_today_off := monday_is_day_off]
    data[weekday == "Tuesday", is_today_off := tuesday_is_day_off]
    data[weekday == "Wednesday", is_today_off := wednesday_is_day_off]
    data[weekday == "Thursday", is_today_off := thursday_is_day_off]
    data[weekday == "Friday", is_today_off := friday_is_day_off]
    data[weekday == "Saturday", is_today_off := saturday_is_day_off]
    data[weekday == "Sunday", is_today_off := sunday_is_day_off]
    
    data[, c('monday_is_day_off', 'tuesday_is_day_off', 'wednesday_is_day_off', 'thursday_is_day_off', 
             'friday_is_day_off', 'saturday_is_day_off', 'sunday_is_day_off') := NULL]
    
    return(data)
}


get_predictions <- function(train_1, test_1, train_2, test_2, params = NULL) {
    train_1 <- preprocess_data(train_1)
    test_1 <- preprocess_data(test_1)
    
    train_2 <- preprocess_data(train_2)
    test_2 <- preprocess_data(test_2)
    
    test_2[, consumption := NULL]
    
    # Create features on train, then merge on to test (TRAIN SET)
    mean_hour <- train_1[, .(mean_hour = mean(consumption)), by = .(series_id, hour)]
    mean_day_hour <- train_1[, .(mean_day_hour = mean(consumption)), by = .(series_id, hour, weekday)]
    mean_global <- train_1[, .(mean_global = mean(consumption)), by = .(series_id)]
    
    test_1 <- merge(test_1, mean_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_1 <- merge(test_1, mean_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)
    test_1 <- merge(test_1, mean_global, by = c('series_id'), all.x = T, sort = F)
    
    # Create features on train, then merge on to test (TEST SET)
    mean_hour <- train_2[, .(mean_hour = mean(consumption)), by = .(series_id, hour)]
    mean_day_hour <- train_2[, .(mean_day_hour = mean(consumption)), by = .(series_id, hour, weekday)]
    mean_global <- train_2[, .(mean_global = mean(consumption)), by = .(series_id)]
    
    test_2 <- merge(test_2, mean_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_2 <- merge(test_2, mean_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)
    test_2 <- merge(test_2, mean_global, by = c('series_id'), all.x = T, sort = F)
    
    # Mean
    
    test_1[, consumption_ratio := consumption / mean_global]
    
    # Model
    
    mdl <- lm("consumption_ratio ~ I(mean_hour/mean_global) + I(mean_day_hour/mean_global) + I((mean_hour/mean_global) * (mean_day_hour/mean_global)) + 0", test_1)
    predictions <- predict(mdl, test_2)
    
    predictions <- predictions * test_2[, mean_global]
    
    mdl2 <- lm("consumption_ratio ~ I(mean_hour/mean_global) + 0", test_1)
    ind <- is.na(predictions)
    predictions[ind] <- predict(mdl2, test_2[ind]) * test_2[ind, mean_global]
    
    return(predictions)
    
    # x <- c('mean_hour', 'mean_day_hour')
    # 
    # Dtrain <- xgb.DMatrix(data = as.matrix(test_1[, x, with = FALSE]), label = test_1[["consumption"]])
    # Dtest <- xgb.DMatrix(data = as.matrix(test_2[, x, with = FALSE]))
    # 
    # params <- list(objective = "reg:linear",
    #                nrounds = 100,
    #                eta = 0.12,
    #                max_depth = 16)
    # 
    # mdl <- xgboost(data = Dtrain,
    #                params = params,
    #                nrounds = params[["nrounds"]])
    # 
    # predictions <- predict(mdl, Dtest)
    # predictions <- pmax(0, predictions)
    
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
