# 007
# median, mean of hour/day_hour, convert to log before summarising


preprocess_data <- function(data) {
    data[, timestamp := fasttime::fastPOSIXct(timestamp)]
    data[, hour := hour(timestamp)]
    data[, month := month(timestamp)]
    data[, weekday := weekdays(timestamp)]
}


get_predictions <- function(train_1, test_1, train_2, test_2, params = NULL) {
    train_1 <- preprocess_data(train_1)
    test_1 <- preprocess_data(test_1)
    
    train_2 <- preprocess_data(train_2)
    test_2 <- preprocess_data(test_2)
    
    train_2[, consumption := log1p(consumption)]
    
    # Create features on train, then merge on to test_2
    mean_hour <- train_2[, .(mean_hour = mean(consumption)), by = .(series_id, hour)]
    mean_day_hour <- train_2[, .(mean_day_hour = mean(consumption)), by = .(series_id, hour, weekday)]   
    median_hour <- train_2[, .(median_hour = median(consumption)), by = .(series_id, hour)]
    median_day_hour <- train_2[, .(median_day_hour = median(consumption)), by = .(series_id, hour, weekday)]
    
    test_2 <- merge(test_2, mean_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_2 <- merge(test_2, mean_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)    
    test_2 <- merge(test_2, median_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_2 <- merge(test_2, median_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)
    
    weights <- c(0.2283, 1, 0, 0.4868)
    col_weights <- matrix(rep(weights, times = nrow(test_2)),
                          ncol = 4, byrow = TRUE)
    
    col_weights[is.na(test_2[, .(mean_hour, mean_day_hour, median_hour, median_day_hour)])] <- 0
    col_weights <- t(apply(col_weights, 1, function(x) x / sum(x)))  # Normalise weights by row to sum to 1
    
    predictions <- rowSums(test_2[, .(mean_hour, mean_day_hour, median_hour, median_day_hour)] * col_weights, na.rm = T)   
    
    predictions <- 0.9936 * expm1(predictions)
    
    return(predictions)
}

