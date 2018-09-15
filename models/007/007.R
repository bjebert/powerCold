# 007
# 005 but median instead of mean


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
    
    # Create features on train, then merge on to test_2
    median_hour <- train_2[, .(median_hour = median(consumption)), by = .(series_id, hour)]
    median_day <- train_2[, .(median_day = median(consumption)), by = .(series_id, weekday)]
    median_day_hour <- train_2[, .(median_day_hour = median(consumption)), by = .(series_id, hour, weekday)]
    median_global <- train_2[, .(median_global = median(consumption)), by = .(series_id)]
    
    test_2 <- merge(test_2, median_hour, by = c('series_id', 'hour'), all.x = T, sort = F)
    test_2 <- merge(test_2, median_day, by = c('series_id', 'weekday'), all.x = T, sort = F)
    test_2 <- merge(test_2, median_day_hour, by = c('series_id', 'hour', 'weekday'), all.x = T, sort = F)
    test_2 <- merge(test_2, median_global, by = c('series_id'), all.x = T, sort = F)
    
    col_weights <- matrix(rep(params, times = nrow(test_2)),
                          ncol = 4, byrow = TRUE)
    
    col_weights[is.na(test_2[, .(median_hour, median_day, median_day_hour, median_global)])] <- 0
    col_weights <- t(apply(col_weights, 1, function(x) x / sum(x)))  # Normalise weights by row to sum to 1
    
    predictions <- rowSums(test_2[, .(median_hour, median_day, median_day_hour, median_global)] * col_weights, na.rm = T)   
    
    return(predictions)
}

