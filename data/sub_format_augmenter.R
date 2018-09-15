# Submission format augmenter
# Convert submission format data such that we predict on an hourly basis, instead of daily or weekly

library(data.table)

# Code --------------------------------------------------------------------

submission_format <- fread("data/submission_format.csv")
submission_format[, timestamp := fasttime::fastPOSIXct(timestamp)]

daily <- submission_format[prediction_window == "daily", unique(series_id)]
weekly <- submission_format[prediction_window == "weekly", unique(series_id)]

submission_hourly <- copy(submission_format[prediction_window == "hourly"])
submission_hourly[, pred_id := NULL]
submission_hourly[, i := 1:.N, by = series_id]


for(id in daily) {
    sub_tmp <- submission_format[series_id == id]
    
    min_timestamp <- sub_tmp[, min(timestamp)]
    max_timestamp <- sub_tmp[, max(timestamp)] + 60 * 60 * 23  # Add 23 hours in the future that we have to predict
    
    new_timestamps <- seq(min_timestamp, max_timestamp, "hour")   
    i <- rep(1:(length(new_timestamps) / 24), each = 24)
    
    sub_augment <- data.table(series_id = sub_tmp[["series_id"]],
                              timestamp = new_timestamps,
                              consumption = 0,
                              prediction_window = sub_tmp[["prediction_window"]],
                              i = i)
    
    # Add temperature info
    sub_augment <- merge(sub_augment, sub_tmp[, .(timestamp, temperature)], by = 'timestamp', all.x = T)
    
    submission_hourly <- rbind(submission_hourly, sub_augment)
}

for(id in weekly) {
    sub_tmp <- submission_format[series_id == id]
    
    min_timestamp <- sub_tmp[, min(timestamp)]
    max_timestamp <- sub_tmp[, max(timestamp)] + 60 * 60 * 24 * 7 - 3600  # Add 6 days + 23 hours in the future that we have to predict
    
    new_timestamps <- seq(min_timestamp, max_timestamp, "hour")   
    i <- rep(1:(length(new_timestamps) / 168), each = 168)
    
    sub_augment <- data.table(series_id = sub_tmp[["series_id"]],
                              timestamp = new_timestamps,
                              consumption = 0,
                              prediction_window = sub_tmp[["prediction_window"]],
                              i = i)
    
    # Add temperature info
    sub_augment <- merge(sub_augment, sub_tmp[, .(timestamp, temperature)], by = 'timestamp', all.x = T)
    
    submission_hourly <- rbind(submission_hourly, sub_augment)
}

submission_hourly <- submission_hourly[order(series_id, timestamp)]

fwrite(submission_hourly, "data/submission_hourly.csv", row.names = F)
