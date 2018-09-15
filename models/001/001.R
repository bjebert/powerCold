# 001
# Create a weighted mean based on day and hour consumption records

library(fasttime)
library(data.table)

train <- fread("data/train.csv")
test <- fread("data/test.csv")
submission_format <- fread("data/submission_format.csv")
sub_temp <- copy(submission_format)

test[, timestamp := fastPOSIXct(timestamp)]
test[, day := weekdays(timestamp)]
test[, hour := hour(timestamp)]

sub_temp[, timestamp := fastPOSIXct(timestamp)]
sub_temp[, day := weekdays(timestamp)]
sub_temp[, hour := hour(timestamp)]
sub_temp[, month := month(timestamp)]

test <- merge(test, unique(sub_temp[, c('series_id', 'prediction_window')]), by = "series_id")

mean_hour <- test[, .(meanHour = mean(consumption)), by = .(series_id, hour)]
mean_day_hour <- test[, .(meanDayHour = mean(consumption)), by = .(series_id, day, hour)]
mean_day <- test[, .(meanDay = mean(consumption)), by = .(series_id, day)]
mean_global <- test[, .(meanGlobal = mean(consumption)), by = .(series_id)]

sub_temp <- merge(sub_temp, mean_hour, by = c("series_id", "hour"), all.x = TRUE, sort = FALSE)
sub_temp <- merge(sub_temp, mean_day_hour, by = c("series_id", "day", "hour"), all.x = TRUE, sort = FALSE)
sub_temp <- merge(sub_temp, mean_day, by = c("series_id", "day"), all.x = TRUE, sort = FALSE)
sub_temp <- merge(sub_temp, mean_global, by = c("series_id"), all.x = TRUE, sort = FALSE)

col_weights <- matrix(rep(c(0.5, 0.2, 0.2, 0.1), times = nrow(sub_temp)),
                      ncol = 4, byrow = TRUE)

col_weights[is.na(sub_temp[, 10:13])] <- 0
col_weights <- t(apply(col_weights, 1, function(x) x / sum(x)))  # Normalise weights by row to sum to 1

weighted_prediction <- rowSums(sub_temp[, 10:13] * col_weights, na.rm = TRUE)

submission_format[, consumption := weighted_prediction]

fwrite(submission_format, "submissions/001.csv", row.names = FALSE)
