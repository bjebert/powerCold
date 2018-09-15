library(data.table)
library(ggplot2)

train <- fread("data/train.csv")
test <- fread("data/test.csv")
submission_format <- fread("data/submission_format.csv")

temp_data <- train[!is.na(temperature)]
temp_summary <- temp_data[, .(meanConsumption = mean(consumption)), by = .(temp = round(temperature))]

ggplot(temp_summary, aes(x = temp, y = meanConsumption)) +
    geom_point() +
    geom_smooth()
