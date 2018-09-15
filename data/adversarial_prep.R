# Adversarial validation data preparer
#
# The goal is to predict which series are more similar to the distribution of series found in the test set
# Then, we can use these to form our validation set in training
# 
# Or, alternatively, we can discard series from the training set which are not at all similar to the 
# series found in the test set

rm(list = ls())

train <- fread("data/train.csv")
test <- fread("data/test.csv")

train[, is_test := 0]
test[, is_test := 1]

power_data <- rbind(train, test)
power_data[, V1 := NULL]

series <- power_data[, unique(series_id)]
power_data <- merge(power_data,
                    data.table(series_id = series, series_N = 1:length(series)),
                    by = 'series_id')

# Create a 70%-30% train/test split
N <- 0.3

test_series <- sample(1:length(series), round(N * length(series)))

adv_test <- power_data[series_N %in% test_series]
adv_train <- power_data[!series_N %in% test_series]

fwrite(adv_train, "data/adv_train.csv", row.names = FALSE)
fwrite(adv_test, "data/adv_test.csv", row.names = FALSE)
