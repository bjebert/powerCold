
# ARIMA -------------------------------------------------------------------

library(data.table)
library(forecast)

train <- fread("data/train.csv")

train_1 <- train[split == "train" & sub_split == "train"]
test_1 <- train[split == "train" & sub_split == "test"]
train_2 <- train[split == "val" & sub_split == "train"]
test_2 <- train[split == "val" & sub_split == "test"]


# forecast ----------------------------------------------------------------

test_series <- train_2[, unique(series_id)]

for(series in test_series) {
    y <- ts(train_2[series_id == series, consumption], freq = 24)  
    mdl <- auto.arima(y)
    
}
