# Libraries ---------------------------------------------------------------

library(data.table)
set.seed(0)

# Evaluator ---------------------------------------------------------------

evaluate <- function(model_name = "004", params = NULL) {
    train <- fread("data/train.csv")
    
    train_1 <- train[split == "train" & sub_split == "train"]
    test_1 <- train[split == "train" & sub_split == "test"]
    train_2 <- train[split == "val" & sub_split == "train"]
    test_2 <- train[split == "val" & sub_split == "test"]
    
    model_env <- new.env()
    sys.source(sprintf("models/%s/%s.R", model_name, model_name), envir = model_env)
    
    predictions <- model_env$get_predictions(train_1, test_1, train_2, test_2, params)
    test_2[, predictions := predictions]
    test_2[, m := mean(consumption), by = series_id]

    nmae <- mean(test_2[, abs(predictions - consumption) / m])
    
    return(nmae)
}


# Calls -------------------------------------------------------------------


# evaluate("003")  # 0.33329
# evaluate("004")  # 0.30291
evaluate("005")  # 0.267311
