# Libraries ---------------------------------------------------------------

library(data.table)

# Evaluator ---------------------------------------------------------------

evaluate <- function(model_name = "XGB_001", params = NULL) {
    set.seed(0)
    train <- fread("data/train.csv")
    
    train_1 <- train[split == "train" & sub_split == "train"]
    test_1 <- train[split == "train" & sub_split == "test"]
    train_2 <- train[split == "val" & sub_split == "train"]
    test_2 <- train[split == "val" & sub_split == "test"]
    
    model_env <- new.env()
    sys.source(sprintf("models/%s/%s.R", model_name, model_name), envir = model_env)
    
    predictions <- model_env$get_predictions(copy(train_1), copy(test_1), copy(train_2), copy(test_2), params)
    test_2[, predictions := predictions]
    test_2[, m := mean(consumption), by = series_id]

    nmae <- mean(test_2[, abs(predictions - consumption) / m])
    
    return(nmae)
}


# Calls -------------------------------------------------------------------

evaluate("003")  # CV 0.27315 / LB 0.4169
evaluate("004")  # CV 0.27926 / LB 0.6880
evaluate("005")  # CV 0.19427 / LB 0.3838
evaluate("006")  # CV 0.26574 / LB ?
evaluate("007")  # CV 0.19537 / LB 0.3906
evaluate("LM_001")  # CV 0.19849 / LB 0.3933
evaluate("LM_002")  # CV 0.21072 / LB ?
evaluate("XGB_001")  # CV 0.40697 / LB ?
evaluate("LM_003")  # CV 0.20663
evaluate("LM_004")  # CV 0.20331 / LB 0.3903


