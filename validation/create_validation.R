# Create validation sets given adversarial validator

library(data.table)
library(xgboost)

adv_train <- fread("data/adv_train.csv")
adv_test <- fread("data/adv_test.csv")
adv_data <- rbind(adv_train, adv_test)

# Create ------------------------------------------------------------------

#' @adv_model Model to predict proximity of data distribution to actual test set
#' @param N1 proportion of data that will be the validation set
#' @param N2 proportion of data to hold out for our train/val sets (replicating test vs. submission format)
#' 
create_validation <- function(adv_model = "ADV004", N1 = 0.4, N2 = 0.5) {
    train <- fread("data/train.csv")
    
    adv_env <- new.env()
    sys.source(sprintf("validation/%s/%s.R", adv_model, adv_model), envir = adv_env)
    
    # Predicting "is_test"
    predictions <- adv_env$get_predictions(adv_data, train)
    
    train[, pred := predictions]
    train[, pred := mean(pred), by = series_id]
    unique_train <- unique(train[, .(series_id, pred)])
    
    val_N <- round(N1 * nrow(unique_train))
    val_series <- unique_train[order(-pred)][1:val_N][["series_id"]]
    
    train[, pred := NULL]
    train[, split := "train"]
    train[series_id %in% val_series, split := "val"]   
    
    # Now, "chop off" some of the ending values from each split (by marking it as sub test_set)
    train[, sub_split := "train"]
    train[, N := 1:.N, by = series_id]
    train[N > round(N2 * 672), sub_split := "test"]
    train[, N := NULL]
    
    fwrite(train, "data/train.csv", row.names = FALSE)
}
