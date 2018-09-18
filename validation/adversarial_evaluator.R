rm(list = ls())
library(data.table)

# Evaluation --------------------------------------------------------------
adv_model <- "ADS_001"

adversarial_evaluator <- function(adv_model = "ADS_001") {
    
    adv_train <- fread("data/adv_train.csv")
    adv_test <- fread("data/adv_test.csv")
    
    set.seed(0)
    
    adv_env <- new.env()
    sys.source(sprintf("validation/%s/%s.R", adv_model, adv_model), envir = adv_env)
    
    actuals <- adv_test[, mean(is_test), by = series_id][["V1"]]
    predictions <- adv_env$get_predictions(adv_train, adv_test)
    
    c(Metrics::auc(actuals, predictions), Metrics::logLoss(actuals, predictions))
}


# Results -----------------------------------------------------------------

adversarial_evaluator("ADS_001")  # AUC: 0.9429, NLL: 0.3204


