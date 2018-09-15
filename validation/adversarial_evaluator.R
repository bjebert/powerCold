library(data.table)

adv_train <- fread("data/adv_train.csv")
adv_test <- fread("data/adv_test.csv")


# Evaluation --------------------------------------------------------------


adversarial_evaluator <- function(adv_model = "ADV001") {
    adv_env <- new.env()
    sys.source(sprintf("validation/%s/%s.R", adv_model, adv_model), envir = adv_env)
    
    actuals <- adv_test[["is_test"]]
    predictions <- adv_env$get_predictions(adv_train, adv_test)
    
    c(Metrics::auc(actuals, predictions), Metrics::logLoss(actuals, predictions))
}


# Results -----------------------------------------------------------------


adversarial_evaluator("ADV001")  # 0.556264, 0.717264
adversarial_evaluator("ADV002")  # 0.612448, 0.470459
adversarial_evaluator("ADV003")  # 0.815784, 0.365180
adversarial_evaluator("ADV004")  # 0.874421, 0.344380


