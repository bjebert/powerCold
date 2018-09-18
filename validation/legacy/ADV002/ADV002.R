library(data.table)
library(xgboost)
library(rBayesianOptimization)
set.seed(0)

source("~/utilities/col_convert.R")


# Functions ---------------------------------------------------------------



preprocess_data <- function(adv_data) {
    adv_data <- col_convert(adv_data, "integer", "numeric")    
    return(adv_data)
}






get_predictions <- function(adv_train, adv_test) {
    adv_train <- preprocess_data(adv_train)
    adv_test <- preprocess_data(adv_test)
    
    x <- c("series_id", "consumption", "temperature")
    
    Dtrain <- xgb.DMatrix(data = as.matrix(adv_train[, x, with = FALSE]), label = adv_train[["is_test"]])
    Dtest <- xgb.DMatrix(data = as.matrix(adv_test[, x, with = FALSE]))        
    
    params <- list(objective = "binary:logistic",
                   nrounds = 43,
                   eta = 0.4517,
                   max_depth = 2)    
    
    mdl <- xgboost(data = Dtrain,
                   params = params,
                   nrounds = params[["nrounds"]])    
    
    predict(mdl, Dtest)
}


# Bayesian optimisation ---------------------------------------------------
# 
# adv_train <- preprocess_data(adv_train)
# 
# x <- c("series_id", "consumption", "temperature")
# 
# uq <- unique(adv_train[, series_N])
# val_series <- sample(uq, round(0.3 * length(uq)))
# 
# train2 <- adv_train[!series_N %in% val_series]
# val2 <- adv_train[series_N %in% val_series]
# 
# Dtrain <- xgb.DMatrix(data = as.matrix(train2[, x, with = FALSE]), label = train2[["is_test"]])
# Dval <- xgb.DMatrix(data = as.matrix(val2[, x, with = FALSE]))    
# 
# bayesian_eval <- function(nrounds, eta, max_depth) {
#     params <- list(objective = "binary:logistic",
#                    nrounds = nrounds,
#                    eta = eta,
#                    max_depth = max_depth)
#     
#     mdl <- xgboost(data = Dtrain,
#                    params = params,
#                    nrounds = params[["nrounds"]],
#                    verbose = 0)
#     
#     preds <- predict(mdl, Dval)
#     
#     auc <- Metrics::auc(val2[["is_test"]], preds)
#     
#     list(Score = auc,
#          Pred = preds)
# }
# 
# BayesianOptimization(bayesian_eval,
#                      bounds = list(nrounds = c(5L, 500L),
#                                    eta = c(0.05, 0.5),
#                                    max_depth = c(1L, 20L)),
#                      init_points = 20,
#                      n_iter = 100)
# 
# 
# 
