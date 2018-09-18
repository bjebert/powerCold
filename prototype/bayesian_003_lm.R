
# bayesian optimisation ----------------------------------------------------------

library(rBayesianOptimization)
source("models/evaluator.R")

bayesian_eval <- function(w1, w2, w3, w4, m) {
    params <- c(w1, w2, w3, w4, m)
    nmae <- evaluate("007", params)
    
    list(Score = -nmae,
         Pred = 0)
}

BayesianOptimization(bayesian_eval,
                     bounds = list(w1 = c(0, 1),
                                   w2 = c(0, 1),
                                   w3 = c(0, 1),
                                   w4 = c(0, 1),
                                   m = c(0.8, 1.2)),
                     init_points = 10,
                     n_iter = 200)

