
# bayesian optimisation ----------------------------------------------------------

library(rBayesianOptimization)
source("models/evaluator.R")

bayesian_eval <- function(w1, w2, w3, w4) {
    params <- c(w1, w2, w3, w4)
    nmae <- evaluate("005", params)
    
    list(Score = -nmae,
         Pred = 0)
}

BayesianOptimization(bayesian_eval,
                     bounds = list(w1 = c(0, 1),
                                   w2 = c(0, 1),
                                   w3 = c(0, 1),
                                   w4 = c(0, 1)),
                     init_points = 50,
                     n_iter = 200)

