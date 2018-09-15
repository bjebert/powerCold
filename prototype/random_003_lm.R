
# random optimisation ----------------------------------------------------------

source("models/evaluator.R")

eval_predictions <- function(w1, w2, w3, w4) {
    params <- c(w1, w2, w3, w4)
    nmae <- evaluate("005", params)
    
    return(nmae)
}


best_score <- Inf
i <- 0
while(TRUE) {
    i <- i + 1
    w1 <- runif(1, 0, 1)
    w2 <- runif(1, 0, 1)
    w3 <- runif(1, 0, 1)
    w4 <- runif(1, 0, 1)
    
    nmae <- eval_predictions(w1, w2, w3, w4)
    if(nmae < best_score) {
        best_score <- nmae
        print(sprintf("new best score: %f | %f / %f / %f / %f | (Iteration %d)", best_score, w1, w2, w3, w4, i))
    }
}

