# Baseline models

modelAlwaysLight <- function(training, testing) {
    rep("L", nrow(testing))
}

modelHalfSum <- function(training, testing) {
    d <- which(testing$Red + testing$Green + testing$Blue < 1.5)
    prediction <- rep("L", nrow(testing))
    prediction[d] <- "D"
    prediction
}

modelStandardLuma <- function(training, testing) {
    l <- which(0.299 * testing$Red + 0.587 * testing$Green + 0.114 * testing$Blue < 0.5)
    prediction <- rep("L", nrow(testing))
    prediction[l] <- "D"
    prediction
}