library(caret)

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

# Linear models

modelLogit <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "glm", family = "binomial")
    predict(model, testing)
}

modelLDA <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "lda")
    predict(model, testing)
}

modelQDA <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "qda")
    predict(model, testing)
}

modelNB <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "nb")
    predict(model, testing)
}

# Tree-based

modelTree <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "rpart")
    predict(model, testing)
}

modelRF <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "rf")
    predict(model, testing)
}

# SVM

modelSVMLinear <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "svmLinear2")
    predict(model, testing)
}

modelSVMRadial <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "svmRadial")
    predict(model, testing)
}

modelSVMPoly <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "svmPoly")
    predict(model, testing)
}

# Other

modelKNN <- function(training, testing) {
    model <- train(Lum ~ Red + Green + Blue, data = training, method = "knn")
    predict(model, testing)
}
