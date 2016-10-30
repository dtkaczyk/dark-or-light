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

trainAndPredict <- function(training, testing, method, scale = FALSE, ...) {
    if (scale) {
        preProcValues <- preProcess(training, method = c("center", "scale"))
        training <- predict(preProcValues, training)
        testing <- predict(preProcValues, testing)
    }
    model <- train(Lum ~ ., data = training, method = method, ...)
    predict(model, testing)
}

modelLogit <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "glm", scale, family = "binomial")
}

modelLDA <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "lda", scale)
}

modelQDA <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "qda", scale)
}

modelNB <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "nb", scale)
}

# Tree-based

modelTree <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "rpart", scale)
}

modelRF <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "rf", scale)
}

# SVM

modelSVMLinear <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "svmLinear2", scale)
}

modelSVMRadial <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "svmRadial", scale)
}

modelSVMPoly <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "svmPoly", scale)
}

# Other

modelKNN <- function(training, testing, scale = FALSE) {
    trainAndPredict(training, testing, "knn", scale)
}
