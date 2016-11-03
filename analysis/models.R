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

trainAndPredict <- function(training, testing, method, scale = TRUE, 
                            features = colnames(training), selectFeatures = TRUE, ...) {
    if (scale) {
        preProcValues <- preProcess(training, method = c("range"))
        training <- predict(preProcValues, training)
        testing <- predict(preProcValues, testing)
    }
    features <- features[features!="Lum"]
    bestAccuracy <- 0
    if (selectFeatures) {
        while (length(features) != 0) {
            set.seed(2690)
            model <- train(reformulate(termlabels = features, response = "Lum"), data = training, method = method, ...)
            if (max(model$results$Accuracy) > bestAccuracy) {
                bestAccuracy <- max(model$results$Accuracy)
                bestModel <- model
            }
            if (length(features) == 1) {
                features <- c()
            } else {
                imp <- varImp(model)$importance
                if (!("Overall" %in% colnames(imp))) {
                    imp <- imp[features,]
                    imp$Overall <- apply(imp, 1, function(x) {max(x[1], x[2])})
                }
                imp <- imp[order(imp$Overall), , drop = FALSE]
                toDelete <- row.names(imp)[1]
                features <- features[features != toDelete]
            }
        }
    } else {
        set.seed(2690)
        bestModel <- train(Lum ~ ., data = training, method = method, ...)
    }
    predict(bestModel, testing)
}

modelLogit <- function(training, testing, ...) {
    trainAndPredict(training, testing, "glm", family = "binomial", ...)
}

modelLDA <- function(training, testing, ...) {
    trainAndPredict(training, testing, "lda", ...)
}

modelQDA <- function(training, testing, ...) {
    trainAndPredict(training, testing, "qda", ...)
}

modelNB <- function(training, testing, ...) {
    trainAndPredict(training, testing, "nb", ...)
}

# Tree-based

modelTree <- function(training, testing, ...) {
    trainAndPredict(training, testing, "rpart", ...)
}

modelRF <- function(training, testing, ...) {
    trainAndPredict(training, testing, "rf", ...)
}

# SVM

modelSVMLinear <- function(training, testing, ...) {
    trainAndPredict(training, testing, "svmLinear2", ...)
}

modelSVMRadial <- function(training, testing, ...) {
    trainAndPredict(training, testing, "svmRadial", ...)
}

modelSVMPoly <- function(training, testing, ...) {
    trainAndPredict(training, testing, "svmPoly", ...)
}

# Other

modelKNN <- function(training, testing, ...) {
    trainAndPredict(training, testing, "knn", ...)
}
