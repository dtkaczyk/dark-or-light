evaluate <- function(data, modelFunction) {
    set.seed(2434)
    dataRand <- data[sample(nrow(data)),]
    row.names(dataRand) <- NULL
    accuracy <- rep(0, 10)
    for (fold in 0:9) {
        train <- dataRand[as.numeric(rownames(dataRand)) %% 10 != fold,]
        test <- dataRand[as.numeric(rownames(dataRand)) %% 10 == fold,]
        prediction <- factor(modelFunction(train, test), levels(test$Lum))
        cm <- confusionMatrix(test$Lum, prediction)
        accuracy[fold+1] <- cm$overall[["Accuracy"]]
    }
    accuracy
}

cvSummary <- function(results) {
    list(mean = mean(results), sd = sd(results))
}

visualizeResults <- function(resultList) {
    modelsResults <- data.frame(Accuracy = unlist(resultList), Model = unlist(lapply(names(resultList), rep, 10)))
    ggplot(modelsResults, aes(x = Model, y = Accuracy, fill = Model)) + geom_boxplot() + guides(fill = FALSE)
}
