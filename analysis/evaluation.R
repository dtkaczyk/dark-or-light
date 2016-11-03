library(ggplot2)

evaluate <- function(data, modelFunction, ...) {
    set.seed(2690)
    dataRand <- data[sample(nrow(data)),]
    row.names(dataRand) <- NULL
    accuracy <- rep(0, 10)
    for (fold in 0:9) {
        train <- dataRand[as.numeric(rownames(dataRand)) %% 10 != fold,]
        test <- dataRand[as.numeric(rownames(dataRand)) %% 10 == fold,]
        prediction <- factor(modelFunction(train, test, ...), levels(test$Lum))
        cm <- confusionMatrix(test$Lum, prediction)
        accuracy[fold+1] <- cm$overall[["Accuracy"]]
    }
    accuracy
}

cvSummary <- function(results) {
    list(mean = mean(results), sd = sd(results))
}

summarizeResults <- function(resultList) {
    summaryDF <- data.frame(
        Method = names(resultList),
        Mean   = sapply(resultList, mean),
        SD     = sapply(resultList, sd),
        Min    = sapply(resultList, min),
        Max    = sapply(resultList, max),
        StatWins = 0
    )
    for (i in names(resultList)) {
        x <- resultList[[i]]
        wins <- 0
        for (j in names(resultList)) {
            y <- resultList[[j]]
            if (i != j && mean(x) > mean(y)) {
                pv <- wilcox.test(x, y, paired = TRUE)$p.value
                if (pv < 0.05) {
                    wins <- wins + 1
                }
            }
        }
        summaryDF[summaryDF$Method == i,]$StatWins <- wins
    }
    summaryDF <- summaryDF[order(-summaryDF$StatWins, -summaryDF$Mean),]
    row.names(summaryDF) <- NULL
    summaryDF
}

visualizeResults <- function(resultList) {
    resultNames <- sapply(names(resultList), function(x) {gsub(" ", "\n", x)})
    modelsResults <- data.frame(Accuracy = unlist(resultList), Model = unlist(lapply(resultNames, rep, 10)))
    ggplot(modelsResults, aes(x = Model, y = Accuracy, fill = Model)) + geom_boxplot() + guides(fill = FALSE)
}
