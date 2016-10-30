extractFeatures <- function(data) {
    data$SqRed <- data$Red^2
    data$SqGreen <- data$Green^2
    data$SqBlue <- data$Blue^2
    data$SqRootRed <- sqrt(data$Red)
    data$SqRootGreen <- sqrt(data$Green)
    data$SqRootBlue <- sqrt(data$Blue)
    data$MultRedGreen <- data$Red * data$Green
    data$MultGreenBlue <- data$Green * data$Blue
    data$MultRedBlue <- data$Red * data$Blue
    data$MultRedGreenBlue <- data$Red * data$Green * data$Blue
    data$RatioRedGreen <- data$Red / data$Green
    data$RatioGreenRed <- data$Green / data$Red
    data$RatioRedBlue <- data$Red / data$Blue
    data$RatioBlueRed <- data$Blue / data$Red
    data$RatioBlueGreen <- data$Blue / data$Green
    data$RatioGreenBlue <- data$Green / data$Blue
    data
}

evaluateFeatures <- function(data) {
    dataLight <- data[data$Lum == "L",]
    dataDark <- data[data$Lum == "D",]
    fNames <- colnames(data)
    fNames <- fNames[fNames != "Lum"]
    pvalues <- sapply(fNames, function(x) {ks.test(dataLight[[x]], dataDark[[x]])$p.value})
    pvalues <- data.frame(
        Feature = names(pvalues),
        PValue = pvalues
    )
    pvalues <- pvalues[order(pvalues$PValue),]
    row.names(pvalues) <- NULL
    pvalues
}
