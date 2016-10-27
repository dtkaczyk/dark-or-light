library(ggplot2)

createColorSet <- function(size = 300) {
    data.frame(Red = runif(size), Green = runif(size), Blue = runif(size), Lum = "")
}

visualizeColorSet <- function(data, size = min(30, nrow(data)), offset = 1, random = F) {
    if (random) {
        sData <- data[sample(rownames(data), size),]
    } else {
        sData <- data[offset:(offset+size-1),]
    }
    qplot(sData$Red, sData$Green) +
        xlim(0, 1.05) + ylim(0, 1.05) +
        xlab("Red") + ylab("Green") +
        geom_point(colour = rgb(sData$Red, sData$Green, sData$Blue), size = 7) + 
        geom_text(label = row.names(sData), hjust = -0.7, vjust = -0.7)
}