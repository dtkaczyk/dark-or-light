library(shiny)
library(ggplot2)
library(jpeg)
library(flexclust)

shinyServer(function(input, output) {
    
    output$inputImage <- renderImage({
        image <- input$image
        if (is.null(image)) {
            return(list(src=""))
        }
        return(list(
                src = image$datapath,
                filetype = "image/jpeg",
                alt = "Original image",
                width=250
            ))
    }, deleteFile = FALSE)
    
    colorSummary <- reactive({
        image <- input$image
        if (is.null(image))
            return(NULL)
        
        pixels <- readJPEG(image$datapath)
        lenX <- dim(pixels)[1]
        lenY <- dim(pixels)[2]
        sampleFrac <- .05
        sampleLenX <- round(sampleFrac * lenX)
        sampleLenY <- round(sampleFrac * lenY)
        sampleLen <- sampleLenX * sampleLenY
        sampleX <- sample(1:lenX, sampleLenX)
        sampleY <- sample(1:lenY, sampleLenY)
        samplePixels <- pixels[sampleX,sampleY,]
        
        data <- data.frame(R = rep(0, sampleLen), 
                           G = rep(0, sampleLen), 
                           B = rep(0, sampleLen))
        i <- 1
        for (sx in sampleX) {
            data[((i - 1) * sampleLenY + 1):(i * sampleLenY),] <- pixels[sx,sampleY,]
            i <- i + 1
        }
        
        data$sum <- 0.299 * data$R + 0.587 * data$G + 0.114 * data$B
        data <- data[order(data$sum),]
        dark <- data[data$sum < 0.5,]
        light <- data[data$sum >= 0.5,]
        
        darkMean <- rgb(mean(dark$R), mean(dark$G), mean(dark$B))
        lightMean <- rgb(mean(light$R), mean(light$G), mean(light$B))
        
        twoColors <- data.frame(color = c(darkMean, lightMean), 
                             percentage = c(nrow(dark)/nrow(data), nrow(light)/nrow(data)))
        
        list(data = data, dark = dark, light = light, twoColors = twoColors)
    })
    
    output$barplot <- renderPlot({
        twoColors <- colorSummary()$twoColors
        colorScale <- sort(as.character(twoColors$color))
        
        ggplot(twoColors,
               aes(x = twoColors$color, y = twoColors$percentage, fill = twoColors$color)) +
            geom_bar(stat = "identity") + 
            scale_fill_manual(values = colorScale) +
            xlab("Colors") + ylab("Fraction of the image") +
            theme(text=element_text(size=20)) +
            guides(fill=FALSE)
    })
})
