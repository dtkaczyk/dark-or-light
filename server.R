library(shiny)
library(ggplot2)
library(jpeg)
library(flexclust)
library(gridExtra)

shinyServer(function(input, output) {
    
    output$inputImage <- renderImage({
        image <- input$image
        if (is.null(image)) {
            return(list(src = ""))
        }
        return(list(
                src      = image$datapath,
                filetype = "image/jpeg",
                alt      = "Original image",
                width    = 250
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
        samplePixels <- pixels[sampleX, sampleY,]
        
        data <- data.frame(R = rep(0, sampleLen), 
                           G = rep(0, sampleLen), 
                           B = rep(0, sampleLen))
        i <- 1
        for (sx in sampleX) {
            data[((i-1)*sampleLenY+1):(i*sampleLenY),] <- pixels[sx,sampleY,]
            i <- i + 1
        }
        
        data$sum <- 0.299 * data$R + 0.587 * data$G + 0.114 * data$B
        data <- data[order(data$sum),]
        dark <- data[data$sum < 0.5,]
        light <- data[data$sum >= 0.5,]
        
        darkMean <- rgb(mean(dark$R), mean(dark$G), mean(dark$B))
        lightMean <- rgb(mean(light$R), mean(light$G), mean(light$B))
        
        twoColors <- data.frame(color      = c(darkMean, lightMean), 
                                percentage = c(nrow(dark)/nrow(data), 
                                               nrow(light)/nrow(data)))
        
        clusteringDark <- kcca(dark[,c(1,2,3)], 4, 
                               family = kccaFamily("kmedians"))
        darkColors <- apply(clusteringDark@centers, 1, 
                            function(x) rgb(x[1], x[2], x[3]))
        
        clusteringLight <- kcca(light[,c(1,2,3)], 4, 
                                family = kccaFamily("kmedians"))
        lightColors <- apply(clusteringLight@centers, 1, 
                             function(x) rgb(x[1], x[2], x[3]))
        
        sortedPixels <- array(0, dim = c(sampleLenX, sampleLenY, 3))
        sortedPixels[,,1] <- matrix(data$R, nrow = sampleLenX, ncol = sampleLenY)
        sortedPixels[,,2] <- matrix(data$G, nrow = sampleLenX, ncol = sampleLenY)
        sortedPixels[,,3] <- matrix(data$B, nrow = sampleLenX, ncol = sampleLenY)
        sortedFile <- tempfile(pattern = "file", tmpdir = tempdir(), 
                               fileext = "jpg")
        writeJPEG(sortedPixels, target = sortedFile)
         
        list(twoColors   = twoColors,
             darkColors  = darkColors,
             lightColors = lightColors,
             sortedFile  = sortedFile
        )
    })
    
    output$barplot <- renderPlot({
        twoColors <- colorSummary()$twoColors
        colorScale <- sort(as.character(twoColors$color))
        
        ggplot(twoColors, aes(x = twoColors$color, y = twoColors$percentage,
                              fill = twoColors$color)) +
            geom_bar(stat = "identity") + 
            scale_fill_manual(values = colorScale) +
            xlab("Colors") + ylab("Fraction of the image") +
            theme(text = element_text(size = 20)) +
            guides(fill = FALSE)
    })
    
    output$darkColors <- renderPlot({
        cols <- colorSummary()$darkColors
        if (!is.null(cols)) {
            gs <- lapply(cols, 
                         function(x) grobTree(rectGrob(
                             gp = gpar(fill = x, col = "white")), textGrob("")))
            grid.arrange(grobs = gs, ncol = 2, 
                         top = textGrob("Dark colors", gp = gpar(fontsize = 20)))
            grid.rect(gp = gpar(fill = NA))
        }
    })
    
    output$lightColors <- renderPlot({
        cols <- colorSummary()$lightColors
        if (!is.null(cols)) {
            gs <- lapply(cols, 
                         function(x) grobTree(rectGrob(
                             gp = gpar(fill = x, col = "white")), textGrob("")))
            grid.arrange(grobs = gs, ncol=2, 
                         top = textGrob("Light colors", gp = gpar(fontsize = 20)))
            grid.rect(gp = gpar(fill = NA))
        }
    })
    
    output$sortedImage <- renderImage({
        path <- colorSummary()$sortedFile
        if (is.null(path)) {
            return(list(src = ""))
        }
        return(list(
            src      = path,
            filetype = "image/jpeg",
            alt      = "Sorted image",
            width    = 250
        ))
    }, deleteFile = FALSE)
})
