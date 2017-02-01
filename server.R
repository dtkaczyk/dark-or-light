library(shiny)
library(ggplot2)
library(jpeg)
library(flexclust)
library(gridExtra)
library(yaml)

shinyServer(function(input, output) {
    
    conf = yaml.load_file("config.yml")
    
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
        sampleFrac <- sqrt(conf$sampleSize / lenX / lenY)
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
        
        data$sum <- conf$predict$R * data$R + conf$predict$G * data$G + conf$predict$B * data$B
        data <- data[order(data$sum),]
        dark <- data[data$sum < 0.5,]
        light <- data[data$sum >= 0.5,]
        
        darkMean <- rgb(mean(dark$R), mean(dark$G), mean(dark$B))
        lightMean <- rgb(mean(light$R), mean(light$G), mean(light$B))
        
        twoColors <- data.frame(color      = c(darkMean, lightMean), 
                                percentage = c(nrow(dark)/nrow(data), 
                                               nrow(light)/nrow(data)))
        
        clusteringDark <- kcca(dark[,c(1,2,3)], 4, 
                               family = kccaFamily(conf$clustering$type))
        darkColors <- apply(clusteringDark@centers, 1, 
                            function(x) rgb(x[1], x[2], x[3]))
        darkFractions <- round(clusteringDark@clusinfo$size / sampleLen, digits = conf$clustering$num)
        
        clusteringLight <- kcca(light[,c(1,2,3)], 4, 
                                family = kccaFamily(conf$clustering$type))
        lightColors <- apply(clusteringLight@centers, 1, 
                             function(x) rgb(x[1], x[2], x[3]))
        lightFractions <- round(clusteringLight@clusinfo$size / sampleLen, digits = conf$clustering$num)
        
        sortedPixels <- array(1, dim = c(sampleLenX + 2 * conf$tickLen, sampleLenY, 3))
        for (i in round(0:10 * (sampleLenY-1)/10 + 1)) {
            i <- max(1, i)
            i <- min(sampleLenY, i)
            sortedPixels[1:conf$tickLen,i,2:3] <- 0
            sortedPixels[(sampleLenX+conf$tickLen+1):(sampleLenX+2*conf$tickLen),i,2:3] <- 0
        }
        sortedPixels[(conf$tickLen+1):(conf$tickLen+sampleLenX),,1] <- 
            matrix(data$R, nrow = sampleLenX, ncol = sampleLenY)
        sortedPixels[(conf$tickLen+1):(conf$tickLen+sampleLenX),,2] <- 
            matrix(data$G, nrow = sampleLenX, ncol = sampleLenY)
        sortedPixels[(conf$tickLen+1):(conf$tickLen+sampleLenX),,3] <- 
            matrix(data$B, nrow = sampleLenX, ncol = sampleLenY)
        sortedFile <- tempfile(pattern = "file", tmpdir = tempdir(), 
                               fileext = "jpg")
        writeJPEG(sortedPixels, target = sortedFile, quality = 1)
         
        list(twoColors      = twoColors,
             darkColors     = darkColors,
             lightColors    = lightColors,
             darkFractions  = darkFractions,
             lightFractions = lightFractions,
             sortedFile     = sortedFile
        )
    })
    
    output$barplot <- renderPlot({
        twoColors <- colorSummary()$twoColors
        if (!is.null(twoColors)) {
            colorScale <- sort(as.character(twoColors$color))
            ggplot(twoColors, aes(x = twoColors$color, y = twoColors$percentage,
                              fill = twoColors$color)) +
                geom_bar(stat = "identity") + 
                scale_fill_manual(values = colorScale) +
                scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
                xlab("Colors") + ylab("Fraction of the image") +
                theme(text = element_text(size = 20)) +
                guides(fill = FALSE)
        }
    })
    
    output$darkColors <- renderPlot({
        cols <- colorSummary()$darkColors
        fracs <- colorSummary()$darkFractions
        data <- data.frame(cols = cols, fracs = fracs)
        if (!is.null(cols)) {
            gs <- apply(data, 1, function(x) grobTree(rectGrob(
                                gp = gpar(fill = x[1], col = "white")), 
                                textGrob(x[2], gp = gpar(fontsize = 20, col = "white"))))
            grid.arrange(grobs = gs, ncol = 2, 
                         top = textGrob("Dark colors", gp = gpar(fontsize = 20)))
            grid.rect(gp = gpar(fill = NA))
        }
    })
    
    output$lightColors <- renderPlot({
        cols <- colorSummary()$lightColors
        fracs <- colorSummary()$lightFractions
        data <- data.frame(cols = cols, fracs = fracs)
        if (!is.null(cols)) {
            gs <- apply(data, 1, function(x) grobTree(rectGrob(
                                gp = gpar(fill = x[1], col = "white")), 
                                textGrob(x[2], gp = gpar(fontsize = 20, col = "black"))))
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
