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
        
        clustering <- kcca(data, 2, family = kccaFamily("kmedians"))
        dl <- c(0, 0)
        for (x in 1:lenX) {
            thisdl <- table(predict(clustering, pixels[x,,]))
            count1 <- 0
            if ("1" %in% names(thisdl)) {
                count1 <- thisdl[["1"]]
            }
            count2 <- 0
            if ("2" %in% names(thisdl)) {
                count2 <- thisdl[["2"]]
            }
            dl <- dl + c(count1, count2)
        }
        dl <- dl / sum(dl)
        names(dl) <- rgb(clustering@centers)
        
        pixels <- round(pixels)
        cols <- rep(0, 8)
        names(cols) <- c("#000000", "#0000FF", "#00FF00", "#00FFFF",
                     "#FF0000", "#FF00FF", "#FFFF00", "#FFFFFF")
        for (x in 1:lenX) {
            thisdl <- table(rgb(pixels[x,,]))
            for (n in names(thisdl)) {
                cols[[n]] <- cols[[n]] + thisdl[[n]]
            }
        }
        cols <- cols / sum(cols)
        
        list(dl = dl, cols = cols)
    })
    
    output$barplot <- renderPlot({
        colors <- colorSummary()$dl
        colors <- data.frame(color=names(colors), percentage=colors)
        colorScale <- sort(as.character(colors$color))
        ggplot(colors, 
               aes(x = colors$color, y = colors$percentage, fill = colors$color)) +
            geom_bar(stat = "identity") + 
            scale_fill_manual(values = colorScale) +
            xlab("Colors") + ylab("Fraction of the image") +
            ggtitle("Division into main colors") +
            theme(text=element_text(size=20)) +
            guides(fill=FALSE)
    })
})
