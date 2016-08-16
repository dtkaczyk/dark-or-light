library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Image color analysis"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput('image', 'Choose image to upload',
                      accept = c(
                          'image/jpeg',
                          '.jpg',
                          '.jpeg'
                      )),
            imageOutput("inputImage")
        ),
        
        mainPanel(
            plotOutput("barplot")
        )
    )
))
