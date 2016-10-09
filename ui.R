library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Image color analysis"),
    
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                fileInput('image', 'Choose image to upload',
                          accept = c(
                          'image/jpeg',
                          '.jpg',
                          '.jpeg'
                      ))
            ),
            fluidRow(
                column(12, align = "center", imageOutput("inputImage")
                )
            ),
            fluidRow(
                column(12, align = "center", div(style = "margin-top: -100px;"),
                       imageOutput("sortedImage")
                )
            )
        ),
        
        mainPanel(
            fluidRow(
                column(10, align = "center",
                       plotOutput('barplot', width = "500px", height = "300px")
                )
            ),
            fluidRow(
                column(5, align = "center", div(style = "margin-top: 50px;"),
                       plotOutput("darkColors", width = "300px", height = "300px")
                ),
                column(5, align = "center", div(style = "margin-top: 50px;"),
                       plotOutput("lightColors", width = "300px", height = "300px")
                )
            )
        )
    )
))
