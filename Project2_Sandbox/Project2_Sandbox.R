#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(ggplot2)
library(shiny)
library(tidyverse)

#Data set
usrbhvr_data <- read_csv("user_behavior_dataset.csv")

#Get names
names(usrbhvr_data)

#Fix variable names since these have spaces in some cases
names(usrbhvr_data) <- gsub("\\([^\\)]+\\)", "",
                            str_replace_all(names(usrbhvr_data),
                                            c(" " = "")))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cell Phone User Behavior"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("depth", "Depth:", min = 0, max = 500, value = c(0,100))
      ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
