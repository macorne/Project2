library(shiny)
library(shinyalert)
library(tidyverse)
source("static.R")

#On the sidebar, you will have widgets that allow the user to subset the data
#∗ Choose (at least) two categorical variables they can subset from. If there are groups of
#categories that make sense to have them choose for a given variable, that’s fine. That is, they
#don’t need to be able to choose any level of the variable to subset. The user should be
#able to select all levels as well.
#∗ Give the user a way to select a numeric variable. When selected, use a dynamic UI method
#to create a slider (with two values) so they can subset on that variable if they choose.
#∗ Repeat the previous for a second numeric variable.
#∗ Use an actionButton() that, when pressed, subsets the data according to the selections made
#on the sidebar. (The data should not update unless this button is pressed.) You should be
#using either a reactive() or reactiveValues() object on the server side to subset
#your data appropriately and pass it to subsequent sections of the main panel (see below).

###==============================================###
# Define UI 
ui <- fluidPage(
  "Categorical Subsetting",
  sidebarLayout(
    sidebarPanel(
      h2("Select Categorical Variables:"),
      selectizeInput(
        "subs1",
        label = "Categorical Variable 1",
        choices = cat_vars,
        selected = "Device Model"
      ),
      selectizeInput(
        "subs2",
        label = "Categorical Variable 2",
        choices = cat_vars,
        selected = "Operating System"
      ),
      h2("Choose a subset of the data:"),
      radioButtons(
        "hhl_corr",
        "Household Language",
        choices = list(
          "All" = 1, 
          "English only" = 2, 
          "Spanish" = 3, 
          "Other" = 4
        ),
        selected = 1
      ), 
      radioButtons(
        "fs_corr",
        "SNAP Recipient",
        choices = list(
          "All" = 1, 
          "Yes" = 2, 
          "No" = 3
        ),
        selected = 1
      ),
      radioButtons(
        "schl_corr",
        "Educational attainment",
        choices = list(
          "All" = 1, 
          "High School not Completed" = 2, 
          "High School or GED" = 3, 
          "College Degree" = 4
        ),
        selected = 1
      ),
      h2("Select a Range"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
      sliderInput(
        "Range1",
        "Value:",
        value = c(0,100)
      ),
      h2("Select Another Range"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
      sliderInput(
        "Range2",
        "Value:",
        value = c(0,100)
      ),
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      plotOutput(
        outputId = "distPlot"
      ),
      conditionalPanel(
        "input.corr_sample",
        h2("Guess the correlation!"),
        column(6,
               numericInput(
                 "corr_guess",
                 "",
                 value = 0,
                 min = -1,
                 max = 1
               )
        ),
        column(6, 
               actionButton(
                 "corr_submit", 
                 "Check Your Guess!")
        )
      )
    )
  )
)

#my_sample <- readRDS("my_sample_temp.rds")

###==============================================###
# Define server logic
server <- function(input, output, session) {
  
  sample_corr <- reactiveValues()
  sample_corr$corr_data <- NULL
  sample_corr$corr_truth <- NULL
  
  #Update input boxes so user can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices)
    }
  }
  )
  
  #Use an observeEvent() to look for the action button (corr_sample)
  observeEvent(input$corr_sample, {
    if(input$hhl_corr == "All"){
      hhl_sub <- HHLvals
    } 
    else if(input$hhl_corr == "English only"){
      hhl_sub <- HHLvals["1"]
    } 
    else if(input$hhl_corr == "Spanish"){
      hhl_sub <- HHLvals["2"]
    } 
    else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    if(input$fs_corr == "All"){
      fs_sub <- FSvals
    } 
    else if(input$fs_corr == "Yes"){
      fs_sub <- FSvals["1"]
    } 
    else {
      fs_sub <- FSvals["2"]
    }
    
    if(input$schl_corr == "All"){
      schl_sub <- SCHLvals
    } 
    else if(input$schl_corr == "High School not Completed"){
      schl_sub <- SCHLvals[as.character(0:15)]
    } 
    else if(input$schl_corr == "High School or GED"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } 
    else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    corr_vars <- c(input$corr_x, input$corr_y)
    
    subsetted_data <- my_sample |>
      filter(#cat vars first
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>% 
      #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP >= 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP >= 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP >= 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP >= 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP >= 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP >= 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .} 
    
    index <- sample(
      1:nrow(subsetted_data), 
      size = input$Range1, 
      replace = TRUE, 
      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP)
    )
    
    index <- sample(
      1:nrow(subsetted_data), 
      size = input$Range2, 
      replace = TRUE, 
      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP)
    )
    
    #Warning:  if standard deviation is zero (no correlation), 
    #then guessing a value crashes the app
    sample_corr$corr_data <- subsetted_data[index,]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1,2]
  }
  )
  
  #Create a renderPlot() object to output a scatter plot
  output$distPlot <- renderPlot({
    #Use the code below to validate that data exists,
    #then create the appropriate scatter plot
    validate(
      need(
        !is.null(sample_corr$corr_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
      )
    ) #this is a useful function to add as a placeholder until data is generated!
    ggplot(
      sample_corr$corr_data, 
      aes_string(
        x = isolate(input$corr_x), 
        y = isolate(input$corr_y))) + 
      geom_point()
  })
  
  
  #Use this code for the correlation guessing game!
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", 
                        round(sample_corr$corr_truth, 4), 
                        "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
