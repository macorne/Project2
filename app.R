library(bslib)
library(ggplot2)
library(shiny)
library(shinyalert)
library(tidyverse)
source("helpers.R")

#On the sidebar, you will have widgets that allow the user 
#to subset the data
#∗ Choose (at least) two categorical variables they can subset 
#from. If there are groups of categories that make sense to have
#them choose for a given variable, that’s fine. That is, they
#don’t need to be able to choose any level of the variable 
#to subset. The user should be able to select all levels as well.
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
      h2("Select Numeric Variables:"),
      selectizeInput(
        "y_var",
        label = "y-axis",
        choices = num_vars,
        selected = "NumberofAppsInstalled"
        ),
      h2("Select a Range"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
      conditionalPanel(
        condition = "input.y_var == 'AppUsageTime'",
      sliderInput(
        "Range1",
        "Value:",
        min=min('AppUsageTime'),
        max=,
        value = c(0,100)
      )
      ),
      selectizeInput(
        "x_var",
        label = "x-axis",
        choices = num_vars,
        selected = "DataUsage"
      ),
      h2("Select Another Range"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
      conditionalPanel(
        condition = "input.x_var == '???'",
        sliderInput(
        "Range2",
        "Value:",
        min=0,
        max=100,
        value = c(0,100)
      )
      ),
      h2("Select Device Model:"),
      radioButtons(
        "DevMod_var",
        "Device Model",
        choices = list(
          "All" = 1,
          "Google Pixel 5" = 2,
          "iPhone 12" = 3,
          "OnePlus 9" = 4,
          "Samsung Galaxy S21" = 5,
          "Xiaomi Mi 11" = 6
        ),
        selected = 1
      ),
      h2("Select Operating System:"),
      radioButtons(
        "OpSys_var",
        "Operating System",
        choices = list(
          "All" = 1,
          "Android" = 2,
          "iOS" = 3
        ),
        selected = 1
      ),
      h2("Select User Behavior Class"),
      radioButtons(
        "UBC_var",
        "User Behavior Class",
        choices = list(
          "All" = 1,
          "Between 0 and 300 MB/day" = 2, 
          "Between 300 and 600 MB/day" = 3,
          "Between 600 and 1,000 MB/day" = 4,
          "Between 1,000 and 1,500 MB/day" = 5,
          "More than 1,500 MB/day" = 6
        ),
        selected = 1
      ),
      radioButtons(
        "Gender_var",
        "Gender",
        choices = list(
          "All" = 1,
          "Female" = 2,
          "Male" = 3
        ),
        selected = 1
      )
      ,
      actionButton("subs_sample","Subset the data")
    ),
    mainPanel(
      plotOutput(
        outputId = "distPlot"
      )
    )
  )
)

usrbhvr_data <- read_csv("user_behavior_dataset.csv")

###==============================================###
# Define server logic
server <- function(input, output, session) {
  
  sample_subs <- reactiveValues()
  sample_subs$corr_data <- NULL
  sample_subs$corr_truth <- NULL
  
  #Update input boxes so user can't choose the same numerical variable
  #It doesn't completely matter, though the graph will be a line of slope = 1
  observeEvent(c(input$y_var, input$x_var), {
    y_var <- input$y_var
    x_var <- input$x_var
    choices <- num_vars
    if (y_var == x_var){
      choices <- choices[-which(choices == y_var)]
      updateSelectizeInput(session,
                           "x_var",
                           choices = choices)
    }
  }
  )
  
  #Update input boxes so user can't choose the same categorical variable
  observeEvent(c(input$cat1, input$cat2), {
    cat1 <- input$cat1
    cat2 <- input$cat2
    choices <- cat_vars
    if (cat2 == cat1){
      choices <- choices[-which(choices == cat1)]
      updateSelectizeInput(session,
                           "cat2",
                           choices = choices)
    }
  }
  )
  
  #Use an observeEvent() to look for the action button (subs_sample)
  observeEvent(input$subs_sample, {
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
    
    corr_vars <- c(input$x_var, input$y_var)
    
    subsetted_data <- usrbhvr_data |>
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
      sample_subs$corr_data, 
      aes_string(
        x = isolate(input$x_var), 
        y = isolate(input$y_var))) + 
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
