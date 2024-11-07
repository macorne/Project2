library(shiny)
library(shinyalert)
library(tidyverse)
source("helpers.R")

###==============================================###

#Data set
usrbhvr_data <- read_csv("user_behavior_dataset.csv")

#Fix variable names since these have spaces in some cases
names(usrbhvr_data) <- gsub("\\([^\\)]+\\)", "",
                            str_replace_all(names(usrbhvr_data),
                                            c(" " = "")))

#Modify data to get factor variables
usrbhvr_data <- usrbhvr_data |> 
  mutate(
    AgeF = dplyr::case_when(
      Age >= 18 & Age < 25 ~ "18-24",
      Age >= 25 & Age < 35 ~ "25-34",
      Age >= 35 & Age < 45 ~ "35-44",
      Age >= 45 & Age < 55 ~ "45-54",
      Age >= 55            ~ "55+"
    ),
    # Convert to factor
    AgeF = factor(
      AgeF,
      level = c("18-24", "25-34","35-44","45-54","55+")
    )
  ) |>
  mutate(UBCfac = 
           factor(UserBehaviorClass, 
                  levels=c(1,2,3,4,5), 
                  labels=c("Between 0 and 300 MB/day", 
                           "Between 300 and 600 MB/day",
                           "Between 600 and 1,000 MB/day",
                           "Between 1,000 and 1,500 MB/day",
                           "More than 1,500 MB/day")),
         OpSysfac = 
           factor(OperatingSystem,
                  levels=c(1,2),
                  labels=c("Android",
                           "iOS")),
         DevModfac = 
           factor(DeviceModel,
                  levels=c(1,2,3,4,5),
                  labels=c("Google Pixel 5",
                           "OnePlus 9",
                           "Samsung Galaxy S21",
                           "Xiaomi Mi 11",
                           "iPhone 12")),
         Genderfac = 
           factor(Gender,
                  levels=c(1,2),
                  labels=c("Female","Male"))
  )

usrbhvr_data

#Assign weight
usrbhvr_data <- usrbhvr_data |>
  mutate(weight=1)

usrbhvr_test <- usrbhvr_data
###==============================================###

###==============================================###
# Define UI for application that draws a scatterplot 
# and obtains a correlation
ui <- fluidPage(
  "Correlation Exploration",
  sidebarLayout(
    sidebarPanel(
      h2("Select y-Variables:"),
      selectizeInput(
        "y_var",
        label = "y Variable",
        choices = num_vars,
        selected = "Battery Drain (mAh/day)"
      ),
      selectizeInput(
        "x_var",
        label = "x Variable",
        choices = num_vars,
        selected = "Number of Apps Installed"
      ),
      h2("Choose a subset of the data:"),
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
      radioButtons(
        "Gender_var",
        "Gender",
        choices = list(
          "Female & Male" = 1,
          "Female" = 2,
          "Male" = 3
        ),
        selected = 1
      ),
      h2("Subset Your Data"),
      actionButton("sbst","Subset!")
    ), #END sidebarPanel
    mainPanel(
      plotOutput(
        outputId = "distPlot"
      )
    )
  )
)

###==============================================###
# Define server logic required to draw a scatterplot
server <- function(input, output, session) {
  
#  subsetted_data <- NULL
#  makeReactiveBinding("subsetted_data")
  tsbs <- reactiveValues()
  tsbs$sbst <- NULL
  
  #Update input boxes so user can't choose the same variable
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
  
  #Use an observeEvent() to look for the action button (corr_sample)
  observeEvent(input$sbst, {
    
    if(input$DevMod_var == "All"){
      DevMod_sub <- DevModvals
    } 
    else if(input$DevMod_var == "Google Pixel 5"){
      DevMod_sub <- DevModvals["1"]
    } 
    else if(input$DevMod_var == "iPhone 12"){
      DevMod_sub <- DevModvals["2"]
    } 
    else if (input$DevMod_var == "OnePlus 9"){
      DevMod_sub <- DevModvals["3"]
    }
    else if (input$DevMod_var == "Samsung Galaxy S21"){
      DevMod_sub <- DevModvals["4"]
    }
    else {
      DevMod_sub <- DevModvals["5"]
    }
    
    if(input$Gender_var == "Female & Male"){
      Gender_sub <- Gendervals
    } 
    else if(input$Gender_var == "Female"){
      Gender_sub <- Gendervals["1"]
    } 
    else {
      Gender_sub <- Gendervals["2"]
    }
    
    subs_vars <- c(input$y_var, input$x_var)
    
    subsetted_data <- usrbhvr_data %>%
      #Removed since unnecessary for this dataset, but generally necessary
      #      filter(#cat vars first
      #        OpSysfac %in% OpSys_sub,
      #        DevModfac %in% DevMod_sub,
      #        Genderfac %in% Gender_sub) |>
      #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("AppUsageTime" %in% subs_vars) filter(., AppUsageTime >= 0) else .} %>%
      {if("ScreenOnTime" %in% subs_vars) filter(., ScreenOnTime >= 0.0) else .} %>%
      {if("BatteryDrain" %in% subs_vars) filter(., BatteryDrain >=0) else .} %>%
      {if("NumberofAppsInstalled" %in% subs_vars) filter(., NumberofAppsInstalled >= 0) else .} %>%
      {if("DataUsage" %in% subs_vars) filter(., DataUsage >=0) else .} %>%
      {if("Age" %in% subs_vars) filter(., Age >= 0) else .}

    
      index <- 1:nrow(subsetted_data)
      
      tsbs$sbst <- subsetted_data[index,]  
  }
  )
  
  observeEvent(input$sbst, {
  #Create a renderPlot() object to output a scatter plot
  output$distPlot <- renderPlot({
    #Use the code below to validate that data exists,
    #then create the appropriate scatter plot
    validate(
      need(
        !is.null(tsbs$sbst), 
        "Please select your variables, subset, and click the 'Subset' button."
      )
    ) #this is a useful function to add as a placeholder until data is generated!
    ggplot(
      tsbs$sbst, 
      aes(
        x = !!sym(isolate(input$x_var)),
        y = !!sym(isolate(input$y_var)),
        color = Gender)) + 
      geom_point() +
      scale_color_manual(values=c("#E69F00", "#56B4E9")) +
      geom_jitter(width = 0.2, alpha = 0.3)
  })
  
  })
  
} #END server()

# Run the application 
shinyApp(ui = ui, server = server)
