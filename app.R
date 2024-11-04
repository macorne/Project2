library(bslib)
library(ggplot2)
library(ggcorrplot)
library(ggpie)
library(shiny)
library(shinyalert)
library(tidyverse)
source("helpers.R")


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
        "Range_y",
        "Value:",
        min=30,
        max=598,
        value = c(30,598)
      )
      ),
      conditionalPanel(
        condition = "input.y_var == 'ScreenOnTime'",
        sliderInput(
          "Range_y",
          "Value:",
          min=1.0,
          max=12.0,
          value = c(1.0,12.0)
        )
      ),
      conditionalPanel(
        condition = "input.y_var == 'BatteryDrain'",
        sliderInput(
          "Range_y",
          "Value:",
          min=302,
          max=2993,
          value = c(302,2993)
        )
      ),
      conditionalPanel(
        condition = "input.y_var == 'NumberofAppsInstalled'",
        sliderInput(
          "Range_y",
          "Value:",
          min=10,
          max=99,
          value = c(10,99)
        )
      ),
      conditionalPanel(
        condition = "input.y_var == 'DataUsage'",
        sliderInput(
          "Range_y",
          "Value:",
          min=102,
          max=2497,
          value = c(102,2497)
        )
      ),
      conditionalPanel(
        condition = "input.y_var == 'Age'",
        sliderInput(
          "Range_y",
          "Value:",
          min=18,
          max=59,
          value = c(18,59)
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
        condition = "input.x_var == 'AppUsageTime'",
        sliderInput(
          "Range_x",
          "Value:",
          min=30,
          max=598,
          value = c(30,598)
        )
      ),
      conditionalPanel(
        condition = "input.x_var == 'ScreenOnTime'",
        sliderInput(
          "Range_x",
          "Value:",
          min=1.0,
          max=12.0,
          value = c(1.0,12.0)
        )
      ),
      conditionalPanel(
        condition = "input.x_var == 'BatteryDrain'",
        sliderInput(
          "Range_x",
          "Value:",
          min=302,
          max=2993,
          value = c(302,2993)
        )
      ),
      conditionalPanel(
        condition = "input.x_var == 'NumberofAppsInstalled'",
        sliderInput(
          "Range_x",
          "Value:",
          min=10,
          max=99,
          value = c(10,99)
        )
      ),
      conditionalPanel(
        condition = "input.x_var == 'DataUsage'",
        sliderInput(
          "Range_x",
          "Value:",
          min=102,
          max=2497,
          value = c(102,2497)
        )
      ),
      conditionalPanel(
        condition = "input.x_var == 'Age'",
        sliderInput(
          "Range_x",
          "Value:",
          min=18,
          max=59,
          value = c(18,59)
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
      h2("Select Gender"),
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
      tabsetPanel(
        tabPanel(
          "About"
        ),
        tabPanel(
          "Data Download"
        ),
        tabPanel(
          "Data Exploration",
          fluidRow(
            column(6,
                   plotOutput(
                     outputId = "histPlot"
                     )
                   ),
            column(6,
                   plotOutput(
                     outputId = "kdPlot"
                     )
                   ),
            column(6,
                   plotOutput(
                     outputId = "boxPlot"
                     )
                   )
            ),
          fluidRow(
            column(6,
                   plotOutput(
                     outputId = "scatPlot"
                     )
                   ),
            column(6,
                   plotOutput(
                     outputId = "corrPlot"
                     )
                   ),
            column(6,
                   plotOutput(
                     outputId = "piePlot"
                     )
                   )
            )
          )
        )
      )
    )
  )

usrbhvr_data <- read_csv("user_behavior_dataset.csv")
#Fix variable names since these have spaces in some cases
names(usrbhvr_data) <- gsub("\\([^\\)]+\\)", "",
                            str_replace_all(names(usrbhvr_data),
                                            c(" " = "")))
usrbhvr_data <- usrbhvr_data |>
    mutate(UBCfac = 
             factor(
               UserBehaviorClass,
               levels=c(1,2,3,4,5),
               labels=c(
                 "Between 0 and 300 MB/day",
                 "Between 300 and 600 MB/day",
                 "Between 600 and 1,000 MB/day",
                 "Between 1,000 and 1,500 MB/day",
                 "More than 1,500 MB/day"
                 )
               ),
           OpSysfac = 
             factor(
               OperatingSystem,
               levels=c(
                 "Android",
                 "iOS"
                 )
               ),
           DevModfac = 
             factor(
               DeviceModel,
               levels=c(
                 "Google Pixel 5",
                 "OnePlus 9",
                 "Samsung Galaxy S21",
                 "Xiaomi Mi 11",
                 "iPhone 12"
                 )
               ),
           Genderfac = 
             factor(
               Gender,
               levels=c(
                 "Female",
                 "Male")
               )
    )

###==============================================###
# Define server logic
server <- function(input, output, session) {
  
  sample_subs <- reactiveValues()
  subsetted_data <- NULL
#  sample_subs$subs_truth <- NULL
  
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
  
  
  #Use an observeEvent() to look for the action button (subs_sample)
  observeEvent(input$subs_sample, {
    if(input$DevMod_var == "All"){
      DevMod_sub <- HHLvals
    } 
    else if(input$DevMod_var == "Google Pixel 5"){
      DevMod_sub <- DevModvals["1"]
    } 
    else if(input$DevMod_var == "iPhone 12"){
      DevMod_sub <- DevModvals["2"]
    } 
    else if(input$DevMod_var == "OnePlus 9"){
      DevMod_sub <- DevModvals["3"]
    }
    else if(input$DevMod_var == "Samsung Galaxy S21"){
      DevMod_sub <- DevModvals["4"]
    }
    else {
      DevMod_sub <- DevModvals["5"]
    }
    
    if(input$OpSys_var == "All"){
      OpSys_sub <- OpSysvals
    } 
    else if(input$OpSys_var == "Android"){
      OpSys_sub <- OpSysvals["1"]
    } 
    else {
      OpSys_sub <- OpSysvals["2"]
    }
    
    if(input$UBC_var == "All"){
      UBC_sub <- UBCvals
    } 
    else if(input$UBC_var == "Between 0 and 300 MB/day"){
      UBC_sub <- UBCvals["1"]
    } 
    else if(input$UBC_var == "Between 300 and 600 MB/day"){
      UBC_sub <- UBCvals["2"]
    } 
    else if(input$UBC_var == "Between 600 and 1,000 MB/day"){
      UBC_sub <- UBCvals["3"]
    } 
    else if(input$UBC_var == "Between 1,000 and 1,500 MB/day"){
      UBC_sub <- UBCvals["4"]
    } 
    else {
      UBC_sub <- UBCvals["5"]
    }
    
    if(input$Gender_var == "All"){
      Gender_sub <- Gendervals
    } 
    else if(input$Gender_var == "Female"){
      Gender_sub <- Gendervals["1"]
    } 
    else {
      Gender_sub <- Gendervals["2"]
    }
    
    subs_vars <- c(input$x_var, input$y_var)
    
    subsetted_data <- usrbhvr_data |>
      filter(#cat vars first
        DevModfac %in% DevMod_sub,
        OpSysfac %in% OpSys_sub,
        UBCfac %in% UBC_sub,
        Genderfac %in% Gender_sub
      ) %>% 
      #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("AppUsageTime" %in% subs_vars) filter(., AppUsageTime >= 30 & AppUsageTime <= 598) else .} %>%
      {if("ScreenOnTime" %in% subs_vars) filter(., ScreenOnTime >= 1.0 & ScreenOnTime <= 12.0) else .} %>%
      {if("BatteryDrain" %in% subs_vars) filter(., BatteryDrain >= 302 & BatteryDrain <= 2993) else .} %>%
      {if("NumberofAppsInstalled" %in% subs_vars) filter(., NumberofAppsInstalled >= 10 & NumberofAppsInstalled <= 99) else .} %>%
      {if("DataUsage" %in% subs_vars) filter(., DataUsage >= 102 & DataUsage <= 2497) else .} %>%
      {if("Age" %in% subs_vars) filter(., Age >= 18 & Age <= 59) else .}
    
#    index <- sample(
#      1:nrow(subsetted_data), 
#      size = input$Range_y, 
#      replace = TRUE, 
#      prob = 1/length(usrbhvr_data$UserID)
#    )
    
#    index <- sample(
#      1:nrow(subsetted_data), 
#      size = input$Range_x, 
#      replace = TRUE, 
#      prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP)
#    )
    
#    sample_subs$subs_data <- subsetted_data[index,]
#    sample_subs$subs_data <- subsetted_data
  }
  )
  
  #Plot (1)
  #Create a renderPlot() object to output a histogram
  output$histPlot <- renderPlot({
    #Use the code below to validate that data exists,
    #then create the appropriate scatter plot
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
      )
    ) #this is a useful function to add as a placeholder until data is generated!
    ggplot(subsetted_data,
           aes(
             x = isolate(input$x_var))) +
      geom_histogram(
        alpha=0.5,
        aes(
          fill = input$OpSys_var, 
          col=I("black")
          ),
        position = "identity") + 
      scale_fill_manual(
        values=c("#E69F00", "#56B4E9")) + 
      xlab("Battery Drain (mAh/day)") + 
      ylab("Number of Phones") + 
      ggtitle(
        str_wrap(
          "Number of Phones with x-var, by Operating System",
          45)
        ) + 
      theme(
        plot.title = element_text(
          hjust = 0.5)
        )
  })
  #Plot (2)
  #Generate kernel density plot
  output$kdplot <- renderPlot({
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
      )
    )
    ggplot(subsetted_data,
           aes(x = isolate(input$x_var))) +
      geom_density(alpha=0.5,
                   aes(fill = address,
                       col=I("black"))) + 
      scale_fill_manual(values=
                          c("#E69F00",
                            "#56B4E9"
                            )
                        ) + 
      xlab("TBD") + 
      ylab("Density of x_var") +
      ggtitle(
        str_wrap("TBD"
                 )
        ) +
      theme(
        plot.title = element_text(
          hjust=0.5))
  })
  #Plot (3)
  #Generate box plot
  output$boxPlot <- renderPlot({
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
      )
    ) 
    ggplot(subsetted_data) + 
    geom_boxplot(
      aes(x = isolate(input$DevMod_var), 
          y = isolate(input$y_var), 
          fill = Gender)) + 
    scale_fill_manual(
      values=c("#E69F00", "#56B4E9")) + 
    xlab("Device Model") + 
    ylab("y-var") + 
    ggtitle(
      str_wrap(
        "Box Plot of y-var, by Device Model",
        45)) + 
    theme(plot.title = element_text(hjust = 0.5))
  }
  )
  #Plot (4)
  #Generate scatterplot
  output$scatPlot <- renderPlot({
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
        )
    )
  ggplot(subsetted_data,
         aes_string(
           x = isolate(input$x_var),
           y = isolate(input$y_var), 
           color = isolate(input$OpSys_var))) +
    geom_point() +
    scale_color_manual(
      values=c("#E69F00", "#56B4E9")) +
    geom_jitter(width = 0.2, alpha = 0.3)
  }
  )
  #Plot (5)
  #Generate correlation plot
  output$corrPlot <- renderPlot({
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
      )
    )
    subs_data_num <- subsetted_data |> 
      filter(
        OperatingSystem=="input$OpSys_var"
        ) |> 
      select(
      -c(UserID,
         Age,
         AgeF,
         DeviceModel,
         OperatingSystem,
         Gender,
         UserBehaviorClass)
      )
    corr <- round(
      cor(
        usrbhvr_numdata
        ),
      1
      )
    ggcorrplot(corr)
  })
  #Plot (6)
  #Generate pie chart
  output$piePlot <- renderPlot({
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Get a Sample!' button."
      )
    )
    ggpie(subsetted_data |> 
          filter(Gender=="Gender_var"), 
          group_key = "DevMod_var", 
          count_type = "full",
          label_info = "all", 
          label_type = "horizon",
          label_size = 4, 
          label_pos = "out" )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
