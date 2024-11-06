library(bslib)
library(cowplot)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(ggpie)
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
# Define UI 
ui <- fluidPage(
  h1("Categorical Subsetting"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Numeric Variables:"),
      selectizeInput(
        "y_var",
        label = "y-axis",
        choices = num_vars,
        selected = "Number of Apps Installed"
        ),
      h3("Select a Range"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
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
        selected = "Data Usage (MB/day)"
      ),
      h3("Select Another Range"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
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
      h2("Select Categorical Variable"),
      selectizeInput(
        "cat1_var",
        label = "Categorical Variable 1",
        choices = cat_vars,
        selected = "DeviceModel"
      ),
      h3("Select Option"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
      conditionalPanel(
        condition = "input.cat1_var == 'DeviceModel'",
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
        )
      ),
      conditionalPanel(
        condition = "input.cat1_var == 'OperatingSystem'",
        radioButtons(
          "OpSys_var",
          "Operating System",
          choices = list(
            "Android and iOS" = 1,
            "Android" = 2,
            "iOS" = 3
          ),
          selected = 1
        )
      ),
      conditionalPanel(
        condition = "input.cat1_var == 'UserBehaviorClass'",
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
        )
      ),
      conditionalPanel(
        condition = "input.cat1_var == 'Gender'",
        radioButtons(
          "Gender_var",
          "Gender",
          choices = list(
            "Female and Male" = 1,
            "Female" = 2,
            "Male" = 3
          ),
          selected = 1
        )
      ),
      h2("Select Other Categorical Variable"),
      selectizeInput(
        "cat2_var",
        label = "Categorical Variable 2",
        choices = cat_vars,
        selected = "OperatingSystem"
      ),
      h3("Select Option"), #https://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
      conditionalPanel(
        condition = "input.cat2_var == 'DeviceModel'",
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
        )
      ),
      conditionalPanel(
        condition = "input.cat2_var == 'OperatingSystem'",
        radioButtons(
          "OpSys_var",
          "Operating System",
          choices = list(
            "Android and iOS" = 1,
            "Android" = 2,
            "iOS" = 3
          ),
          selected = 1
        )
      ),
      conditionalPanel(
        condition = "input.cat2_var == 'UserBehaviorClass'",
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
        )
      ),
      conditionalPanel(
        condition = "input.cat2_var == 'Gender'",
        radioButtons(
          "Gender_var",
          "Gender",
          choices = list(
            "Female and Male" = 1,
            "Female" = 2,
            "Male" = 3
          ),
          selected = 1
        )
      ),
      actionButton("sbst","Subset the data")
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
          tabsetPanel(
            tabPanel("Summaries",
                     tabsetPanel(
                       tabPanel("Contingency Tables",
                                tabsetPanel(
                                  tabPanel("One-Way"),
                                  tabPanel("Two-Way")
                                )),
                       tabPanel("Numeric Summaries")
                     )),
            tabPanel("Plots",
                     fluidRow(
                       column(12,
                              plotOutput(
                                outputId = "histPlot"
                                )
                              ),
                       column(12,
                              plotOutput(
                                outputId = "kdPlot"
                                )
                              )),
                     fluidRow(
                       column(12,
                              plotOutput(
                                outputId = "boxPlot"
                                )
                              )
                       ),
                       column(12,
                              plotOutput(
                                outputId = "scatPlot"
                                )
                              )),
                    fluidRow(
                       column(12,
                              plotOutput(
                                outputId = "corrPlot"
                                )
                              ),
                       column(12,
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
        )

###==============================================###

###==============================================###
# Define server logic
server <- function(input, output, session) {
  
#  tsbs <- reactiveValues()
#  subsetted_data <- NULL
  subsetted_data <- NULL
  makeReactiveBinding("subsetted_data")
  
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
  observeEvent(c(input$cat1_var, input$cat2_var), {
    cat1_var <- input$cat1_var
    cat2_var <- input$cat2_var
    choices <- cat_vars
    if (cat1_var == cat2_var){
      choices <- choices[-which(choices == cat1_var)]
      updateSelectizeInput(session,
                           "cat2_var",
                           choices = choices)
    }
  }
  )
  
  #Use an observeEvent() to look for the action button (sbst)
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
    else if(input$DevMod_var == "OnePlus 9"){
      DevMod_sub <- DevModvals["3"]
    }
    else if(input$DevMod_var == "Samsung Galaxy S21"){
      DevMod_sub <- DevModvals["4"]
    }
    else {
      DevMod_sub <- DevModvals["5"]
    }
    
    if(input$OpSys_var == "Android and iOS"){
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
    
    if(input$Gender_var == "Female and Male"){
      Gender_sub <- Gendervals
    } 
    else if(input$Gender_var == "Female"){
      Gender_sub <- Gendervals["1"]
    } 
    else {
      Gender_sub <- Gendervals["2"]
    }
    
    subs_vars <- c(input$x_var, input$y_var)
    
    #Subset the data
    subsetted_data <<- usrbhvr_data %>%
      #Remove since this dataset is fine, need generally
      #filter(#cat vars first
      #DevModfac %in% DevMod_sub,
      #OpSysfac %in% OpSys_sub,
      #UBCfac %in% UBC_sub,
      #Genderfac %in% Gender_sub) |>
      #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("AppUsageTime" %in% subs_vars) filter(., AppUsageTime >= 0) else .} %>%
      {if("ScreenOnTime" %in% subs_vars) filter(., ScreenOnTime >= 0.0) else .} %>%
      {if("BatteryDrain" %in% subs_vars) filter(., BatteryDrain >=0) else .} %>%
      {if("NumberofAppsInstalled" %in% subs_vars) filter(., NumberofAppsInstalled >= 0) else .} %>%
      {if("DataUsage" %in% subs_vars) filter(., DataUsage >=0) else .} %>%
      {if("Age" %in% subs_vars) filter(., Age >= 18) else .}

  }
  ) #END observeEvent() for actionButton
  
  #Plot (1)
  #Create a renderPlot() object to output a histogram
#  output$histPlot <- renderPlot({
    #Use the code to validate that data exists
#    validate(
#      need(
#        !is.null(subsetted_data), 
#        "Please select your variables, subset, and click the 'Get a Sample!' button."
#      )
#    ) #this is a useful function to add as a placeholder until data is generated!
    #Plot chunk
#    ggplot(subsetted_data,
#           aes(
#             x = !!sym(input$x_var))) +
#      geom_histogram(
#        alpha=0.5,
#        aes(
#          fill = input$cat1_var, 
#          col=I("black")
#          ),
#        binwidth = 30,
#        stat = "count",
#        position = "identity") + 
#      scale_fill_manual(
#        values=c("#E69F00", "#56B4E9")) + 
#      xlab("x-var") + 
#      ylab("Number of Phones") + 
#      ggtitle(
#        str_wrap(
#          "Number of Phones with x-var, by Operating System",
#          45)
#        ) + 
#      theme(
#        plot.title = element_text(
#          hjust = 0.5)
#        )
#  })
  #Plot (2)
  #Generate kernel density plot
#  output$kdplot <- renderPlot({
#    validate(
#      need(
#        !is.null(subsetted_data), 
#        "Please select your variables, subset, and click the 'Get a Sample!' button."
#      )
#    )
#    ggplot(subsetted_data,
#           aes(x = isolate(input$x_var))) +
#      geom_density(alpha=0.5,
#                   aes(fill = input$cat2_var,
#                       col=I("black"))) + 
#      scale_fill_manual(values=
#                          c("#E69F00",
#                            "#56B4E9"
#                            )
#                        ) + 
#      xlab("TBD") + 
#      ylab("Density of x_var") +
#      ggtitle(
#        str_wrap("TBD"
#                 )
#        ) +
#      theme(
#        plot.title = element_text(
#          hjust=0.5))
#  })
  #Plot (3)
  #Generate box plot
#  output$boxPlot <- renderPlot({
#    validate(
#      need(
#        !is.null(subsetted_data), 
#        "Please select your variables, subset, and click the 'Get a Sample!' button."
#      )
#    ) 
#    ggplot(subsetted_data) + 
#    geom_boxplot(
#      aes(x = isolate(input$cat1_var), 
#          y = isolate(input$y_var), 
#          fill = input$cat2_var)) + 
#    scale_fill_manual(
#      values=c("#E69F00", "#56B4E9")) + 
#    xlab("Device Model") + 
#    ylab("y-var") + 
#    ggtitle(
#      str_wrap(
#        "Box Plot of y-var, by Device Model",
#        45)) + 
#    theme(plot.title = element_text(hjust = 0.5))
#  }
#  )
  #Plot (4)
  #Generate scatterplot
  output$scatPlot <- renderPlot({
    validate(
      need(
        !is.null(subsetted_data), 
        "Please select your variables, subset, and click the 'Subset the data!' button."
        )
    )
  ggplot(
    subsetted_data,
    aes_string(
      x = input$x_var,
      y = input$y_var
    )
    ) +
    geom_point() +
    xlim(input$Range_x[1],input$Range_x[2]) +
    ylim(input$Range_y[1],input$Range_y[2])
  }
  )
  #Plot (5)
  #Generate correlation plot
#  output$corrPlot <- renderPlot({
#    validate(
#      need(
#        !is.null(subsetted_data), 
#        "Please select your variables, subset, and click the 'Get a Sample!' button."
#      )
#    )
#    subs_data_num <- subsetted_data |> 
#      filter(
#        input$cat2_var==input$cat2_var["2"]
#        ) |> 
#      select(
#      -c(UserID,
#         Age,
#         DeviceModel,
#         OperatingSystem,
#         Gender,
#         UserBehaviorClass)
#      )
#    corr <- round(
#      cor(
#        subsetted_data
#        ),
#      1
#      )
#    ggcorrplot(corr)
#  })
  #Plot (6)
  #Generate pie chart
  #https://www.csc2.ncsu.edu/faculty/healey/msa/shiny/
#  output$piePlot <- renderPlot({
#    validate(
#      need(
#        !is.null(subsetted_data), 
#        "Please select your variables, subset, and click the 'Get a Sample!' button."
#      )
#    )
#    ggpie(subsetted_data |> 
#            filter(
#              input$cat2_var==input$cat2_var["1"]
#              ), 
#          group_key = input$cat1_var, 
#          count_type = "full",
#          label_info = "all", 
#          label_type = "horizon",
#          label_size = 4, 
#          label_pos = "out" )
#  }
#  )
}

# Run the application 
shinyApp(ui = ui, server = server)
