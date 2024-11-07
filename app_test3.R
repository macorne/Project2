library(ggplot2)
library(shiny)
library(shinyalert)
library(tidyverse)
library(ggpie)
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
  h3("Categorical Subsetting"),
  sidebarLayout(
    sidebarPanel(
      h2("Select y-Variable:"),
      selectizeInput(
        "y_var",
        label = "y Variable",
        choices = num_vars,
        selected = "Battery Drain (mAh/day)"
      ),
      h2("Select x-Variable:"),
      selectizeInput(
        "x_var",
        label = "x Variable",
        choices = num_vars,
        selected = "Number of Apps Installed"
      ),
      h2("Select An Option For The Categorical Variable:"),
      radioButtons(
        "OpSys_var",
        "Operating System",
        choices = list(
          "Android & iOS" = 1,
          "Android" = 2,
          "iOS" = 3
        ),
        selected = 1
      ),
      h2("Select An Option For The Other Categorical Variable:"),
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
                                  tabPanel("One-Way",
                                           tableOutput(
                                             outputId = "opsysTable"
                                           ),
                                           tableOutput(
                                             outputId = "genderTable"
                                           )),
                                  tabPanel("Two-Way")
                                )),
                       tabPanel("Numeric Summaries")
                     )),
            tabPanel("Plots",
                     tabsetPanel(
                       tabPanel("Kernel Density Plot",
                                plotOutput(
                                  outputId = "kdPlot"
                                  )),
                       tabPanel("Scatterplot",
                                plotOutput(
                                  outputId = "scatPlot"
                                  )),
                       tabPanel("Box Plot",
                                plotOutput(
                                  outputId = "boxPlot"
                                )),
                       tabPanel("Pie Chart",
                                plotOutput(
                                  outputId = "piePlot"
                                )),
                       tabPanel("Correlation Plot",
                                plotOutput(
                                  outputId = "corrPlot"
                                ))
                     )
                     )
                     )
          )))))
    
  

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
    
    if(input$OpSys_var == "Android & iOS"){
      OpSys_sub <- OpSysvals
    } 
    else if(input$OpSys_var == "Android"){
      OpSys_sub <- OpSysvals["1"]
    } 
    else {
      OpSys_sub <- OpSysvals["2"]
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
    
    #One-way contingency tables
    #Operating System
    output$opsysTable <- renderTable({
      #Use the code to validate that data exists
      validate(
        need(
          !is.null(subsetted_data), 
          "Please select your variables, subset, and click the 'Subset' button."
        )
      ) #this is a useful function to add as a placeholder until data is generated!
      table(tsbs$sbst$OperatingSystem,dnn=list("OperatingSystem"))
    })
    
    #Gender
    output$genderTable <- renderTable({
      #Use the code to validate that data exists
      validate(
        need(
          !is.null(subsetted_data), 
          "Please select your variables, subset, and click the 'Subset' button."
        )
      ) #this is a useful function to add as a placeholder until data is generated!
      table(tsbs$sbst$Gender,dnn=list("Gender"))
    })
    
    #Plot (1)
    #Create a renderPlot() object to output a kernel density plot
      output$kdPlot <- renderPlot({
    #Use the code to validate that data exists
        validate(
          need(
            !is.null(subsetted_data), 
            "Please select your variables, subset, and click the 'Subset' button."
          )
        ) #this is a useful function to add as a placeholder until data is generated!
    #Plot chunk
        ggplot(tsbs$sbst,
               aes(
                 x = !!sym(isolate(input$x_var)))) +
          geom_density(
            alpha=0.5,
            aes(
              fill = OperatingSystem, 
              col=I("black")
              ),
            position = "identity") + 
          scale_fill_manual(
            values=c("#E69F00", "#56B4E9")) + 
          ylab("Number of Phones") + 
          ggtitle(
            str_wrap(
              "TBD",
              45)
            ) + 
          theme(
            plot.title = element_text(
              hjust = 0.5)
            )
      })
    #Plot (2)
    #Generate scatterplot
    output$scatPlot <- renderPlot({
      #Use the code below to validate that data exists,
      #then create the appropriate scatter plot
      validate(
        need(
          !is.null(tsbs$sbst), 
          "Please select your variables, subset, and click the 'Subset' button."
        )
      ) #this is a useful function to add as a placeholder until data is generated!
      ggplot(tsbs$sbst, 
        aes(
          x = !!sym(isolate(input$x_var)),
          y = !!sym(isolate(input$y_var)),
          color = Gender)) + 
        geom_point() +
        scale_color_manual(values=c("#E69F00", "#56B4E9")) +
        geom_jitter(width = 0.2, alpha = 0.3)
    })
    #Plot (3) 
    #Boxplot for BatteryDrain across Operating System
    output$boxPlot <- renderPlot({
      #Use the code below to validate that data exists,
      #then create the appropriate scatter plot
      validate(
        need(
          !is.null(tsbs$sbst), 
          "Please select your variables, subset, and click the 'Subset' button."
        )
      ) #this is a useful function to add as a placeholder until data is generated!
      
    ggplot(tsbs$sbst) + 
      geom_boxplot(
        aes(x = OperatingSystem, 
            y = BatteryDrain, 
            fill = Gender)) + 
      scale_fill_manual(
        values=c("#E69F00", "#56B4E9")) + 
      xlab("Operating System") + 
      ylab("Battery Drain (mAh/day)") + 
      ggtitle(
        str_wrap(
          "Box Plot of Battery Drain Per Day, by Gender",
          45)) + 
      theme(plot.title = element_text(hjust = 0.5))
    })
    #Plot (4) 
    #Pie Chart
    output$piePlot <- renderPlot({
      #Use the code below to validate that data exists,
      #then create the appropriate scatter plot
      validate(
        need(
          !is.null(tsbs$sbst), 
          "Please select your variables, subset, and click the 'Subset' button."
        )
      ) #this is a useful function to add as a placeholder until data is generated!
      
      ggpie(tsbs$sbst, 
            group_key = "DeviceModel", 
            count_type = "full",
            label_info = "all", 
            label_type = "horizon",
            label_size = 4, 
            label_pos = "out" )
    })
    #Plot (5) 
    #Correlation plot (ggcorrplot)
    #Compute a correlation matrix for numeric variables
    output$corrPlot <- renderPlot({
      #Use the code below to validate that data exists,
      #then create the appropriate scatter plot
      validate(
        need(
          !is.null(tsbs$sbst), 
          "Please select your variables, subset, and click the 'Subset' button."
        )
      ) #this is a useful function to add as a placeholder until data is generated!
      
    sbst_num <- tsbs$sbst |>
      select(
      -c(UserID,Age,AgeF,DeviceModel,OperatingSystem,Gender,UserBehaviorClass))
    corr <- round(cor(usrbhvr_numdata), 1)
    ggcorrplot(corr)
    })
    
  })
  
} #END server()

# Run the application 
shinyApp(ui = ui, server = server)
