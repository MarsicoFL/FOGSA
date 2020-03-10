## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(crosstalk)
library(tidyverse)

options(shiny.host = '192.168.148.172')
options(shiny.port = 8080)


ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "FOGSA"),
  dashboardSidebar(sidebarMenu(
                  menuItem("Description", tabName = "Description", icon = icon("th")),
                  menuItem("Inputs", tabName = "Inputs", icon = icon("arrow-alt-circle-up")),
                  #menuItem("Pedigri", tabName = "Pedigri", icon = icon("dna")),
                  menuItem("Descriptive", tabName = "Descriptive statistics", icon = icon("chart-line"),
                           menuSubItem("Summary", tabName = "Summary"),
                           menuSubItem("LRdistributions", tabName = "LRdistributions"),
                           menuSubItem("PowerPlot", tabName = "PowerPlot"),
                           menuSubItem("Rates", tabName = "Rates")
                  ),
                  menuItem("LRdt", tabName = "LRdt", icon = icon("eye")),
                  sliderInput("LR", "LR thershold:",
                              min = 1, max = 500,
                              value = 1, step = 1,
                              animate =
                                animationOptions(interval = 10, loop = TRUE))
  )
  ),
    dashboardBody(
      tabItems(
      # First tab content
        tabItem(tabName = "Description",
                h2("FOrensic Genetics Simulation Analysis", align= "Center"),
        ),
        
       # second tab content
        tabItem(tabName = "Inputs",
                h2("Uploading data"),
                fluidRow(
                  fileInput("file1", "Familias simulation output",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  ),
                  tags$hr(),
                  ),
        ),
       # third tab content
       tabItem(tabName = "LRdistributions",
               mainPanel(
                 plotOutput(outputId = "LRdistributions")),
               mainPanel(
                 plotOutput(outputId = "LRdistributions2"))),
         
       # four tab content
       tabItem(tabName = "Summary",
               sidebarPanel(
                 h4("Obtained LR"),
                 tableOutput("contents")),
               mainPanel(
                 h2("Descriptive statistics"),
                 h3("Unrelated"),
                 verbatimTextOutput("SumX1"),
                 h3("Related"),
                 verbatimTextOutput("SumX2")
               )),
       # five tab content
       tabItem(tabName = "PowerPlot",
               plotOutput(outputId = "PowerPlot")),
       # six tab content
       tabItem(tabName = "Rates",
               h3("Rates calculation in function of LR threshold selected:"),
               tableOutput("Rates")),
       # seven tab content
       tabItem(tabName = "LRdt",
               h3("Likelihood Ratio Decision Threshold", align= "Center"),
               h4("X axis: False Negatives Rates"),
               h4("Y axis: False Positives Rates"),
               plotlyOutput("LRdet"))
      )
    )
  )

server <- function(input, output) { 
  ##### Uploading the file 
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.delim(inFile$datapath, col.names = c("Unrelated", "Related"), header = FALSE, skip = 2, sep = "\t")
  })
  ##### Obtaining LR distribution graph
  output$LRdistributions <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                        grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    
    plot(density(log10(X2)), 
         xlab= "log10 LR", 
         ylab= "Densidad", 
         col=1, 
         lwd=2, 
         main="LR distribution - Related" 
         )
  })
  
  output$LRdistributions2 <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    
    plot(density(log10(X1)), 
         xlab= "log10 LR", 
         ylab= "Densidad", 
         col=2, 
         lwd=2, 
         main="LR distribution - Unrelated" 
    )
  })
  
  
  ##### Power Plot analysis
  output$PowerPlot <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                        grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    
    PI <- (sum(X2>10000))/length(X2)
    PE <- (sum(X1 == 0))/length(X1)
    
    
    plot(PI, PE, 
         type = "p",
         xlab= "Power of Inclution", 
         ylab= "Power of Exclution", 
         col=2,
         pch = 19,
         lwd=2, 
         main="Power Plot",
         xlim = c(0,1),
         ylim = c(0,1)
         )
  })
  
  output$SumX1 <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    summary(X1)
    })
  
  output$SumX2 <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    summary(X2)
  })
  
  sliderValues <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    
      FP <- sum(X1 > as.numeric(input$LR))/length(X1)
      FN <- sum(X2 < as.numeric(input$LR))/length(X1)
      TP <- sum(X2 > as.numeric(input$LR))/length(X1)
      TN <- sum(X1 < as.numeric(input$LR))/length(X1)
      Accuracy <- (TP+TN)/(TP+TN+FP+FN)
      MCC<-(TP*TN-FP*FN)/(sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN))
      
      data.frame(
        Parameter = c("False Positives Rates",
                      "False Negatives Rates",
                      "True Positives Rates",
                      "True Negatives Rates",
                      "Accuracy",
                      "Matthews Coef."
        ),
        Values = as.numeric(c(FP,
                             FN,
                             TP,
                             TN,         
                             Accuracy ,
                             MCC
        )
        ))})
    
  output$Rates <- renderTable({
    sliderValues()
  })
  
  output$LRdet <- renderPlotly({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    
    LRvalues = seq(1, 10000, length.out=10000) 
    FPs = 0
    FNs = 0
    for(i in 1:10000) { FPs[i] = sum(X1 > LRvalues[i]); #False Positives.
                        FNs[i] = sum(X2 < LRvalues[i])} #False negatives.
    
    Datas = data.frame(x = LRvalues, y= FPs/length(X1), z= FNs/length(X2), w=FPs)
    
    p <- plot_ly(
      Datas, x = Datas$z, y = Datas$y,
      text = ~paste("LR: ", Datas$x),
      color = Datas$z, size = 1
    )
      
    
  })
}

shinyApp(ui, server)

