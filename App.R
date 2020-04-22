## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(crosstalk)
library(tidyverse)
library(viridis)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "FOGSA"),
  dashboardSidebar(sidebarMenu(
                  #menuItem("Description", tabName = "Description", icon = icon("")),
                  menuItem("Inputs", tabName = "Inputs", icon = icon("arrow-alt-circle-up")),
                  #menuItem("Pedigri", tabName = "Pedigri", icon = icon("dna")),
                  menuItem("Descriptive statistics", tabName = "Descriptive statistics", icon = icon("chart-line"),
                           menuSubItem("Summary", tabName = "Summary"),
                           menuSubItem("LRdistributions", tabName = "LRdistributions"),
                           menuSubItem("PowerPlot", tabName = "PowerPlot"),
                           menuSubItem("Rates", tabName = "Rates")
                  ),
                  menuItem("LRdt", tabName = "LRdt", icon = icon("eye")))
                  
  ),
    dashboardBody(
      tabItems(
      # First tab content
      # tabItem(tabName = "Description",
      # second tab content
        tabItem(tabName = "Inputs",
                h2("FOrensic Genetics Simulation Analysis", align= "Center"),
                HTML("<h5>Purpose: Despite that the use of DNA in Missing Person Identification and Disaster Victim Identification cases has gained increasing focus during the last two decades, there still are several complications that the forensic scientists are faced with, for instance complex family structures and large scale comparisons are examples. Evaluating the impact of these complications in the identification process is one of the main tasks of the forensic genetics statistics. The development of algorithms and the application of statistical analysis in order to measure the uncertainty and minimize the probability of both false positive and false negatives types of error can be crucial to the purpose of the Missing Person Identification. FOGSA (Forensic Genetics Simulation Analysis) provides an userfriendly interface that allows the analysis of the simulation Raw Data from different softwares such as Familias R version and Forrel Package. FOGSA aimed at helping in the statistical analysis for decision making in forensic genetic caseworks.<h5>"),
                h3("Start uploading your simulation Raw Data"),
                fluidRow(
                  fileInput("file1", "Familias simulation output",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  ),
                  tags$hr(),
                  )),
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
                 verbatimTextOutput("SumX2"),
                 plotOutput("boxplot1")
               )),
       # five tab content
       tabItem(tabName = "PowerPlot",
               h2("Statistical Power Evaluation"),
               HTML("<h5>For a given pedigree the statistical power of the DNA-based identification could be calculated from the simulated data. Two measures could help in order to characterize a family group: Inclution Power (IP): It is the probability of obtaining a likelihood ratio of zero when the tested person (POI) is not related with the family group. Exclution Power (EP): The probability of obtaining a likelihood ratio upper than 10000 when the tested person (POI) is actually the missing person (MP). The Power Plot could be perfomed combining this two measures. A family group with low PI and PE has low statistical power where a high probability of false positives and false negatives could be expected. In this cases more genetic information must be incorporated to the pedigree. It could be done by the incorporation of more genetic markers or more members. As counterpart, a pedigree with high IP and EP allows performing an identification with an accurate result.<br/><br/><br/><h5>"),
               plotOutput(outputId = "PowerPlot")),
       # six tab content
       tabItem(tabName = "Rates",
               h3("Rates calculation in function of LR threshold selected", align = "Center"),
               HTML("<h5> False positive and false negative rates could be estimated for each Likelihood ratio selected as threshold (please select one using the sliderbar). Taking into account this measures, Matthews Correlation Coefficient and Acuracy can be calculated. Both measures gives an idea about how accurate is the LR threshold selected. If you are working with massive comparisons in genetic databases the expected number of false positives could be approximated multiplying the false positive rate by the database size.<br/><br/><h5>"),
               tableOutput("Rates"),
               # Copy the line below to make a number input box into the UI.
               numericInput("num", label = h3("Numeric input"), value = 1),
               hr(),
               fluidRow(column(3, verbatimTextOutput("LR threshold value")))),
       # seven tab content
       tabItem(tabName = "LRdt",
               h3("Likelihood Ratio Decision Threshold Plot", align= "Center"),
               HTML("<h5> <b>Some remarks:</b> In massive comparisons carried out in Missing Person Identification cases, matches (LR values upper the LR threshold) are re-analyzed by geneticists and more genetic (Y-Chromosome, X-Chromosome and mitochondrial DNA) and non-genetic information (date of birth) is incorporated to the case in order to avoid false positives. In contrast, false negatives are not detected (low LR values) so they don't allow the re-analysis. With this in mind, calculations of true and false negatives rates with different numbers of LR thresholds (from 1 to 10000) for each family group could considered in order to select an appropiate LR threshold. This value could be used for the establishment of a LR threshold (named LR decision treshold or LRdt) with the aim of selecting those matches that need a re-evaluation with more genetic and non genetic information in order to minimize the probability of obtaining a false negative result. The LR decision threshold could be selected taking into consideration:<br/><br/>
               <b>    - False negative rate minimization <br/><br/>
               <b>    - Database size and resources for the re-analysis of the expected number of false positive (false positive rate * database size) <br/><br/>
               <b>    - The behavior of the LRdt plot. It indicates that, in some cases, increasing the LRdt value results in a high reduction of false positive rate whilst the false negative rate has a small increase.<br/><h5>"),
               plotlyOutput("LRdet"),
               h4("X axis: False Negatives Rates"),
               h4("Y axis: False Positives Rates"))
      )
    )
  )

server <- function(input, output) { 
  ##### Uploading the file 
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.delim(inFile$datapath, col.names = c("X1-Unrelated", "X2-Related"), header = FALSE, skip = 2, sep = "\t")
  })
  
  ##### Obtaining boxplot 
  output$boxplot1 <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ",", 
                                                           grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
  
    
    data2 <- log10(data1)
    attach(data2)

    A <- gather(data2)
    
    colnames(A) <- c("type","LR")
    attach(A)
    
    #boxplot(log10(data2), horizontal = TRUE)
    # Violin basic
    A %>%
      ggplot(aes(x= A$type, y = A$LR)) +
      geom_violin() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
      theme(
        legend.position="none",
        plot.title = element_text(size=13)
      ) +
      ggtitle("Violin chart of LR distribution") +
      ylab("Log10(LR)") +
      xlab("") +
      coord_flip()
  })
  
  ##### Obtaining LR distribution graph
  output$LRdistributions <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data1 <- read_delim(inFile$datapath, "\t", 
                        escape_backslash = TRUE, escape_double = FALSE, 
                        col_names = FALSE, locale = locale(decimal_mark = ".", 
                        grouping_mark = ""), trim_ws = TRUE, 
                        skip = 2)
    
    attach(data1)
    
    plot(density(log10(X2)), 
         xlab= "log10 LR", 
         ylab= "Density", 
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
         ylab= "Density", 
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
         cex=2.5,
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
    
      FP <- sum(X1 > as.numeric(input$num))/length(X1)
      FN <- sum(X2 < as.numeric(input$num))/length(X1)
      TP <- sum(X2 > as.numeric(input$num))/length(X1)
      TN <- sum(X1 < as.numeric(input$num))/length(X1)
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

