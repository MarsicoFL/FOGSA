## app.R ##

fogsaGUI <- function() {

ui <- shinydashboard::dashboardPage(skin = "purple",
  shinydashboard::dashboardHeader(title = "FOGSA"),
  shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
                  #menuItem("Description", tabName = "Description", icon = icon("")),
                  shinydashboard::menuItem("Inputs", tabName = "Inputs", icon = shiny::icon("arrow-alt-circle-up")),
                  #menuItem("Pedigri", tabName = "Pedigri", icon = icon("dna")),
                  shinydashboard::menuItem("Descriptive statistics", tabName = "Descriptive statistics", icon = shiny::icon("chart-line"),
                                           shinydashboard::menuSubItem("Summary", tabName = "Summary"),
                                           shinydashboard::menuSubItem("LRdistributions", tabName = "LRdistributions"),
                                           shinydashboard::menuSubItem("PowerPlot", tabName = "PowerPlot"),
                                           shinydashboard::menuSubItem("Rates", tabName = "Rates")
                  ),
                  shinydashboard::menuItem("LRdt", tabName = "LRdt", icon = shiny::icon("eye")))

  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # First tab content
      # tabItem(tabName = "Description",
      # second tab content
      shinydashboard::tabItem(tabName = "Inputs",
                shiny::h2("FOrensic Genetics Simulation Analysis", align= "Center"),
                shiny::HTML("<h5>Purpose: Despite that the use of DNA in Missing Person Identification and Disaster Victim Identification cases has gained increasing focus during the last two decades, there still are several complications that the forensic scientists are faced with, for instance complex family structures and large scale comparisons are examples. Evaluating the impact of these complications in the identification process is one of the main tasks of the forensic genetics statistics. The development of algorithms and the application of statistical analysis in order to measure the uncertainty and minimize the probability of both false positive and false negatives types of error can be crucial to the purpose of the Missing Person Identification. FOGSA (Forensic Genetics Simulation Analysis) provides an userfriendly interface that allows the analysis of the simulation Raw Data from different softwares such as Familias R version and Forrel Package. FOGSA aimed at helping in the statistical analysis for decision making in forensic genetic caseworks.<h5>"),
                shiny::h3("Start uploading your simulation Raw Data"),
                shiny::fluidRow(
                  shiny::fileInput("file1", "Familias simulation output",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  ),
                  shiny::tags$hr(),
                  )),
       # third tab content
      shinydashboard::tabItem(tabName = "LRdistributions",
                     shiny::mainPanel(
                       shiny::plotOutput(outputId = "LRdistributions")),
                 shiny::mainPanel(
                   shiny::plotOutput(outputId = "LRdistributions2"))),

       # four tab content
      shinydashboard::tabItem(tabName = "Summary",
                     shiny::sidebarPanel(
                    shiny::h4("Obtained LR"),
                 shiny::tableOutput("contents")),
                 shiny::mainPanel(
                   shiny::h2("Descriptive statistics"),
                   shiny::h3("Unrelated"),
                   shiny::verbatimTextOutput("SumX1"),
                   shiny::h3("Related"),
                   shiny::verbatimTextOutput("SumX2"),
                   shiny::plotOutput("boxplot1")
               )),
       # five tab content
      shinydashboard::tabItem(tabName = "PowerPlot",
                     shiny::h2("Statistical Power Evaluation"),
                     shiny::HTML("<h5>For a given pedigree the statistical power of the DNA-based identification could be calculated from the simulated data. Two measures could help in order to characterize a family group: Inclution Power (IP): It is the probability of obtaining a likelihood ratio of zero when the tested person (POI) is not related with the family group. Exclution Power (EP): The probability of obtaining a likelihood ratio upper than 10000 when the tested person (POI) is actually the missing person (MP). The Power Plot could be perfomed combining this two measures. A family group with low PI and PE has low statistical power where a high probability of false positives and false negatives could be expected. In this cases more genetic information must be incorporated to the pedigree. It could be done by the incorporation of more genetic markers or more members. As counterpart, a pedigree with high IP and EP allows performing an identification with an accurate result.<br/><br/><br/><h5>"),
                     shiny::plotOutput(outputId = "PowerPlot")),
       # six tab content
      shinydashboard::tabItem(tabName = "Rates",
                              shiny::h3("Rates calculation in function of LR threshold selected", align = "Center"),
                              shiny::HTML("<h5> False positive and false negative rates could be estimated for each Likelihood ratio selected as threshold (please select one using the sliderbar). Taking into account this measures, Matthews Correlation Coefficient and Acuracy can be calculated. Both measures gives an idea about how accurate is the LR threshold selected. If you are working with massive comparisons in genetic databases the expected number of false positives could be approximated multiplying the false positive rate by the database size.<br/><br/><h5>"),
                              shiny::tableOutput("Rates"),
               # Copy the line below to make a number input box into the UI.
               shiny::numericInput("num", label = shiny::h4("Please select the Likelihood Ratio Threshold:"), value = 1),
               shiny::hr(),
               shiny::fluidRow(shiny::column(3, shiny::verbatimTextOutput("LR threshold value")))),
       # seven tab content
      shinydashboard::tabItem(tabName = "LRdt",
                              shiny::h3("Likelihood Ratio Decision Threshold Plot", align= "Center"),
                              shiny::HTML("<h5> <b>Some remarks:</b> In massive comparisons carried out in Missing Person Identification cases, matches (LR values upper the LR threshold) are re-analyzed by geneticists and more genetic (Y-Chromosome, X-Chromosome and mitochondrial DNA) and non-genetic information (date of birth) is incorporated to the case in order to avoid false positives. In contrast, false negatives are not detected (low LR values) so they don't allow the re-analysis. With this in mind, calculations of true and false negatives rates with different numbers of LR thresholds (from 1 to 10000) for each family group could considered in order to select an appropiate LR threshold. This value could be used for the establishment of a LR threshold (named LR decision treshold or LRdt) with the aim of selecting those matches that need a re-evaluation with more genetic and non genetic information in order to minimize the probability of obtaining a false negative result. The LR decision threshold could be selected taking into consideration:<br/><br/>
               <b>    - False negative rate minimization <br/><br/>
               <b>    - Database size and resources for the re-analysis of the expected number of false positive (false positive rate * database size) <br/><br/>
               <b>    - The behavior of the LRdt plot. It indicates that, in some cases, increasing the LRdt value results in a high reduction of false positive rate whilst the false negative rate has a small increase.<br/><h5>"),
                              plotly::plotlyOutput("LRdet"),
                              shiny::h4("X axis: False Negatives Rates"),
                              shiny::h4("Y axis: False Positives Rates"))
      )
    )
  )

server <- function(input, output) {
  ##### Uploading the file
  output$contents <- shiny::renderTable({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    utils::read.delim(inFile$datapath, col.names = c("X1-Unrelated", "X2-Related"), header = FALSE, skip = 2, sep = "\t")
  })

  ##### Obtaining boxplot
  output$boxplot1 <- shiny::renderPlot({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                                                           grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

    data2 <- log10(data1)
    data2

    A <- tidyr::gather(data2)

    colnames(A) <- c("type","LR")

    #boxplot(log10(data2), horizontal = TRUE)
    # Violin basic

      ggplot2::ggplot(A, ggplot2::aes(x= A$type, y = A$LR)) +
      ggplot2::geom_violin() +
      viridis::scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
      ggplot2::theme(
        legend.position="none",
        plot.title = ggplot2::element_text(size=13)
      ) +
      ggplot2::ggtitle("Violin chart of LR distribution") +
      ggplot2::ylab("Log10(LR)") +
      ggplot2::xlab("") +
      ggplot2::coord_flip()
  })

  ##### Obtaining LR distribution graph
  output$LRdistributions <- shiny::renderPlot({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                        grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

    plot(stats::density(log10(data1$X2)),
         xlab= "log10 LR",
         ylab= "Density",
         col=1,
         lwd=2,
         main="LR distribution - Related"
         )
  })

  output$LRdistributions2 <- shiny::renderPlot({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                                                           grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

    data1

    plot(stats::density(log10(data1$X1)),
         xlab= "log10 LR",
         ylab= "Density",
         col=2,
         lwd=2,
         main="LR distribution - Unrelated"
    )
  })


  ##### Power Plot analysis
  output$PowerPlot <- shiny::renderPlot({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                        grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)


    PI <- (sum(data1$X2>10000))/length(data1$X2)
    PE <- (sum(data1$X1 == 0))/length(data1$X1)


    plot(PI, PE,
         type = "p",
         xlab= "Inclusion Power",
         ylab= "Exclusion Power",
         col=2,
         pch = 19,
         lwd=2,
         cex=2.5,
         main="Power Plot",
         xlim = c(0,1),
         ylim = c(0,1)
         )
  })

  output$SumX1 <- shiny::renderPrint({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                                                           grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

    summary(data1$X1)
    })

  output$SumX2 <- shiny::renderPrint({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                                                           grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

    summary(data1$X2)
  })

  sliderValues <- shiny::reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                                                           grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

      FP <- sum(data1$X1 > as.numeric(input$num))/length(data1$X1)
      FN <- sum(data1$X2 < as.numeric(input$num))/length(data1$X1)
      TP <- sum(data1$X2 > as.numeric(input$num))/length(data1$X1)
      TN <- sum(data1$X1 < as.numeric(input$num))/length(data1$X1)
      Accuracy <- (TP+TN)/(TP+TN+FP+FN)
      MCC<-(TP*TN-FP*FN)/(sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN))

      data.frame(
        Parameter = c("False Positive Rate",
                      "False Negative Rate",
                      "True Positive Rate",
                      "True Negative Rate",
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

  output$Rates <- shiny::renderTable({
    sliderValues()
  })

  output$LRdet <- plotly::renderPlotly({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    data1 <- readr::read_delim(inFile$datapath, "\t",
                        escape_backslash = TRUE, escape_double = FALSE,
                        col_names = FALSE, locale = readr::locale(decimal_mark = ".",
                                                           grouping_mark = ""), trim_ws = TRUE,
                        skip = 2)

    LRvalues = seq(1, 10000, length.out=10000)
    FPs = 0
    FNs = 0
    for(i in 1:10000) { FPs[i] = sum(data1$X1 > LRvalues[i]); #False Positives.
                        FNs[i] = sum(data1$X2 < LRvalues[i])} #False negatives.

    Datas = data.frame(x = LRvalues, y= FPs/length(data1$X1), z= FNs/length(data1$X2), w=FPs)

    p <- plotly::plot_ly(
      Datas, x = Datas$z, y = Datas$y,
      text = ~paste("LR: ", Datas$x),
      color = Datas$z, size = 1
    )


  })
}

shiny::shinyApp(ui, server)

}
