library(ggplot2)
library(shiny)
library(DT)
library(plotly)
library(crosstalk)
m <- read.delim2("/home/franco/Documentos/Datos/Apps/FOSApp/levantado.csv", header=TRUE)
tibble::rownames_to_column(m)
                  
# COMANDO PARA ELIMINAR COLUMNAS REPETIDAS: 
DC_simulaciones = DC_simulaciones[,!duplicated(names(DC_simulaciones))]
####################################User Interface###################################################      
ui <- fluidPage(
                #Titulo 
                h1("Teoría de la decisión", align = "center"),
                 #Elegir el file input
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
                    tags$hr(),
                    checkboxInput("header", "Header", TRUE),
                    
                  #Gráfico de PE vs PI
                  plotlyOutput("x2"),
                  #Estadistica proveniente de las simulaciones
                  varSelectInput("TPED", "True pedigree:", DC_simulaciones, multiple = TRUE),  
                  varSelectInput("RPED", "Reference pedigree:", DC_simulaciones, multiple = TRUE),
                  tableOutput("data"),
                    
                    
                  sliderInput("LR", "LR thershold:",
                              min = 1, max = 10000,
                              value = 0.1, step = 1,
                              animate =
                                animationOptions(interval = 1000, loop = TRUE)),
                    
                    
                  mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("histograma", plotOutput("histograma")),
                                 tabPanel("distribucion", plotOutput("distribucion")),
                                 tabPanel("EstadisticaTP", verbatimTextOutput("EstadisticaTP")),
                                 tabPanel("EstadisticaRP", verbatimTextOutput("EstadisticaRP")),
                                 tabPanel("ValoresTP", tableOutput("ValoresTP")),
                                 tabPanel("ValoresRP", tableOutput("ValoresRP")),
                                 tabPanel("Simulación", tableOutput("Simulación"))
                    )),
                  DT::dataTableOutput("x1"),
                    fluidRow(
                      p(class = 'text-center', downloadButton('x3', 'Descargar seleccionados'))
                    )
                  )
                 
###########################################Server#################################################
              
server <- function(input, output) {
               
          d <- SharedData$new(m, ~Family.id)
                   
          # highlight selected rows in the scatterplot
          output$x2 <- renderPlotly({
                   
           s <- input$x1_rows_selected
                  
           if (!length(s)) {
                p <- d %>%
                plot_ly(x = ~PI, y = ~PE, mode = "markers", color = I('black'), name = 'No seleccionados') %>%
                layout(showlegend = T) %>% 
                highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = "Seleccionados"))} 
           else if (length(s)) {
                        pp <- m %>%
                          plot_ly() %>% 
                          add_trace(x = ~PI, y = ~PE, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
                          layout(showlegend = T)
                  
  # selected data
                        pp <- add_trace(pp, data = m[s, , drop = F], x = ~PI, y = ~PE, mode = "markers",
                                        color = I('red'), name = 'Family.id')
                      }
                  
                    })
                    
  #  Hacer la estadistica básica
                output$EstadisticaTP <- renderPrint({
                summary(DC_simulaciones %>% dplyr::select(!!!input$TPED))
                    })
                output$EstadisticaRP <- renderPrint({
                summary(DC_simulaciones %>% dplyr::select(!!!input$RPED))
                    })
                    
  #  Histograma
                output$histograma <- renderPlot({
                hist(log10(as.numeric(unlist(DC_simulaciones %>% dplyr::select(!!!input$TPED)))), xlab = "logLR", ylab = "count", main ="logLR (lnLR)")  
                    }) 
                    
                    
  #  Mostrar los valores obtenidos
                output$ValoresTP <-renderTable({
                DC_simulaciones %>% dplyr::select(!!!input$TPED)
                }, rownames = TRUE)
                    
                output$ValoresRP <-renderTable({
                DC_simulaciones %>% dplyr::select(!!!input$RPED)
                }, rownames = TRUE)
                    
 #   Plotear distribución de los seleccionados
                output$distribucion <- renderPlot ({
                plot(log10(as.numeric(unlist(DC_simulaciones %>% dplyr::select(!!!input$RPED)))), xlab= "log10 LR", ylab= "Densidad", col=1, lwd=2, main="", xlim=c(-20,20), ylim=c(0,0.65), labels=TRUE)
                points(density(log10(as.numeric(unlist(DC_simulaciones %>% dplyr::select(!!!input$RPED))))), col=2, lty=1, lwd=2, type= "l", labels=TRUE)
                points(density(log10(as.numeric(unlist(DC_simulaciones %>% dplyr::select(!!!input$TPED))))), col=1, lty=1, lwd=2, type= "l", labels=TRUE)
                })
                    
                    
 #   highlight selected rows in the table
                output$x1 <- DT::renderDataTable({
                m2 <- m[d$selection(),]
                dt <- DT::datatable(m)
                  if (NROW(m2) == 0) {
                   dt } 
                  else {
                   DT::formatStyle(dt, "Family.id", target = "row",
                   color = DT::styleEqual(m2$Family.id, rep("white", length(m2$Family.id))),
                   backgroundColor = DT::styleEqual(m2$Family.id, rep("black", length(m2$Family.id))))
                       }
                   })
                  
  # download the filtered data
               output$x3 = downloadHandler('seleccionados.csv', content = function(file) {
                      s <- input$x1_rows_selected
                      if (length(s)) {
                        write.csv(m[s, , drop = FALSE], file)
                      } else if (!length(s)) {
                        write.csv(m[d$selection(),], file)
                      }
                    })
                   
  # Meter archivos desde afuera 
          output$contents <- renderTable({
                      # input$file1 will be NULL initially. After the user selects
                      # and uploads a file, it will be a data frame with 'name',
                      # 'size', 'type', and 'datapath' columns. The 'datapath'
                      # column will contain the local filenames where the data can
                      # be found.
           inFile <- input$file1
                      
           if (is.null(inFile))
                        return(NULL)
                      
            read.csv(inFile$datapath, header = input$header)
            })
                    
# Teoría de la decisión
            sliderValues <- reactive({
            FP <- sum(DC_simulaciones %>% dplyr::select(!!!input$RPED) > as.numeric(input$LR))
            FN <- sum(DC_simulaciones %>% dplyr::select(!!!input$TPED) < as.numeric(input$LR))
            TP <- sum(DC_simulaciones %>% dplyr::select(!!!input$TPED) > as.numeric(input$LR))
            TN <- sum(DC_simulaciones %>% dplyr::select(!!!input$RPED) < as.numeric(input$LR))
            Accuracy <- (TP+TN)/(TP+TN+FP+FN)
            MCC<-(TP*TN-FP*FN)/(sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN))
                      
            data.frame(
                        Parametro = c("Falsos positivos",
                                      "Falsos negativos",
                                      "Verdaderos positivos",
                                      "Verdaderos negativos",
                                      "Accuracy",
                                      "Matthews Coef."
                        ),
                        Valor = as.numeric(c(FP,
                                             FN,
                                             TP,
                                             TN,         
                                             Accuracy ,
                                             MCC
                        )
                        ))})
                    output$Simulación <- renderTable({
                      sliderValues()
                    })
                  }
                  
shinyApp(ui, server)
