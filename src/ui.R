#Condicion que istala los paquetes si no se encuentran instalados 
#OJO si no se abre por que esta intalando paquetes puede necesitar que le de al Run App despues de instalarlos 
list.of.packages <- c("ggplot2", "shiny","reshape2","datasets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(datasets)
library(reshape2)
library(ggplot2)


ui <- shinyUI(fluidPage(
  titlePanel("Trabajo Final Lector De Ficheros CSV"),
  tabsetPanel(
    tabPanel("Datos",
             titlePanel("Cargar Archivo CSV"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Escoger Archivo',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 dateInput("Calendario", "Date:"),
                 tags$br(),
                 
                 checkboxInput('header', 'Cabecera', TRUE),
                 checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                 
# Esta ya de forma predefinida como para leer nuestro fichero (sep = ";" ,dec = "," y quote = comillas dobles )

                 radioButtons('sep', 'Separador',
                              c("Coma"=',',
                                "Punto y Coma"=';',
                                "Tabulacion"='\t'),
                              ';'),
                 radioButtons("quote", 'Comillas',
                              c(Nada='',
                                "Comillas Dobles" ='"',
                                "Comillas Simples" = "'"),
                              '"'),
                 radioButtons("dec", 'Decimales',
                              c("Entero"='',
                                "Coma" =',',
                                "Punto" = "."),
                              ",")
                 
               ),
               mainPanel(
                 h6("Datos"),
                 tableOutput("filedf"),
                 h6("Tabla"),
                 tableOutput('contents')
              
               )
               
             )
    ),
    
    tabPanel("Head y Tail", h6("Head"),tableOutput("head"),h6("Tail"),tableOutput("tail")),
    tabPanel("Resumen", tableOutput("summary")),
    tabPanel("Suma/Variacion", 
             pageWithSidebar(
               headerPanel('Selector De Datos'),
               sidebarPanel(
                 fluidRow(
                   column(8,checkboxGroupInput("yaxisGrpSum","Y-axis:", c("1"="1","2"="2")))
                    
                 )
                
               ),
               mainPanel(
                 h5("Suma"),
                 tableOutput("sum"),
                 h5("Variacion"),
                 tableOutput("var")
                 
                 )
               )
             ),
    tabPanel("SD",
             pageWithSidebar(
               headerPanel('SD y Quantile'),
               sidebarPanel(
                 fluidRow(
                 selectInput('xcolSd', "Selector de datos", "",selected = "" ),
                 column(12,sliderInput("slider1", "Selector de quantile", min = 0.00, 
                                      max = 1, value = 0.50)
                 )
                 )
               ),
               mainPanel(
                 h6("sd"),
                 tableOutput('sd'),
                 h6("Quantile"),
                 tableOutput('quantile'),
                 h6("rnorm"),
                 tableOutput('rnorm')
               )
               
             )
    ),
    
    tabPanel("Histograma",
             pageWithSidebar(
               headerPanel('Selector De Datos'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcolHist', 'X Variable', "",selected = "")
                 
               ),
               mainPanel(
                 plotOutput('Hist')
               )
             
             )
          ),
    tabPanel("Boxplot",
             pageWithSidebar(
               headerPanel('Selector De Datos'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcolBox', 'X Variable', "",selected = ""),
                 selectInput('ycolBox', 'Y Variable', "",selected = "")
                 
               ),
               mainPanel(
                 plotOutput('BoxPlot')
               )
               
             )
    ),
    
      tabPanel("Plot",
             pageWithSidebar(
               headerPanel('Selector De Datos'),
               sidebarPanel(
                 
                 fluidRow(
                   column(8,radioButtons("xaxisGrp","X-Axis:", c("1"="1","2"="2"))),
                   column(8,checkboxGroupInput("yaxisGrp","Y-axis:", c("1"="1","2"="2")))
                 )
                 
               ),
               mainPanel(
                 plotOutput("Plot"),
                 plotOutput("AreaPlot")
            )
          )
        )
      )
    )
  )
