server <- shinyServer(function(input, output, session) {

  #Leemos y guardamos el fichero en el formato pedido(.csv)
  data <- reactive({ 
    req(input$file1) 
    
    inFile <- input$file1 
    
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote, dec = input$dec,stringsAsFactors = input$stringAsFactors)
    
    #Actualizacion de los botones y demas tipos de selectores (recogen los datos seleccionados)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df))
    
    updateSelectInput(session, inputId = 'xcolHist', label = 'X Variable',
                      choices = names(df), selected = names(df))
    
    updateSelectInput(session, inputId = 'xcolSd', label = 'X Variable',
                      choices = names(df), selected = names(df))
    
    updateSelectInput(session, inputId = 'xcolBox', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycolBox', label = 'Y Variable',
                      choices = names(df), selected = names(df))
    
    return(df)
    
  })
  
  #METODOS
  
  # Fecha
  output$odate<- renderPrint({input$date})
  
  #Datos del fichero(peso,nombre,directorio)
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file1
  })
  #Datos (tabla)
  output$contents <- renderTable({
    data()
  })
  #Resumen de la tabla muestra el min/max los cuartiles, la media, mediana y ademas el numero de Na que tiene cada columna
  output$summary<-renderTable({
    if(is.null(data())){return ()}
    summary(data())
  })
  #Parte superior de la tabla (metodo head)
  output$head<-renderTable({
    if(is.null(data())){return ()}
    head(data())
  })
  #Parte inferior de la tabla (metodo tail)
  output$tail<-renderTable({
    if(is.null(data())){return ()}
    tail(data())
  })
  #Suma las columnas de la tabla 
  output$sum<-renderTable({
    if(is.null(data())){return ()}
    sum(x<- data()[, input$yaxisGrpSum]) # <- escogida por un selector con multiples posibilidades
  })
  #variacion de las columnas de la tabla 
  output$var<-renderTable({
    if(is.null(data())){return ()}
    var(y<- data()[, input$yaxisGrpSum]) # <- escogida por un selector con multiples posibilidades
  })
  #desvicion estandar de las columnas de la tabla 
  output$sd<-renderTable({
    if(is.null(data())){return ()}
    sd(x<- data()[, input$xcolSd]) # <- escogida por un selector con multiples posibilidades
  })
  #rnorm
  output$rnorm<-renderTable({
    if(is.null(data())){return ()}
    rnorm(x<- data()[, input$xcolSd]) # <- escogida por un selector con multiples posibilidades
  })
  #quantiles de una columna de la tabla escogida por un selector 
  output$quantile<-renderTable({
    if(is.null(data())){return ()}
    output$slider1 <- renderPrint({ input$slider1 }) # <- barra para seleccionar el porcentaje del quantile de 0.00 a 1.00
    quantile(x<- data()[, input$xcolSd],input$slider1) 
  })
  
  #Plot de tipo boxplot de dos columnas escogidas por un selector
  output$BoxPlot <- renderPlot({
    
    x <- data()[, input$xcolBox]
    y <- data()[, input$ycolBox]
    boxplot(x~y)
    grid()
    
  })
  
  #manera alternativa de usar el boxplot mediante el ggplot2 
#  output$boxPlot2 = renderPlot(
#    {
#      df <- data()
#      gp <- NULL
#      if (!is.null(df)){
#        xv <- input$xaxisGrp
#        yv <- input$yaxisGrp
#        if (!is.null(xv) & !is.null(yv)){
#          if (sum(xv %in% names(df))>0){ # supress error when changing files
#            mdf <- melt(df,id.vars=xv,measure.vars=yv)
#            gp <- ggplot(data=mdf) + 
#              geom_boxplot(aes_string(x=xv,y="value",color="variable"))
#          }
#        }
#      }
#      return(gp)
#    }
#  )
  
  #Plot de tipo Histogrma coge 1 columnas y muestra en pantalla la frecuencia con la que se repiten los elementos
  output$Hist <- renderPlot({
    
    x    <- data()[, c(input$xcolHist)]
    bins <- nrow(data())
    hist(x, breaks = bins, col = 'blue', border = 'white')
  })
  
  #Plot en el que se mostrara cualquiera de las columnas del eje y por una del eje x en forma de puntos
  output$Plot = renderPlot(
    {
      df <- data()
      gp <- NULL
      if (!is.null(df)){
        xv <- input$xaxisGrp
        yv <- input$yaxisGrp
        if (!is.null(xv) & !is.null(yv)){
          if (sum(xv %in% names(df))>0){ # supress error when changing files
            mdf <- melt(df,id.vars=xv,measure.vars=yv)
            gp <- ggplot(data=mdf) + 
            geom_point(aes_string(x=xv,y="value",color="variable"))
            
          }
        }
      }
      return(gp)
    }
  )
  
  #Plot en el que se mostrara cualquiera de las columnas del eje y por una del eje x en forma de areas
  output$AreaPlot = renderPlot(
    {
      df <- data()
      gp <- NULL
      if (!is.null(df)){
        xv <- input$xaxisGrp
        yv <- input$yaxisGrp
        if (!is.null(xv) & !is.null(yv)){
          if (sum(xv %in% names(df))>0){ # supress error when changing files
            mdf <- melt(df,id.vars=xv,measure.vars=yv)
            gp <- ggplot(data=mdf) + 
             geom_area(aes_string(x=xv,y="value",color="variable"))
            
            
          }
        }
      }
      return(gp)
    }
  )
  observe({
    dsnames <- names(data())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    
    updateRadioButtons(session, "xaxisGrp",
                       label = "Eje X",
                       choices = cb_options,
                       selected = "")
    
    updateCheckboxGroupInput(session, "yaxisGrp",
                             label = "Eje Y",
                             choices = cb_options,
                             selected = "")
    
    updateCheckboxGroupInput(session, "yaxisGrpSum",
                             label = "Eje Y",
                             choices = cb_options,
                             selected = "")
  })
})