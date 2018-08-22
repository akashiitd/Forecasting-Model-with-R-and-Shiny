shinyServer(function(input, output, session) {
  #### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      
######################### UI code for login page #########################
      
      fluidPage(
        tags$style(HTML("body{ 
                        background-image: url('Gen.png');
                        background-repeat: no-repeat;
                        background-size:cover,auto}")),
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
        )
  } else {
    
######################### UI code for Volume Predictor App #########################
    
    library(xts)
    library(ggplot2)
    library(reshape2)
    library(dplyr)
    library(devtools)
    library(forecast)
    library(forecastHybrid)
    library(plotly)
    library(dygraphs)
    library(shinythemes)
    
    
    #set theme for ggplot2 graphics
    theme = theme_set(theme_minimal())
    theme = theme_update(legend.position="top",
                         axis.text.x = element_text(angle = 45, hjust = 1, vjust =1.25))
    
    # navbar for multiple tabs
    navbarPage("Forecasting Tool",theme = shinytheme("cerulean"),
               tabPanel("ECRS",icon = icon("bar-chart"), id = "pm", # tabpanel for ECRS
                        conditionalPanel(condition= "id == pm"),
                        titlePanel("ECRS Volume Predictor"),
                        fluidRow(
                          column(width=2,
                                 fileInput("File1", "Choose a CSV files", accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
                                 hr(),
                                 selectInput(inputId="i_task_select1", "Select ECRS",'',''),
                                 sliderInput(inputId="i_forecast_n1","Forecast for selected ECRS",value=3,min=2,max=12,step=1),
                                 numericInput(inputId="Num_Res1", "Num of Resources", value = 3),
                                 numericInput(inputId="capacity1", "Capacity per Resource", value = 30),
                                 actionButton(inputId="goButton1", strong("Start Forecasting!")),
                                 br(),
                                 br(),
                                 downloadButton('downloader1', 'Download Forecasts',width = 3)
                          ),
                          
                          column(width=2,offset=0,
                                 dateInput(inputId = "i_Date1","Ad-Hoc Period",startview = "year")),
                          
                          column(width=2,offset=0,
                                 numericInput(inputId="i_market1", "Ad-Hoc Volume", value = 3)),
                          
                          column(width=2,offset=0,
                                 dateInput(inputId = "i_Date2","Backlog Period",startview = "year")),
                          
                          column(width=2,offset = 0,
                                 numericInput(inputId="i_back1", "Backlog", value = 3)),
                          
                          column(width=10,
                                 tabsetPanel(type="tabs",
                                             tabPanel("LOBF", icon = icon("bar-chart"), h4("Line of Best Fit (Linear Regression)"), br(), dygraphOutput("p_LOBF"), value=1),
                                             tabPanel("ARIMA",icon = icon("line-chart"),h4("Forecast of Selected Series"),br(),dygraphOutput("p_MF",height=400),value=2),
                                             tabPanel("SES",icon = icon("line-chart"), h4("Simple Exponential Smoothing Model"), br(), dygraphOutput("p_SES"), value=3),
                                             tabPanel("MA", icon = icon("line-chart"), h4("Moving Average Smoothing Model"), br(), dygraphOutput("p_MA"), value=4),
                                             tabPanel("HW", icon = icon("line-chart"), h4("Holt-Winters Exponential Smoothing Model"), br(), dygraphOutput("p_HW"), value=5),
                                             tabPanel("COMBINE", icon = icon("sliders"), h4("Hybrid Model with Weighted Forecasts based on Performance (ARIMA, Exponential, NeuralNets)"), br(), dygraphOutput("p_COMBINE"), value=6),
                                             tabPanel("COMPARE", icon = icon("puzzle-piece"), h4("Compare Forecast Model Performance Metrics"), h6("Read more about ", a("forecasting accuracy here", href="https://en.wikipedia.org/wiki/Forecasting#Forecasting_accuracy", target="_blank")), br(), DT::dataTableOutput("t_COMPARE"), value=7, id="COMP_TAB"),
                                             
                                             id = "timeSeriesTabs1"
                                )
                                )),
                        
                        fluidRow(column(width=12,
                                        h4("Forecast Values With Upper and Lower 95% Confidence Interval"),
                                        DT::dataTableOutput("time_series_table1")),
                                 column(width=12,
                                        h4("Performance Metrics"),
                                        DT::dataTableOutput("performance_metrics"))
               )),
               
               
               tabPanel("GPO",icon = icon("bar-chart"),id="sm", #tabpanel for GPO
                        conditionalPanel(condition = "id==sm"),
                        titlePanel("GPO Volume Predictor"),
                        fluidRow(
                          column(width=2,
                                 fileInput("File2","Choose a CSV files",accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
                                 hr(),
                                 selectInput(inputId="i_task_select2", "Select GPO",'',''),
                                 sliderInput(inputId="i_forecast_n2","Forecast for selected GPO",value=3,min=2,max=12,step=1),
                                 numericInput(inputId="Num_Res2", "Num of Resources", value = 3),
                                 numericInput(inputId="capacity2", "Capacity per Resource", value = 30),
                                 actionButton(inputId="goButton2", strong("Start Forecasting!")),
                                 br(),
                                 br(),
                                 downloadButton('downloader2', 'Download Forecasts',width = 3)
                          ),
                          
                          column(width=2,offset=0,
                                 dateInput(inputId = "i_Date3","Ad-Hoc Period",startview = "year")),
                          
                          column(width=2,offset=0,
                                 numericInput(inputId="i_market2", "Ad Hoc Volume", value = 3)),
                          
                          column(width=2,offset=0,
                                 dateInput(inputId = "i_Date4","Backlog Period",startview = "year")),
                          
                          column(width=2,offset = 0,
                                 numericInput(inputId="i_back2", "Backlog", value = 3)),
                          
                          column(width=10,
                                 tabsetPanel(type="tabs",
                                             tabPanel("LOBF", icon = icon("bar-chart"), h4("Line of Best Fit (Linear Regression)"), br(), dygraphOutput("m_LOBF"), value=1),
                                             tabPanel("ARIMA",icon = icon("line-chart"),h4("Forecast of Selected Series"),br(),dygraphOutput("m_MF",height=400),value=2),
                                             tabPanel("SES",icon = icon("line-chart"), h4("Simple Exponential Smoothing Model"), br(), dygraphOutput("m_SES"), value=3),
                                             tabPanel("MA", icon = icon("line-chart"), h4("Moving Average Smoothing Model"), br(), dygraphOutput("m_MA"), value=4),
                                             tabPanel("HW", icon = icon("line-chart"), h4("Holt-Winters Exponential Smoothing Model"), br(), dygraphOutput("m_HW"), value=5),
                                             tabPanel("COMBINE", icon = icon("sliders"), h4("Hybrid Model with Weighted Forecasts based on Performance (ARIMA, Exponential)"), br(), dygraphOutput("m_COMBINE"), value=6),
                                             tabPanel("COMPARE", icon = icon("puzzle-piece"), h4("Compare Forecast Model Performance Metrics"), h6("Read more about ", a("forecasting accuracy here", href="https://en.wikipedia.org/wiki/Forecasting#Forecasting_accuracy", target="_blank")), br(), DT::dataTableOutput("s_COMPARE"), value=7, id="COMP_TAB"),
                                             
                                             id = "timeSeriesTabs2"
                                 )
                                 )),
                        
                        fluidRow(column(width=12,
                                        h4("Forecast Values With Upper and Lower 95% Confidence Interval"),
                                        DT::dataTableOutput("time_series_table2")),
                                 column(width=12,
                                        h4("Performance Metrics"),
                                        DT::dataTableOutput("performance_metrics1"))
                        )
                        
               )
               
               
   )
    
    
  }
})
  
######################### Server Code for Volume Prediction ################
  
  ## load packages
  library(zoo)
  library(xts)
  library(ggplot2)
  library(reshape2)
  library(devtools)
  library(forecastHybrid)
  library(forecast)
  library(data.table)
  library(DT)
  library(plotly)
  library(dygraphs)
  library(dplyr)
  library(mondate)
  
  #set theme for ggplot2 graphics
  theme = theme_set(theme_minimal())
  theme = theme_update(legend.position="top",
                       axis.text.x = element_text(angle = 45, hjust = 1, vjust =1.25))
  
  #read in reference CSV for public holidays - dummy variables
  pubhol <- read.csv('PublicHoliday.csv',
                     header=T,
                     sep=",",
                     quote='"',
                     strip.white=T,
                     stringsAsFactors=F,
                     fill=T)
  
  pubhol$Date <- zoo::as.Date(pubhol$Date, format = "%m/%d/%Y")
  pubhol$weekday <- weekdays(pubhol$Date)
  

  
################################ ECRS Calculation ################################
  
    
  #Read based on file selection
  mySeries_raw <- reactive({
    
    inFile <- input$File1
    
    if (is.null(inFile))
      return(NULL)
    
    mySeries <- read.csv(inFile$datapath, 
                              header = T,
                              strip.white=T,
                              stringsAsFactors=F,
                              fill=T)
    
    #convert date format
    mySeries$Date <- zoo::as.Date(mySeries$Date, format = "%m/%d/%Y")
    
    #remove NAs
    mySeries <- mySeries %>% filter(!is.na(Date))
    
    #add 'weekday' variable
    mySeries$weekday <- weekdays(mySeries$Date)
    
    #add public holiday variable for use as exogenous variable
    mySeries$pubhol <- ifelse(mySeries$Date %in% pubhol$Date,1,0)
    
    #assign id field for visualisations
    mySeries$id <- 1:nrow(mySeries)
    
    mySeries <- mySeries
    
  })
  
  
  #Dynamic drop down
  observeEvent(mySeries_raw(), {
    
    mySeries <- mySeries_raw()
    
    updateSelectInput(session, 
                      'i_task_select1', 
                      label = 'Select Series',
                      choices = names(select(mySeries, -Date, -pubhol, -id, -weekday)),
                      names(select(mySeries, -Date, -pubhol, -id, -weekday))[1])
    
  })
  
  
  
  # Reactive Filtered DataFrame
  mySeries_filtered <- eventReactive(
    {input$i_forecast_n1
     input$i_task_select1
     input$goButton1},
    
     {
    
    #dependency on 'start forecasting' button being pressed
    #input$goButton1
    
    
    if (nrow(mySeries_raw())==0) 
      return()
      
      
    #clear out performance comparison table
    performance$performance_compare <- data.frame(
      metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1'))
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    
    
  })
  
  # Reactive Filtered DataFrame
 # mySeries_filtered <- eventReactive(input$i_task_select1, {
   # 
    #dependency on 'start forecasting' button being pressed
   # input$goButton
  #  
    #if (nrow(mySeries_raw())==0) 
     # return()
    #
    #clear out performance comparison table
    #performance$performance_compare <- data.frame(
     # metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1'))
    #
    #use existing reactive structures
    #mySeries <- mySeries_raw()
    
    
  #})
  
  # Reactive Date Input 1
  observeEvent(mySeries_raw(),{
    
    mydate1 <- mySeries_raw()
    observe({
      currdate1 <- max(mydate1$Date)
      datelen1 <- input$i_forecast_n1+1
      maxdate1<-max(seq.Date(currdate1,by="month",length.out = datelen1))
      
      updateDateInput(session,"i_Date1","Ad-Hoc Period",value=currdate1,max = maxdate1)
   })
  })
  
  # Reactive Date Input 2
  observeEvent(mySeries_raw(),{
    
    mydate2 <- mySeries_raw()
    observe({
      currdate2 <- max(mydate2$Date)
      datelen2 <- input$i_forecast_n1+1
      maxdate2<-max(seq.Date(currdate2,by="month",length.out = datelen2))
      
      updateDateInput(session,"i_Date2","Backlog Period",value=currdate2,max = maxdate2)
   })
  })
  
  
  # Object to store performance metrics
  performance <- reactiveValues(
    
    performance_compare = data.frame(
      metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1')
    )
    
  )
  
  # Forecasting and rendering graph
  
  output$p_MF <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_MF_daily <- mySeries_filtered()
    
    if (nrow(mySeries_MF_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    
    task_type <- input$i_task_select1
    forecast_n <- input$i_forecast_n1
    resource_n <- input$Num_Res1
    capacity_n <- input$capacity1
    date_n1 <- input$i_Date1
    mark_n1 <- input$i_market1
    date_n2 <- input$i_Date2
    back_n1 <- input$i_back1
    
    #mean forecast
    myY <-  ts(select_(mySeries_MF_daily, task_type),start=c(2014,1),frequency = 12)  # check for frequency and start information  
    
    TS_mySeries_ARIMA_fit <- auto.arima(myY)
    TS_mySeries_ARIMA_Forecast <-forecast(TS_mySeries_ARIMA_fit,h=forecast_n)
    
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_MF_daily_df <- cbind(as.data.frame(TS_mySeries_ARIMA_fit$fitted)[1:nrow(mySeries_MF_daily),],
                             select_(mySeries_MF_daily, quote(Date)))
    
    colnames(fit_MF_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_MF_daily_df <- with(TS_mySeries_ARIMA_Forecast,
                                 data.frame(mean=TS_mySeries_ARIMA_Forecast$mean,
                                            upper=TS_mySeries_ARIMA_Forecast$upper[,2],
                                            lower=TS_mySeries_ARIMA_Forecast$lower[,2],
                                            back=TS_mySeries_ARIMA_Forecast$upper-(resource_n*capacity_n)))
    
    forecast_MF_daily_df$Date <- seq.Date(max(mySeries_MF_daily$Date+31),
                                          by = "month",length.out = forecast_n)
    
    mySeq <- seq.Date(as.Date(first(select_(forecast_MF_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_MF_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n1] <- forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n1] + mark_n1
    forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n1] <- forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n1] + mark_n1
    forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n1] <- forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n1] + mark_n1
    
    
    # Add backlog to immediate next month for table
    forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n2] <- forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n2] + back_n1
    forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n2] <- forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n2] + back_n1
    forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n2] <- forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n2] + back_n1
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_MF_daily_df$back <- forecast_MF_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_MF_daily_df[forecast_MF_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(TS_mySeries_ARIMA_Forecast,
                                data.frame(backlog = TS_mySeries_ARIMA_Forecast$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(mySeries_MF_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + mark_n1
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n2] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + back_n1
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table1 <- renderDataTable({
      
      forecast_MF_daily_df <- forecast_MF_daily_df%>%
        filter(mean > 0) %>%
        select(Date, mean, upper, lower,back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2)
        )
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaded when model selected
    output$downloader1 = downloadHandler(paste0('MF','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_MF_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_ARIMA_fit)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(MF = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_MF_daily, series = task_type),
               select_(mySeries_MF_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_MF_daily$Date))
    
    myfitted <- xts(select_(fit_MF_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_MF_daily$Date))
    
    myPred <- xts(select_(forecast_MF_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_MF_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred,myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('ARIMA Forecast: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = TRUE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      #dyCrosshair(direction = "vertical")%>%
      #dyLegend(show = "follow")%>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  #######  SIMPLE EXPONENTIAL SMOOTHING DYGRAPH  ####### 
  output$p_SES <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_SES <- mySeries_filtered()
    
    if (nrow(mySeries_SES) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select1
    forecast_n <- input$i_forecast_n1
    resource_n <- input$Num_Res1
    capacity_n <- input$capacity1
    date_n1 <- input$i_Date1
    mark_n1 <- input$i_market1
    date_n2 <- input$i_Date2
    back_n1 <- input$i_back1
    
    #simple exponential smoothing
    myY <-  ts(select_(mySeries_SES, task_type),start=c(2014,1), frequency=12)
    
    TS_mySeries_SES <- ses(myY, h=forecast_n, initial='optimal')
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_SES_daily_df <- cbind(as.data.frame(TS_mySeries_SES$fitted)[1:nrow(mySeries_SES),],
                              select_(mySeries_SES, quote(Date)))
    
    colnames(fit_SES_daily_df) <- c('fitted','Date')
    
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_SES_daily_df <- with(TS_mySeries_SES,
                                  data.frame(mean=TS_mySeries_SES$mean,
                                             upper=TS_mySeries_SES$upper[,2],
                                             lower=TS_mySeries_SES$lower[,2],
                                             back=TS_mySeries_SES$upper-(resource_n*capacity_n)))
    forecast_SES_daily_df$Date <- seq.Date(max(mySeries_SES$Date+31),
                                           by = "month",length.out = forecast_n)
    mySeq <- seq.Date(as.Date(first(select_(forecast_SES_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_SES_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n1] <- forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n1] + mark_n1
    forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n1] <- forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n1] + mark_n1
    forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n1] <- forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n1] + mark_n1
    
    
    # Add backlog to immediate next month for table
    forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n2] <- forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n2] + back_n1
    forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n2] <- forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n2] + back_n1
    forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n2] <- forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n2] + back_n1
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_SES_daily_df$back <- forecast_SES_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_SES_daily_df[forecast_SES_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(TS_mySeries_SES,
                                data.frame(backlog = TS_mySeries_SES$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(mySeries_SES$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + mark_n1
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n2] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + back_n1
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table1 <- renderDataTable({
      
      forecast_SES_daily_df <- forecast_SES_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back, 2)
              )
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader1 = downloadHandler(paste0('SES','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_SES_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_SES)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(SES = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_SES, series = task_type),
               select_(mySeries_SES, quote(Date)),
               order.by=as.POSIXct(mySeries_SES$Date))
    
    myfitted <- xts(select_(fit_SES_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_SES$Date))
    
    myPred <- xts(select_(forecast_SES_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower),
                          quote(Date)),
                  order.by = as.POSIXct(forecast_SES_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    
    myDy <- cbind(myX, myfitted, myPred, myback)
    
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('SIMPLE EXPONENTIAL SMOOTHING of ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  #######  HOLT-WINTERS EXPONENTIAL SMOOTHING DYGRAPH  ####### 
  output$p_HW <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_HW_daily <- mySeries_filtered()
    
    if (nrow(mySeries_HW_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )

    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select1
    forecast_n <- input$i_forecast_n1
    resource_n <- input$Num_Res1
    capacity_n <- input$capacity1
    date_n1 <- input$i_Date1
    mark_n1 <- input$i_market1
    date_n2 <- input$i_Date2
    back_n1 <- input$i_back1
    
    
    #HW exponential smoothing
    myY <-  ts(select_(mySeries_HW_daily, task_type),start = c(2014,1),frequency=12)   
    
    #HW smoothing
   # TS_mySeries_HW_daily <- ets(myY,
    #                            model="ZAN",
     #                           damped=F,
      #                          additive.only=T
       #                         #gamma=myM
    #)
    
    TS_mySeries_HW_daily<- hw(myY, h=forecast_n)
    #TS_mySeries_HW_daily<- HoltWinters(myY)
    
    #create fit object
    fit_HW_daily_df <- cbind(as.data.frame(TS_mySeries_HW_daily$fitted)[1:nrow(mySeries_HW_daily),],
                             select_(mySeries_HW_daily, quote(Date)))
    
    #fit_HW_daily_df<-fit_HW_daily_df[,c(-2,-3)] #when using HoltWInters package
    
    colnames(fit_HW_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_HW_daily <- forecast(TS_mySeries_HW_daily, h=forecast_n)
    
    #forecast_HW_daily <- forecast:::forecast.HoltWinters(TS_mySeries_HW_daily, h=forecast_n) #When USing HoltWInters package
    
    forecast_HW_daily_df <- with(forecast_HW_daily,
                                 data.frame(mean=forecast_HW_daily$mean,
                                            upper=forecast_HW_daily$upper[,2],
                                            lower=forecast_HW_daily$lower[,2],
                                            back=forecast_HW_daily$upper-(resource_n*capacity_n)))
    forecast_HW_daily_df$Date <- seq.Date(max(mySeries_HW_daily$Date+31),
                                          by = "month",length.out = forecast_n)
    
    
    mySeq <- seq.Date(as.Date(first(select_(forecast_HW_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
      
    forecast_HW_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n1] <- forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n1] + mark_n1
    forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n1] <- forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n1] + mark_n1
    forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n1] <- forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n1] + mark_n1
    
    
    # Add backlog to immediate next month for table
    forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n2] <- forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n2] + back_n1
    forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n2] <- forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n2] + back_n1
    forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n2] <- forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n2] + back_n1
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_HW_daily_df$back <- forecast_HW_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_HW_daily_df[forecast_HW_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(forecast_HW_daily,
                                data.frame(backlog = forecast_HW_daily$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(mySeries_HW_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + mark_n1
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n2] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + back_n1
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table1 <- renderDataTable({
      
      forecast_HW_daily_df <- forecast_HW_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
                back = round(back,2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('HW','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_HW_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_HW_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(HW = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_HW_daily, series = task_type),
               select_(mySeries_HW_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_HW_daily$Date))
    
    myfitted <- xts(select_(fit_HW_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_HW_daily$Date))
    
    myPred <- xts(select_(forecast_HW_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_HW_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred, myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('HOLT-WINTERS EXPONENTIAL SMOOTHING of ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color="red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  #######  MOVING AVERAGE DYGRAPH  ####### 
  output$p_MA <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_MA_daily <- mySeries_filtered()
    
    if (nrow(mySeries_MA_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select1
    forecast_n <- input$i_forecast_n1
    resource_n <- input$Num_Res1
    capacity_n <- input$capacity1
    date_n1 <- input$i_Date1
    mark_n1 <- input$i_market1
    date_n2 <- input$i_Date2
    back_n1 <- input$i_back1
    
    myMA = 3
    
    #convert to time series object
    myY <-  ts(select_(mySeries_MA_daily, series = task_type),start = c(2014,1),frequency=12)   
    
    withProgress(message = 'Optimising forecast model... ', 
                 detail = 'this may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   
                   #create moving average forecast object
                   TS_mySeries_MA_daily <- forecast(ma(myY, 
                                                       order = myMA),
                                                    h = forecast_n)
                 })
    
    
    #convert elements of time series MODEL to dataframe for plotting
    #fit_MA_daily_df <- select_(mySeries_MA_daily, task_type, quote(Date))
    
    fit_MA_daily_df <- cbind(as.data.frame(TS_mySeries_MA_daily$fitted)[1:nrow(mySeries_MA_daily),],
                             select_(mySeries_MA_daily, quote(Date)))
    
    colnames(fit_MA_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_MA_daily_df <- with(TS_mySeries_MA_daily,
                                 data.frame(mean=TS_mySeries_MA_daily$mean,
                                            upper=TS_mySeries_MA_daily$upper[,2],
                                            lower=TS_mySeries_MA_daily$lower[,2],
                                            back=TS_mySeries_MA_daily$upper-(resource_n*capacity_n)))
    forecast_MA_daily_df$Date <- seq.Date(max(mySeries_MA_daily$Date+31),
                                          by='month',length.out=forecast_n)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
      mySeq <- seq.Date(as.Date(first(select_(forecast_MA_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
      
      forecast_MA_daily_df$Date <- mySeq
    
      # Add market intelligence to forecast dataframe for table 
      forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n1] <- forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n1] + mark_n1
      forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n1] <- forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n1] + mark_n1
      forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n1] <- forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n1] + mark_n1
      
      
      # Add backlog to immediate next month for table
      forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n2] <- forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n2] + back_n1
      forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n2] <- forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n2] + back_n1
      forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n2] <- forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n2] + back_n1
      
      
      # Calculate the backlog with capacity and resource for table
      forecast_MA_daily_df$back <- forecast_MA_daily_df$upper-(resource_n*capacity_n)
      
      # Check for negative values and mutate them to 0
      forecast_MA_daily_df[forecast_MA_daily_df < 0]<-0
      
      # convert elements of time series forecast to dataframe for backlog calculation and plotting
      forecast_backlog_df <- with(TS_mySeries_MA_daily,
                                  data.frame(backlog = TS_mySeries_MA_daily$upper[,2]))
      
      forecast_backlog_df$Date <- seq.Date(max(mySeries_MA_daily$Date+31),
                                           by = "month",length.out = forecast_n)
      
      forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + mark_n1
      
      forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n2] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + back_n1
      
      forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
      
      forecast_backlog_df[forecast_backlog_df<0] <- 0
      
    
    output$time_series_table1 <- renderDataTable({
      
      forecast_MA_daily_df <- forecast_MA_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('MA','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_MA_daily_df, file)
    })
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_MA_daily, x=mySeries_MA_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(MA = myAccuracy[, 2]))
      })
      
    })
    
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_MA_daily, series = task_type),
               select_(mySeries_MA_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_MA_daily$Date))
    
    myfitted <- xts(select_(fit_MA_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_MA_daily$Date))
    
    myPred <- xts(select_(forecast_MA_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_MA_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by = as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred, myback)
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0(myMA, '-ORDER MOVING AVERAGE of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      #dyRoller(rollPeriod = myMA) %>%     #lets users enter a 'smoothing' parameter interactively
      dyRangeSelector()
    
    print(d)
    
    
  })
  
 
  
  #######  LINE OF BEST FIT LINEAR DYGRAPH  ####### 
  output$p_LOBF <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_LOBF_daily <- mySeries_filtered()
    
    if (nrow(mySeries_LOBF_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select1
    forecast_n <- input$i_forecast_n1
    resource_n <- input$Num_Res1
    capacity_n <- input$capacity1
    date_n1 <- input$i_Date1
    mark_n1 <- input$i_market1
    date_n2 <- input$i_Date2
    back_n1 <- input$i_back1
    
    
    
    #line of best fit
    TS_mySeries_LOBF_daily <- lm(paste0(task_type, "~", quote(Date)), data = mySeries_LOBF_daily)
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_LOBF_daily_df <- cbind(as.vector(TS_mySeries_LOBF_daily$fitted.values),
                               select_(mySeries_LOBF_daily, quote(Date)))
    
    colnames(fit_LOBF_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    myTS <-  ts(fit_LOBF_daily_df$fitted,freq = 12)
    
    TS_mySeries_LOBF_daily <- forecast(myTS, 
                                       h=forecast_n)
    
    forecast_LOBF_daily_df <- with(TS_mySeries_LOBF_daily,
                                   data.frame(mean=TS_mySeries_LOBF_daily$mean,
                                              upper=TS_mySeries_LOBF_daily$upper[,2],
                                              lower=TS_mySeries_LOBF_daily$lower[,2],
                                              back=TS_mySeries_LOBF_daily$upper-(resource_n*capacity_n)))
    
    forecast_LOBF_daily_df$Date <- seq.Date(max(mySeries_LOBF_daily$Date+31),
                                            by="month",length.out=forecast_n)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
      mySeq <- seq.Date(as.Date(first(select_(forecast_LOBF_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
      
      forecast_LOBF_daily_df$Date <- mySeq
      
      # Add market intelligence to forecast dataframe for table 
      forecast_LOBF_daily_df$mean[forecast_LOBF_daily_df$Date==date_n1] <- forecast_LOBF_daily_df$mean[forecast_LOBF_daily_df$Date==date_n1] + mark_n1
      forecast_LOBF_daily_df$upper[forecast_LOBF_daily_df$Date==date_n1] <- forecast_LOBF_daily_df$upper[forecast_LOBF_daily_df$Date==date_n1] + mark_n1
      forecast_LOBF_daily_df$lower[forecast_LOBF_daily_df$Date==date_n1] <- forecast_LOBF_daily_df$lower[forecast_LOBF_daily_df$Date==date_n1] + mark_n1
      
      
      # Add backlog to immediate next month for table
      forecast_LOBF_daily_df$mean[forecast_LOBF_daily_df$Date==date_n2] <- forecast_LOBF_daily_df$mean[forecast_LOBF_daily_df$Date==date_n2] + back_n1
      forecast_LOBF_daily_df$upper[forecast_LOBF_daily_df$Date==date_n2] <- forecast_LOBF_daily_df$upper[forecast_LOBF_daily_df$Date==date_n2] + back_n1
      forecast_LOBF_daily_df$lower[forecast_LOBF_daily_df$Date==date_n2] <- forecast_LOBF_daily_df$lower[forecast_LOBF_daily_df$Date==date_n2] + back_n1
      
      
      # Calculate the backlog with capacity and resource for table
      forecast_LOBF_daily_df$back <- forecast_LOBF_daily_df$upper-(resource_n*capacity_n)
      
      # Check for negative values and mutate them to 0
      forecast_LOBF_daily_df[forecast_LOBF_daily_df < 0]<-0
      
      # convert elements of time series forecast to dataframe for backlog calculation and plotting
      forecast_backlog_df <- with(TS_mySeries_LOBF_daily ,
                                  data.frame(backlog = TS_mySeries_LOBF_daily$upper[,2]))
      
      forecast_backlog_df$Date <- seq.Date(max(mySeries_LOBF_daily$Date+31),
                                           by = "month",length.out = forecast_n)
      
      forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + mark_n1
      
      forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n2] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + back_n1
      
      forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
      
      forecast_backlog_df[forecast_backlog_df<0] <- 0
      
      
    
    
    output$time_series_table1 <- renderDataTable({
      
      forecast_LOBF_daily_df <- forecast_LOBF_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('LOBF','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_LOBF_daily_df, file)
    })
    
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_LOBF_daily, x=mySeries_LOBF_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(LOBF = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_LOBF_daily, series = task_type),
               select_(mySeries_LOBF_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_LOBF_daily$Date))
    
    myfitted <- xts(select_(fit_LOBF_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_LOBF_daily$Date))
    
    myPred <- xts(select_(forecast_LOBF_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_LOBF_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred,myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('LINE OF BEST FIT of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  #######  COMBINED FORECAST DYGRAPH  ####### 
  output$p_COMBINE <- renderDygraph({
    
    #use existing reactive structures
    mySeries <- mySeries_raw()
    mySeries_COMB_daily <- mySeries_filtered()
    
    if (nrow(mySeries_COMB_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select1
    forecast_n <- input$i_forecast_n1
    resource_n <- input$Num_Res1
    capacity_n <- input$capacity1
    date_n1 <- input$i_Date1
    mark_n1 <- input$i_market1
    date_n2 <- input$i_Date2
    back_n1 <- input$i_back1
    
    #create ts object
    myY <-  ts(select_(mySeries_COMB_daily, task_type),start = c(2014,1),frequency = 12)   
    
    
  
                  # create XREG object
                   
                    # myXReg_fit <- fourier(myY, K=2)
                    # myXReg_forecast <- fourier(myY, K = 2, h = forecast_n)
    #myXReg_fit <- cbind(fourier(myY, K = 2),
     #                   select_(mySeries_COMB_daily, quote(pubhol)))
    #myXReg_forecast <- cbind(fourier(myY, K = 3, h = forecast_n), 
     #                        ifelse(seq.Date(as.Date(last(select_(mySeries_COMB_daily, quote(Date)))[[1]]), 
      #                                       as.Date(last(select_(mySeries_COMB_daily, quote(Date)))[[1]]) + forecast_n, by = "month") %in% select_(pubhol, quote(Date)), 1, 0))
    
                   
                   
                   
                   #cast myY as numeric for 'hybridModel' function
                   myY <- as.numeric(myY)
                   
                   #COMBINED Forecast using forecastHybrid package
                   TS_mySeries_COMB_daily <- hybridModel(myY, 
                                                         models = "aen",
                                                         errorMethod = "RMSE",
                                                         weights = "insample.errors"
                                                         )
                   
                 
    
    
    #create fit object
    fit_COMB_daily_df <- cbind(as.data.frame(TS_mySeries_COMB_daily$fitted)[1:nrow(mySeries_COMB_daily),],
                               select_(mySeries_COMB_daily, quote(Date)))
    
    colnames(fit_COMB_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_COMB_daily <- forecast(TS_mySeries_COMB_daily, 
                                    h=forecast_n)
    forecast_COMB_daily_df <- with(forecast_COMB_daily,
                                   data.frame(mean=forecast_COMB_daily$mean,
                                              upper=forecast_COMB_daily$upper[,2],
                                              lower=forecast_COMB_daily$lower[,2],
                                              back=forecast_COMB_daily$upper-(resource_n*capacity_n)))
    
    forecast_COMB_daily_df$Date <- seq.Date(max(mySeries_COMB_daily$Date+31), by = "month",length.out = forecast_n)
                                            
    
    #add validation as to whether forecast should include WEEKENDS or not
      mySeq <- seq.Date(as.Date(first(select_(forecast_COMB_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
      
      forecast_COMB_daily_df$Date <- mySeq
    
      # Add market intelligence to forecast dataframe for table 
      forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n1] <- forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n1] + mark_n1
      forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n1] <- forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n1] + mark_n1
      forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n1] <- forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n1] + mark_n1
      
      
      # Add backlog to immediate next month for table
      forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n2] <- forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n2] + back_n1
      forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n2] <- forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n2] + back_n1
      forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n2] <- forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n2] + back_n1
      
      
      # Calculate the backlog with capacity and resource for table
      forecast_COMB_daily_df$back <- forecast_COMB_daily_df$upper-(resource_n*capacity_n)
      
      # Check for negative values and mutate them to 0
      forecast_COMB_daily_df[forecast_COMB_daily_df < 0]<-0
      
      # convert elements of time series forecast to dataframe for backlog calculation and plotting
      forecast_backlog_df <- with(forecast_COMB_daily ,
                                  data.frame(backlog = forecast_COMB_daily$upper[,2]))
      
      forecast_backlog_df$Date <- seq.Date(max(mySeries_COMB_daily$Date+31),
                                           by = "month",length.out = forecast_n)
      
      forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + mark_n1
      
      forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n2] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n1] + back_n1
      
      forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
      
      forecast_backlog_df[forecast_backlog_df<0] <- 0
      
    
    output$time_series_table1 <- renderDataTable({
      
      forecast_COMB_daily_df <- forecast_COMB_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('COMBINED','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_COMB_daily_df, file)
    })
    
    
    output$performance_metrics <- renderDataTable({
      
      myAccuracy <- accuracy(TS_mySeries_COMB_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare = bind_cols(performance$performance_compare,
                                                    data.frame(COMB = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(mySeries_COMB_daily, series = task_type),
               select_(mySeries_COMB_daily, quote(Date)),
               order.by=as.POSIXct(mySeries_COMB_daily$Date))
    
    myfitted <- xts(select_(fit_COMB_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(mySeries_COMB_daily$Date))
    
    myPred <- xts(select_(forecast_COMB_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_COMB_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    
    myDy <- cbind(myX, myfitted, myPred,myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('COMBINED FORECAST MODEL of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color="red") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
  })
    
###### PERFORMANCE METRIC COMPARISON TABLE ######
  output$t_COMPARE <- renderDataTable({
    
    performance$performance_compare
    
  })


    
    
  ########################### GPO Calculation ################################
  # Read based on file selection
  gpo_mySeries_raw <- reactive({
    
    inFile <- input$File2
    
    if (is.null(inFile))
      return(NULL)
    
    gpo_mySeries <- read.csv(inFile$datapath, 
                             header = T,
                             strip.white=T,
                             stringsAsFactors=F,
                             fill=T)
    
    #convert date format
    gpo_mySeries$Date <- zoo::as.Date(gpo_mySeries$Date, format = "%m/%d/%Y")
    
    #remove NAs
    gpo_mySeries <- gpo_mySeries %>% filter(!is.na(Date))
    
    #add 'weekday' variable
    gpo_mySeries$weekday <- weekdays(gpo_mySeries$Date)
    
    #add public holiday variable for use as exogenous variable
    gpo_mySeries$pubhol <- ifelse(gpo_mySeries$Date %in% pubhol$Date,1,0)
    
    #assign id field for visualisations
    gpo_mySeries$id <- 1:nrow(gpo_mySeries)
    
    gpo_mySeries <- gpo_mySeries
    
  })
  
  
  #Dynamic
  observeEvent(gpo_mySeries_raw(), {
    
    gpo_mySeries <- gpo_mySeries_raw()
    
    updateSelectInput(session, 
                      'i_task_select2', 
                      label = 'Select Series',
                      choices = names(select(gpo_mySeries, -Date, -pubhol, -id, -weekday)),
                      names(select(gpo_mySeries, -Date, -pubhol, -id, -weekday))[1])
    
  })
  
  
  
  ####### REACTIVE FILTERED DATAFRAME ####### 
  gpo_mySeries_filtered <- eventReactive(
    {input$i_forecast_n2
      input$i_task_select2
      input$goButton2}
      
                                         
    , {
    
    #dependency on 'start forecasting' button being pressed
    #input$goButton2
    
    if (nrow(gpo_mySeries_raw())==0) 
      return()
    
    #clear out performance comparison table
    performance$performance_compare1 <- data.frame(
      metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1'))
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    
    
  })
  
  
  ###### OBJECT TO STORE PERFORMANCE METRICS ######
  performance <- reactiveValues(
    
    performance_compare1 = data.frame(
      metric = c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1')
    )
    
  )
  
##################Reactive Date Input 3 ################
  
  observeEvent(gpo_mySeries_raw(),{
    
    mydate3 <- gpo_mySeries_raw()
    observe({
      currdate3 <- max(mydate3$Date)
      datelen3 <- input$i_forecast_n2+1
      maxdate3<-max(seq.Date(currdate3,by="month",length.out = datelen3))
      
      updateDateInput(session,"i_Date3","Ad-Hoc Period",value=currdate3,max = maxdate3)
   })    
  })
  
  ############## Reactive Date Input4 #################
  
  observeEvent(gpo_mySeries_raw(),{
    
    mydate4 <- gpo_mySeries_raw()
    observe({
      currdate4 <- max(mydate4$Date)
      datelen4 <- input$i_forecast_n2+1
      maxdate4<-max(seq.Date(currdate4,by="month",length.out = datelen4))
      
      updateDateInput(session,"i_Date4","Backlog Period",value=currdate4,max = maxdate4)
   })
  })
  
  ####### FORECAST METHOD ###################
  
  output$m_MF <- renderDygraph({
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    gpo_mySeries_MF_daily <- gpo_mySeries_filtered()
    
    if (nrow(gpo_mySeries_MF_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    
    task_type <- input$i_task_select2
    forecast_n <- input$i_forecast_n2
    resource_n <- input$Num_Res2
    capacity_n <- input$capacity2
    date_n3 <- input$i_Date3
    mark_n2 <- input$i_market2
    date_n4 <- input$i_Date4
    back_n2 <- input$i_back2
    
    
    #mean forecast
    myY <-  ts(select_(gpo_mySeries_MF_daily, task_type),start=c(2015,1),frequency = 12)  # check for frequency and start information  
    
    TS_gpo_mySeries_ARIMA_fit <- auto.arima(myY)
    TS_gpo_mySeries_ARIMA_Forecast <-forecast(TS_gpo_mySeries_ARIMA_fit,h=forecast_n)
    
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_MF_daily_df <- cbind(as.data.frame(TS_gpo_mySeries_ARIMA_fit$fitted)[1:nrow(gpo_mySeries_MF_daily),],
                             select_(gpo_mySeries_MF_daily, quote(Date)))
    
    colnames(fit_MF_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_MF_daily_df <- with(TS_gpo_mySeries_ARIMA_Forecast,
                                 data.frame(mean=TS_gpo_mySeries_ARIMA_Forecast$mean,
                                            upper=TS_gpo_mySeries_ARIMA_Forecast$upper[,2],
                                            lower=TS_gpo_mySeries_ARIMA_Forecast$lower[,2],
                                            back=TS_gpo_mySeries_ARIMA_Forecast$upper-(resource_n*capacity_n)))
    
    forecast_MF_daily_df$Date <- seq.Date(max(gpo_mySeries_MF_daily$Date+31),
                                          by = "month",length.out = forecast_n)
    
    mySeq <- seq.Date(as.Date(first(select_(forecast_MF_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_MF_daily_df$Date <- mySeq
    
    forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n3] <- forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n3] + mark_n2
    forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n3] <- forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n3] + mark_n2
    forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n3] <- forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n3] + mark_n2
    
    
    #backlog addition to the forecast dataframe
    forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n4] <- forecast_MF_daily_df$mean[forecast_MF_daily_df$Date==date_n4] + back_n2
    forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n4] <- forecast_MF_daily_df$upper[forecast_MF_daily_df$Date==date_n4] + back_n2
    forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n4] <- forecast_MF_daily_df$lower[forecast_MF_daily_df$Date==date_n4] + back_n2
    
    
    forecast_MF_daily_df$back <- forecast_MF_daily_df$upper-(resource_n*capacity_n)
    
    forecast_MF_daily_df[forecast_MF_daily_df < 0]<-0
    
    forecast_backlog_df <- with(TS_gpo_mySeries_ARIMA_Forecast,
                                data.frame(backlog = TS_gpo_mySeries_ARIMA_Forecast$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(gpo_mySeries_MF_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + mark_n2
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n4] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n4] + back_n2
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    # forecast_backlog_df <- with(TS_gpo_mySeries_ARIMA_Forecast,
    #                             data.frame(backlog = TS_gpo_mySeries_ARIMA_Forecast$mean-(resource_n*capacity_n)))
    # 
    # forecast_backlog_df$Date <- seq.Date(max(gpo_mySeries_MF_daily$Date+31),
    #                                      by = "month",length.out = forecast_n)
    # 
    # forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table2 <- renderDataTable({
      
      forecast_MF_daily_df <- forecast_MF_daily_df%>%
        filter(mean > 0) %>%
        select(Date, mean, upper, lower,back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2)
        )
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader2 = downloadHandler(paste0('MF','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_MF_daily_df, file)
    })
    
    
    output$performance_metrics1 <- renderDataTable({
      
      myAccuracy <- accuracy(TS_gpo_mySeries_ARIMA_fit)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare1 = bind_cols(performance$performance_compare1,
                                                    data.frame(MF = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(gpo_mySeries_MF_daily, series = task_type),
               select_(gpo_mySeries_MF_daily, quote(Date)),
               order.by=as.POSIXct(gpo_mySeries_MF_daily$Date))
    
    myfitted <- xts(select_(fit_MF_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(gpo_mySeries_MF_daily$Date))
    
    myPred <- xts(select_(forecast_MF_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_MF_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred,myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('ARIMA Forecast: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = TRUE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      #dyCrosshair(direction = "vertical")%>%
      #dyLegend(show = "follow")%>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  #######  SIMPLE EXPONENTIAL SMOOTHING DYGRAPH - GPO  ####### 
  output$m_SES <- renderDygraph({
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    gpo_mySeries_SES <- gpo_mySeries_filtered()
    
    if (nrow(gpo_mySeries_SES) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select2
    forecast_n <- input$i_forecast_n2
    resource_n <- input$Num_Res2
    capacity_n <- input$capacity2
    date_n3 <- input$i_Date3
    mark_n2 <- input$i_market2
    date_n4 <- input$i_Date4
    back_n2 <- input$i_back2
    
    
    #simple exponential smoothing
    myY <-  ts(select_(gpo_mySeries_SES, task_type),start=c(2015,1), frequency=12)
    
    TS_gpo_mySeries_SES <- ses(myY, h=forecast_n, initial='optimal')
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_SES_daily_df <- cbind(as.data.frame(TS_gpo_mySeries_SES$fitted)[1:nrow(gpo_mySeries_SES),],
                              select_(gpo_mySeries_SES, quote(Date)))
    
    colnames(fit_SES_daily_df) <- c('fitted','Date')
    
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_SES_daily_df <- with(TS_gpo_mySeries_SES,
                                  data.frame(mean=TS_gpo_mySeries_SES$mean,
                                             upper=TS_gpo_mySeries_SES$upper[,2],
                                             lower=TS_gpo_mySeries_SES$lower[,2],
                                             back=TS_gpo_mySeries_SES$upper-(resource_n*capacity_n)))
    forecast_SES_daily_df$Date <- seq.Date(max(gpo_mySeries_SES$Date+31),
                                           by = "month",length.out = forecast_n)
    mySeq <- seq.Date(as.Date(first(select_(forecast_SES_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_SES_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n3] <- forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n3] + mark_n2
    forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n3] <- forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n3] + mark_n2
    forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n3] <- forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n3] + mark_n2
    
    
    # Add backlog to immediate next month for table
    forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n4] <- forecast_SES_daily_df$mean[forecast_SES_daily_df$Date==date_n4] + back_n2
    forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n4] <- forecast_SES_daily_df$upper[forecast_SES_daily_df$Date==date_n4] + back_n2
    forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n4] <- forecast_SES_daily_df$lower[forecast_SES_daily_df$Date==date_n4] + back_n2
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_SES_daily_df$back <- forecast_SES_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_SES_daily_df[forecast_SES_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(TS_gpo_mySeries_SES,
                                data.frame(backlog = TS_gpo_mySeries_SES$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(gpo_mySeries_SES$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + mark_n2
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n4] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + back_n2
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table2 <- renderDataTable({
      
      forecast_SES_daily_df <- forecast_SES_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back, 2)
        )
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader1 = downloadHandler(paste0('SES','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_SES_daily_df, file)
    })
    
    
    output$performance_metrics1 <- renderDataTable({
      
      myAccuracy <- accuracy(TS_gpo_mySeries_SES)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare1 = bind_cols(performance$performance_compare1,
                                                    data.frame(SES = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(gpo_mySeries_SES, series = task_type),
               select_(gpo_mySeries_SES, quote(Date)),
               order.by=as.POSIXct(gpo_mySeries_SES$Date))
    
    myfitted <- xts(select_(fit_SES_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(gpo_mySeries_SES$Date))
    
    myPred <- xts(select_(forecast_SES_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower),
                          quote(Date)),
                  order.by = as.POSIXct(forecast_SES_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    
    myDy <- cbind(myX, myfitted, myPred, myback)
    
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('SIMPLE EXPONENTIAL SMOOTHING of ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  #######  HOLT-WINTERS EXPONENTIAL SMOOTHING DYGRAPH - GPO  ####### 
  output$m_HW <- renderDygraph({
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    gpo_mySeries_HW_daily <- gpo_mySeries_filtered()
    
    if (nrow(gpo_mySeries_HW_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select2
    forecast_n <- input$i_forecast_n2
    resource_n <- input$Num_Res2
    capacity_n <- input$capacity2
    date_n3 <- input$i_Date3
    mark_n2 <- input$i_market2
    date_n4 <- input$i_Date4
    back_n2 <- input$i_back2
    
    
    
    #HW exponential smoothing
    myY <-  ts(select_(gpo_mySeries_HW_daily, task_type),start = c(2015,1),frequency=12)   
    
    #HW smoothing
   # TS_gpo_mySeries_HW_daily <- ets(myY,
    #                                model="ZAN",
     #                               damped=F,
      #                              additive.only=T
       #                             #gamma=myM
    #)
    
    TS_gpo_mySeries_HW_daily<- hw(myY, h=forecast_n)
    
    #create fit object
    fit_HW_daily_df <- cbind(as.data.frame(TS_gpo_mySeries_HW_daily$fitted)[1:nrow(gpo_mySeries_HW_daily),],
                             select_(gpo_mySeries_HW_daily, quote(Date)))
    
    colnames(fit_HW_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_HW_daily <- forecast(TS_gpo_mySeries_HW_daily, h=forecast_n)
    forecast_HW_daily_df <- with(forecast_HW_daily,
                                 data.frame(mean=forecast_HW_daily$mean,
                                            upper=forecast_HW_daily$upper[,2],
                                            lower=forecast_HW_daily$lower[,2],
                                            back=forecast_HW_daily$upper-(resource_n*capacity_n)))
    forecast_HW_daily_df$Date <- seq.Date(max(gpo_mySeries_HW_daily$Date+31),
                                          by = "month",length.out = forecast_n)
    
    
    mySeq <- seq.Date(as.Date(first(select_(forecast_HW_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_HW_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n3] <- forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n3] + mark_n2
    forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n3] <- forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n3] + mark_n2
    forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n3] <- forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n3] + mark_n2
    
    
    # Add backlog to immediate next month for table
    forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n4] <- forecast_HW_daily_df$mean[forecast_HW_daily_df$Date==date_n4] + back_n2
    forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n4] <- forecast_HW_daily_df$upper[forecast_HW_daily_df$Date==date_n4] + back_n2
    forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n4] <- forecast_HW_daily_df$lower[forecast_HW_daily_df$Date==date_n4] + back_n2
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_HW_daily_df$back <- forecast_HW_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_HW_daily_df[forecast_HW_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(forecast_HW_daily,
                                data.frame(backlog = forecast_HW_daily$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(gpo_mySeries_HW_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + mark_n2
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n4] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + back_n2
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table2 <- renderDataTable({
      
      forecast_HW_daily_df <- forecast_HW_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('HW','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_HW_daily_df, file)
    })
    
    
    output$performance_metrics1 <- renderDataTable({
      
      myAccuracy <- accuracy(TS_gpo_mySeries_HW_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare1 = bind_cols(performance$performance_compare1,
                                                    data.frame(HW = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(gpo_mySeries_HW_daily, series = task_type),
               select_(gpo_mySeries_HW_daily, quote(Date)),
               order.by=as.POSIXct(gpo_mySeries_HW_daily$Date))
    
    myfitted <- xts(select_(fit_HW_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(gpo_mySeries_HW_daily$Date))
    
    myPred <- xts(select_(forecast_HW_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_HW_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred, myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('HOLT-WINTERS EXPONENTIAL SMOOTHING of ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color="red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  #######  MOVING AVERAGE DYGRAPH - GPO  ####### 
  output$m_MA <- renderDygraph({
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    gpo_mySeries_MA_daily <- gpo_mySeries_filtered()
    
    if (nrow(gpo_mySeries_MA_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select2
    forecast_n <- input$i_forecast_n2
    resource_n <- input$Num_Res2
    capacity_n <- input$capacity2
    date_n3 <- input$i_Date3
    mark_n2 <- input$i_market2
    date_n4 <- input$i_Date4
    back_n2 <- input$i_back2
    
    
    myMA = 3
    
    #convert to time series object
    myY <-  ts(select_(gpo_mySeries_MA_daily, series = task_type),start = c(2015,1),frequency=12)   
    
    withProgress(message = 'Optimising forecast model... ', 
                 detail = 'this may take a few seconds',
                 value = 0.1,
                 min = 0,
                 max = 1, {
                   
                   #create moving average forecast object
                   TS_gpo_mySeries_MA_daily <- forecast(ma(myY, 
                                                           order = myMA),
                                                        h = forecast_n)
                 })
    
    
    #convert elements of time series MODEL to dataframe for plotting
    #fit_MA_daily_df <- select_(gpo_mySeries_MA_daily, task_type, quote(Date))
    
    fit_MA_daily_df <- cbind(as.data.frame(TS_gpo_mySeries_MA_daily$fitted)[1:nrow(gpo_mySeries_MA_daily),],
                             select_(gpo_mySeries_MA_daily, quote(Date)))
    
    colnames(fit_MA_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_MA_daily_df <- with(TS_gpo_mySeries_MA_daily,
                                 data.frame(mean=TS_gpo_mySeries_MA_daily$mean,
                                            upper=TS_gpo_mySeries_MA_daily$upper[,2],
                                            lower=TS_gpo_mySeries_MA_daily$lower[,2],
                                            back=TS_gpo_mySeries_MA_daily$upper-(resource_n*capacity_n)))
    forecast_MA_daily_df$Date <- seq.Date(max(gpo_mySeries_MA_daily$Date+31),
                                          by='month',length.out=forecast_n)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    mySeq <- seq.Date(as.Date(first(select_(forecast_MA_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_MA_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n3] <- forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n3] + mark_n2
    forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n3] <- forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n3] + mark_n2
    forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n3] <- forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n3] + mark_n2
    
    
    # Add backlog to immediate next month for table
    forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n4] <- forecast_MA_daily_df$mean[forecast_MA_daily_df$Date==date_n4] + back_n2
    forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n4] <- forecast_MA_daily_df$upper[forecast_MA_daily_df$Date==date_n4] + back_n2
    forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n4] <- forecast_MA_daily_df$lower[forecast_MA_daily_df$Date==date_n4] + back_n2
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_MA_daily_df$back <- forecast_MA_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_MA_daily_df[forecast_MA_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(TS_gpo_mySeries_MA_daily,
                                data.frame(backlog = TS_gpo_mySeries_MA_daily$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(gpo_mySeries_MA_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + mark_n2
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n4] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + back_n2
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table2 <- renderDataTable({
      
      forecast_MA_daily_df <- forecast_MA_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('MA','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_MA_daily_df, file)
    })
    
    output$performance_metrics1 <- renderDataTable({
      
      myAccuracy <- accuracy(TS_gpo_mySeries_MA_daily, x=gpo_mySeries_MA_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare1 = bind_cols(performance$performance_compare1,
                                                    data.frame(MA = myAccuracy[, 2]))
      })
      
    })
    
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(gpo_mySeries_MA_daily, series = task_type),
               select_(gpo_mySeries_MA_daily, quote(Date)),
               order.by=as.POSIXct(gpo_mySeries_MA_daily$Date))
    
    myfitted <- xts(select_(fit_MA_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(gpo_mySeries_MA_daily$Date))
    
    myPred <- xts(select_(forecast_MA_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_MA_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by = as.POSIXct(forecast_backlog_df$Date))
    
    myDy <- cbind(myX, myfitted, myPred, myback)
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0(myMA, '-ORDER MOVING AVERAGE of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      #dyRoller(rollPeriod = myMA) %>%     #lets users enter a 'smoothing' parameter interactively
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  
  
  #######  LINE OF BEST FIT LINEAR DYGRAPH - GPO ####### 
  output$m_LOBF <- renderDygraph({
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    gpo_mySeries_LOBF_daily <- gpo_mySeries_filtered()
    
    if (nrow(gpo_mySeries_LOBF_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select2
    forecast_n <- input$i_forecast_n2
    resource_n <- input$Num_Res2
    capacity_n <- input$capacity2
    date_n3 <- input$i_Date3
    mark_n2 <- input$i_market2
    date_n4 <- input$i_Date4
    back_n2 <- input$i_back2
    
    
    
    
    #line of best fit
    TS_gpo_mySeries_LOBF_daily <- lm(paste0(task_type, "~", quote(Date)), 
                                     data = gpo_mySeries_LOBF_daily)
    
    
    #convert elements of time series MODEL to dataframe for plotting
    fit_LOBF_daily_df1 <- cbind(as.vector(TS_gpo_mySeries_LOBF_daily$fitted.values),
                               select_(gpo_mySeries_LOBF_daily, quote(Date)))
    
    colnames(fit_LOBF_daily_df1) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    myTS <- ts(fit_LOBF_daily_df1$fitted,freq = 12)
    
    TS_gpo_mySeries_LOBF_daily <- forecast(myTS, 
                                           h=forecast_n)
    
    forecast_LOBF_daily_df1 <- with(TS_gpo_mySeries_LOBF_daily,
                                   data.frame(mean=TS_gpo_mySeries_LOBF_daily$mean,
                                              upper=TS_gpo_mySeries_LOBF_daily$upper[,2],
                                              lower=TS_gpo_mySeries_LOBF_daily$lower[,2],
                                              back=TS_gpo_mySeries_LOBF_daily$upper-(resource_n*capacity_n)))
    
    forecast_LOBF_daily_df1$Date <- seq.Date(max(gpo_mySeries_LOBF_daily$Date+31),
                                            by="month",length.out=forecast_n)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    mySeq <- seq.Date(as.Date(first(select_(forecast_LOBF_daily_df1, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_LOBF_daily_df1$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_LOBF_daily_df1$mean[forecast_LOBF_daily_df1$Date==date_n3] <- forecast_LOBF_daily_df1$mean[forecast_LOBF_daily_df1$Date==date_n3] + mark_n2
    forecast_LOBF_daily_df1$upper[forecast_LOBF_daily_df1$Date==date_n3] <- forecast_LOBF_daily_df1$upper[forecast_LOBF_daily_df1$Date==date_n3] + mark_n2
    forecast_LOBF_daily_df1$lower[forecast_LOBF_daily_df1$Date==date_n3] <- forecast_LOBF_daily_df1$lower[forecast_LOBF_daily_df1$Date==date_n3] + mark_n2
    
    
    # Add backlog to immediate next month for table
    forecast_LOBF_daily_df1$mean[forecast_LOBF_daily_df1$Date==date_n4] <- forecast_LOBF_daily_df1$mean[forecast_LOBF_daily_df1$Date==date_n4] + back_n2
    forecast_LOBF_daily_df1$upper[forecast_LOBF_daily_df1$Date==date_n4] <- forecast_LOBF_daily_df1$upper[forecast_LOBF_daily_df1$Date==date_n4] + back_n2
    forecast_LOBF_daily_df1$lower[forecast_LOBF_daily_df1$Date==date_n4] <- forecast_LOBF_daily_df1$lower[forecast_LOBF_daily_df1$Date==date_n4] + back_n2
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_LOBF_daily_df1$back <- forecast_LOBF_daily_df1$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_LOBF_daily_df1[forecast_LOBF_daily_df1 < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df1 <- with(TS_gpo_mySeries_LOBF_daily ,
                                data.frame(backlog = TS_gpo_mySeries_LOBF_daily$upper[,2]))
    
    forecast_backlog_df1$Date <- seq.Date(max(gpo_mySeries_LOBF_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df1$backlog[forecast_backlog_df1$Date==date_n3] <- forecast_backlog_df1$backlog[forecast_backlog_df1$Date==date_n3] + mark_n2
    
    forecast_backlog_df1$backlog[forecast_backlog_df1$Date==date_n4] <- forecast_backlog_df1$backlog[forecast_backlog_df1$Date==date_n3] + back_n2
    
    forecast_backlog_df1$backlog <- forecast_backlog_df1$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df1[forecast_backlog_df1<0] <- 0
    
    
    
    
    output$time_series_table2 <- renderDataTable({
      
      forecast_LOBF_daily_df1 <- forecast_LOBF_daily_df1 %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('LOBF','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_LOBF_daily_df1, file)
    })
    
    
    
    output$performance_metrics1 <- renderDataTable({
      
      myAccuracy <- accuracy(TS_gpo_mySeries_LOBF_daily, x=gpo_mySeries_LOBF_daily$WorkIn)[2,]
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var1, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare1 = bind_cols(performance$performance_compare1,
                                                    data.frame(LOBF = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(gpo_mySeries_LOBF_daily, series = task_type),
               select_(gpo_mySeries_LOBF_daily, quote(Date)),
               order.by=as.POSIXct(gpo_mySeries_LOBF_daily$Date))
    
    myfitted <- xts(select_(fit_LOBF_daily_df1, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(gpo_mySeries_LOBF_daily$Date))
    
    myPred <- xts(select_(forecast_LOBF_daily_df1, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_LOBF_daily_df1$Date))
    
    myback <- xts(select_(forecast_backlog_df1,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df1$Date))
    
    myDy <- cbind(myX, myfitted, myPred,myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('LINE OF BEST FIT of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color = "red")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      
      dyRangeSelector()
    
    print(d)
    
    
  })
  
  #######  COMBINED FORECAST DYGRAPH - GPO  ####### 
  output$m_COMBINE <- renderDygraph({
    
    #use existing reactive structures
    gpo_mySeries <- gpo_mySeries_raw()
    gpo_mySeries_COMB_daily <- gpo_mySeries_filtered()
    
    if (nrow(gpo_mySeries_COMB_daily) == 0){
      stop(
        
        showModal(modalDialog(
          title = "Important message",
          'Please hit "start forecasting"!',
          easyClose = TRUE,
          size = 's'
        ))
      )
    }
    
    #make inputs dependent on users hiting 'start forecasting' button
    task_type <- input$i_task_select2
    forecast_n <- input$i_forecast_n2
    resource_n <- input$Num_Res2
    capacity_n <- input$capacity2
    date_n3 <- input$i_Date3
    mark_n2 <- input$i_market2
    date_n4 <- input$i_Date4
    back_n2 <- input$i_back2
    
    
    #create ts object
    myY <-  ts(select_(gpo_mySeries_COMB_daily, task_type),start = c(2015,1),frequency = 12)   
    
    
    
    # create XREG object
    
   # myXReg_fit <- fourier(myY, K=2)
  #  myXReg_forecast <- cbind(fourier(myY, K = 2, h = forecast_n), 
   #                          ifelse(seq.Date(as.Date(last(select_(gpo_mySeries_COMB_daily, quote(Date)))[[1]]), 
    #                                         as.Date(last(select_(gpo_mySeries_COMB_daily, quote(Date)))[[1]]) + forecast_n, by = "month") %in% select_(pubhol, quote(Date)), 1, 0))
    
    
    
    
    #cast myY as numeric for 'hybridModel' function
    myY <- as.numeric(myY)
    
    #COMBINED Forecast using forecastHybrid package
    TS_gpo_mySeries_COMB_daily <- hybridModel(myY, 
                                              models = "ae",
                                              errorMethod = "MASE",
                                              weights = "insample.errors")
    
    
    
    #create fit object
    fit_COMB_daily_df <- cbind(as.data.frame(TS_gpo_mySeries_COMB_daily$fitted)[1:nrow(gpo_mySeries_COMB_daily),],
                               select_(gpo_mySeries_COMB_daily, quote(Date)))
    
    colnames(fit_COMB_daily_df) <- c('fitted','Date')
    
    
    #convert elements of time series FORECAST to dataframe for plotting
    forecast_COMB_daily <- forecast(TS_gpo_mySeries_COMB_daily, 
                                    h=forecast_n
                                    )
    forecast_COMB_daily_df <- with(forecast_COMB_daily,
                                   data.frame(mean=forecast_COMB_daily$mean,
                                              upper=forecast_COMB_daily$upper[,2],
                                              lower=forecast_COMB_daily$lower[,2],
                                              back=forecast_COMB_daily$upper-(resource_n*capacity_n)))
    forecast_COMB_daily_df$Date <- seq.Date(max(gpo_mySeries_COMB_daily$Date+31),
                                            by="month",length.out= forecast_n)
    
    
    #add validation as to whether forecast should include WEEKENDS or not
    mySeq <- seq.Date(as.Date(first(select_(forecast_COMB_daily_df, quote(Date)))[[1]]), length.out = forecast_n, by = "month")
    
    forecast_COMB_daily_df$Date <- mySeq
    
    # Add market intelligence to forecast dataframe for table 
    forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n3] <- forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n3] + mark_n2
    forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n3] <- forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n3] + mark_n2
    forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n3] <- forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n3] + mark_n2
    
    
    # Add backlog to immediate next month for table
    forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n4] <- forecast_COMB_daily_df$mean[forecast_COMB_daily_df$Date==date_n4] + back_n2
    forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n4] <- forecast_COMB_daily_df$upper[forecast_COMB_daily_df$Date==date_n4] + back_n2
    forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n4] <- forecast_COMB_daily_df$lower[forecast_COMB_daily_df$Date==date_n4] + back_n2
    
    
    # Calculate the backlog with capacity and resource for table
    forecast_COMB_daily_df$back <- forecast_COMB_daily_df$upper-(resource_n*capacity_n)
    
    # Check for negative values and mutate them to 0
    forecast_COMB_daily_df[forecast_COMB_daily_df < 0]<-0
    
    # convert elements of time series forecast to dataframe for backlog calculation and plotting
    forecast_backlog_df <- with(forecast_COMB_daily ,
                                data.frame(backlog = forecast_COMB_daily$upper[,2]))
    
    forecast_backlog_df$Date <- seq.Date(max(gpo_mySeries_COMB_daily$Date+31),
                                         by = "month",length.out = forecast_n)
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + mark_n2
    
    forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n4] <- forecast_backlog_df$backlog[forecast_backlog_df$Date==date_n3] + back_n2
    
    forecast_backlog_df$backlog <- forecast_backlog_df$backlog - (resource_n*capacity_n)
    
    forecast_backlog_df[forecast_backlog_df<0] <- 0
    
    
    output$time_series_table2 <- renderDataTable({
      
      forecast_COMB_daily_df <- forecast_COMB_daily_df %>% 
        filter(mean > 0) %>%
        select(Date, mean, upper, lower, back) %>%
        mutate(mean = round(mean, 2),
               upper = round(upper, 2),
               lower = round(lower, 2),
               back = round(back,2))
      
    })
    
    #build CSV file of forecasted valyes that can be downloaed when model selected
    output$downloader = downloadHandler(paste0('COMBINED','_',task_type,'_',forecast_n,'periods.csv'), content = function(file) {
      write.csv(forecast_COMB_daily_df, file)
    })
    
    
    output$performance_metrics1 <- renderDataTable({
      
      myAccuracy <- accuracy(TS_gpo_mySeries_COMB_daily)
      
      myAccuracy <- as.data.frame(as.table(myAccuracy))
      
      myAccuracy <- myAccuracy %>%
        select(metric = Var2, value = Freq) %>%
        mutate(value = round(value, 3))
      
      isolate({
        #update REACTIVE VALUES object with accuracy metrics
        performance$performance_compare1 = bind_cols(performance$performance_compare1,
                                                    data.frame(COMB = myAccuracy[, 2]))
      })
      
    })
    
    
    #construct DYGRAPH Visualisation
    myX <- xts(select_(gpo_mySeries_COMB_daily, series = task_type),
               select_(gpo_mySeries_COMB_daily, quote(Date)),
               order.by=as.POSIXct(gpo_mySeries_COMB_daily$Date))
    
    myfitted <- xts(select_(fit_COMB_daily_df, quote(fitted), quote(Date)),
                    order.by=as.POSIXct(gpo_mySeries_COMB_daily$Date))
    
    myPred <- xts(select_(forecast_COMB_daily_df, 
                          quote(mean), 
                          quote(upper), 
                          quote(lower), 
                          quote(Date)),
                  order.by = as.POSIXct(forecast_COMB_daily_df$Date))
    
    myback <- xts(select_(forecast_backlog_df,quote(backlog),quote(Date)),
                  order.by=as.POSIXct(forecast_backlog_df$Date))
    
    
    myDy <- cbind(myX, myfitted, myPred,myback)
    
    
    d <- dygraph(myDy[,1:9][,c(-3,-7,-9)], main=paste0('COMBINED FORECAST MODEL of: ', task_type, ' for ', forecast_n, ' periods' )) %>% 
      dyAxis("x", drawGrid = FALSE) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dySeries(c('upper','mean','lower'), label="predicted") %>%
      dySeries('series') %>%
      dySeries('fitted', fillGraph = TRUE) %>%
      dySeries('backlog',color="red") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyEvent("2018-1-1", "NewYear 2018", labelLoc = "bottom") %>%
      dyEvent("2018-5-1", "LabourDay 2018", labelLoc = "bottom")%>%
      dyEvent("2018-8-15", "Independence Day 2018", labelLoc = "bottom")%>%
      dyRangeSelector()
    
    print(d)
    
  })
  
  ###### PERFORMANCE METRIC COMPARISON TABLE ######
  output$s_COMPARE <- renderDataTable({
    
    performance$performance_compare1
    
  })
  
  
  
  
  #### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")
  
  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- readRDS("credentials/credentials.rds")
    
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password
    
    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }
    
    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {
      
      user_input$user_locked_out <- TRUE
      
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        saveRDS(credentials, "credentials/credentials.rds")
      }
    }
    
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }
    
    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })   
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),
      
      actionButton("login_button", "Log in")
    )
  })
  
  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
})
