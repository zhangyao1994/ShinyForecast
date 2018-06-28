# Define server logic ----
server <- function(input, output) {
  # Plot HDD Trends
  output$selected_plot <- renderPlotly({
    # for certain CFG and Region
    p <- ggplotly(ggplot(filter(CFG_fcast.joined,CFG==input$CFG,Region==input$Region), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                    geom_point(size = 2) +
                    geom_line(size = 1.5,alpha=0.6) +
                    labs(title = paste(input$CFG,input$Region,'Weekly Sales'), x = "Fiscal Week", y = "Part Quantity") +
                    theme_minimal(base_size = 14) +
                    scale_color_tableau('tableau10medium') +
                    scale_x_discrete(breaks = c('FY17W01', 'FY18W01', 'FY19W01')) +
                    scale_y_continuous(label=comma) + expand_limits(y = 0))
    
    print(p)
  })
  
  # Plot CV
  output$CV_plot <- renderPlotly({
    # for certain CFG and Region
    p <- ggplotly(ggplot(filter(All_fcast_CV,CFG==input$CFG,Region==input$Region,Quarter=="FY18Q1"), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                    geom_point(size = 2) +
                    geom_line(size = 1.5,alpha=0.6) +
                    labs(title = paste("FY18Q1 - FY18Q2",'Weekly Sales'), x = "Fiscal Week", y = "Part Quantity") +
                    theme_minimal(base_size = 14) +
                    scale_color_tableau('tableau10medium') +
                    scale_x_discrete(breaks = c('W01', 'W13', 'W26')) +
                    scale_y_continuous(label=comma) + expand_limits(y = 0)) # + 
    #                 guides(color=FALSE) + # remove the default legend, but this does not help with the legend order
    #                 scale_color_discrete(breaks=c("Truth","Xgboost","ARIMA","Prophet","RandomForest","LinearModel","TBATS")))
    print(p)
  })
  
  # Plot Errors
  output$ErrorPlot1 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[1]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[1],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[1],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot2 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[2]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[2],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[2],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot3 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[3]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[3],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[3],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot4 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[4]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[4],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[4],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot5 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[5]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[5],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[5],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot6 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[6]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[6],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[6],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot7 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[7]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[7],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[7],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot8 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[8]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[8],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[8],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlot9 <- renderPlotly({
    ggplotly(ErrorResults4plot %>% ggplot(aes(x=Models),fill=Models) + geom_bar(aes_string(y=ResultsNames[9]),stat = "identity") +
               labs(title = paste('Model Comparison by',str_replace_all(ResultsNames[9],'_',' ')), x = "Models", y = paste(str_replace_all(ResultsNames[9],'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  
  # APE values Forecast Region----
  APEValues <- reactive({
    data.selected <- filter(APEreslts,CFG==input$CFG,Region==input$Region)
    APE.selected <- data.selected[,seq(4,16,2)]
    colnames(APE.selected) <- c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
    APE.selected
  })
  
  # APE values Forecast Region----
  APEValues_week <- reactive({
    data.selected <- filter(APEreslts,CFG==input$CFG,Region==input$Region)
    APE.selected <- data.selected[,seq(3,16,2)]
    colnames(APE.selected) <- c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
    APE.selected
  })
  
  # Show the values in an HTML table ----
  output$APEvalues <- renderTable({
    APEValues()
  })
  output$APEValues_week <- renderTable({
    APEValues_week()
  })
  
}