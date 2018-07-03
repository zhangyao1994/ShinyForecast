# Define server logic ----
server <- function(input, output) {
  
  # Slection Connection between page 1 and 2
  output$filter1 <- renderUI({
    selection <- input$CFG
    selectInput("CFG2", "CFG Selection", 
                choices = CFGgroups, selected = selection)
  })
  output$filter2 <- renderUI({
    selection <- input$Region
    selectInput("Region2", "Region Selection", 
                choices = Regions, selected = selection)
  })
  output$filter3 <- renderUI({
    selection <- input$Metric
    selectInput("Metric2", "Overall Metric Selection", 
                choices = Metrics, selected = selection)
  })
  
  # Page 1
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
  
  # APE values Forecast Region----
  APEValues <- reactive({
    data.selected <- filter(APEresults,CFG==input$CFG,Region==input$Region)
    APE.selected <- data.selected[,seq(4,16,2)]
    colnames(APE.selected) <- c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
    APE.selected
  })
  
  # APE values Forecast Region----
  APEValues_week <- reactive({
    data.selected <- filter(APEresults,CFG==input$CFG,Region==input$Region)
    APE.selected <- data.selected[,seq(3,16,2)]
    colnames(APE.selected) <- c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
    APE.selected
  })
  
  # Show the values in an HTML table ----
  output$APEvalues <- renderDataTable({
    APE.selected <- APEValues()
    APE.selected %>% 
      datatable(options=list(rowCallback = JS(
        'function(row, data) {
        var num_data = data.slice(1,data.length)
        num_data.sort(function (a, b) {  return a - b;  });
        for(i=1;i < data.length; i++) {
          if(data[i]==num_data[6]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
  } else if(data[i]==num_data[5]) {
        $("td:eq("+i+")", row).css("background-color", "#fff1e6")
  } else if(data[i]==num_data[4]) {
        $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
  } else if(data[i]==num_data[3]) {
        $("td:eq("+i+")", row).css("background-color", " #ffc999")
  } else if(data[i]==num_data[2]) {
        $("td:eq("+i+")", row).css("background-color", "#ffa04d")
  } else if(data[i]==num_data[1]) {
        $("td:eq("+i+")", row).css("background-color", "#ff7700")
  } else if(data[i]==num_data[0]) {
        $("td:eq("+i+")", row).css("background-color", "#cc5f00")
  }
        }
  }'))) %>% 
      formatRound(columns=c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2) 
  })
  
  output$APEValues_week <- renderDataTable({
    APEvalues.selected <- APEValues_week()
    APEvalues.selected %>% 
      datatable(options=list(rowCallback = JS(
        'function(row, data) {
        var num_data = data.slice(1,data.length)
        num_data.sort(function (a, b) {  return a - b;  });
        for(i=1;i < data.length; i++) {
          if(data[i]==num_data[6]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
  } else if(data[i]==num_data[5]) {
        $("td:eq("+i+")", row).css("background-color", "#fff1e6")
  } else if(data[i]==num_data[4]) {
        $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
  } else if(data[i]==num_data[3]) {
        $("td:eq("+i+")", row).css("background-color", " #ffc999")
  } else if(data[i]==num_data[2]) {
        $("td:eq("+i+")", row).css("background-color", "#ffa04d")
  } else if(data[i]==num_data[1]) {
        $("td:eq("+i+")", row).css("background-color", "#ff7700")
  } else if(data[i]==num_data[0]) {
        $("td:eq("+i+")", row).css("background-color", "#cc5f00")
  }
}
    }'))) %>% 
      formatRound(columns=c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2) 
  }
  )
  
  # Plot Errors
  output$ErrorPlotRegion <- renderPlotly({
    ggplotly(filter(Eval.results_fcastRegion,Region==input$Region,Results==input$Metric) %>% 
               select(-Region,-Results,-Comb) %>%
               gather(key="Model",value="Value") %>%
               ggplot(aes(Model,Value)) +
               geom_bar(stat = "identity") +
               labs(title = paste('Model Comparison by Forecast Region Accuracy'), x = "Models", y = paste(str_replace_all(input$Metric,'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$ErrorPlotWeekly <- renderPlotly({
    ggplotly(filter(Eval.results_wk,Region==input$Region,Results==input$Metric) %>% 
               select(-Region,-Results,-Comb) %>%
               gather(key="Model",value="Value") %>%
               ggplot(aes(Model,Value)) +
               geom_bar(stat = "identity") +
               labs(title = paste('Model Comparison by Weekly Accuracy'), x = "Models", y = paste(str_replace_all(input$Metric,'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  
  # Page 2
  # Plot CV
  output$CV_plotQ1 <- renderPlotly({
    # for certain CFG and Region
    p <- ggplotly(ggplot(filter(All_fcast_CV,CFG==input$CFG2,Region==input$Region2,Quarter=="FY18Q1",Fiscal_Wk>="W13"), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                    geom_point(size = 2) +
                    geom_line(size = 1.5,alpha=0.6) +
                    labs(title = paste("FY18Q2"), x = "Fiscal Week", y = "Part Quantity") +
                    theme_minimal(base_size = 14) +
                    scale_color_tableau('tableau10medium') +
                    scale_x_discrete(breaks = c('W01', 'W13', 'W26')) +
                    scale_y_continuous(label=comma) + expand_limits(y = 0))
    #                 guides(color=FALSE) + # remove the default legend, but this does not help with the legend order
    #                 scale_color_discrete(breaks=c("Truth","Xgboost","ARIMA","Prophet","RandomForest","LinearModel","TBATS")))
    print(p)
  })
  output$CV_plotQ2 <- renderPlotly({
    # for certain CFG and Region
    p <- ggplotly(ggplot(filter(All_fcast_CV,CFG==input$CFG2,Region==input$Region2,Quarter=="FY18Q2",Fiscal_Wk>="W13"), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                    geom_point(size = 2) +
                    geom_line(size = 1.5,alpha=0.6) +
                    labs(title = paste("FY18Q3"), x = "Fiscal Week", y = "Part Quantity") +
                    theme_minimal(base_size = 14) +
                    scale_color_tableau('tableau10medium') +
                    scale_x_discrete(breaks = c('W01', 'W13', 'W26')) +
                    scale_y_continuous(label=comma) + expand_limits(y = 0))
    #                 guides(color=FALSE) + # remove the default legend, but this does not help with the legend order
    #                 scale_color_discrete(breaks=c("Truth","Xgboost","ARIMA","Prophet","RandomForest","LinearModel","TBATS")))
    print(p)
  })
  output$CV_plotQ3 <- renderPlotly({
    # for certain CFG and Region
    p <- ggplotly(ggplot(filter(All_fcast_CV,CFG==input$CFG2,Region==input$Region2,Quarter=="FY18Q3",Fiscal_Wk>="W13"), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                    geom_point(size = 2) +
                    geom_line(size = 1.5,alpha=0.6) +
                    labs(title = paste("FY18Q4"), x = "Fiscal Week", y = "Part Quantity") +
                    theme_minimal(base_size = 14) +
                    scale_color_tableau('tableau10medium') +
                    scale_x_discrete(breaks = c('W01', 'W13', 'W26')) +
                    scale_y_continuous(label=comma) + expand_limits(y = 0)) 
    #                 guides(color=FALSE) + # remove the default legend, but this does not help with the legend order
    #                 scale_color_discrete(breaks=c("Truth","Xgboost","ARIMA","Prophet","RandomForest","LinearModel","TBATS")))
    print(p)
  })
  output$CV_plotQ4 <- renderPlotly({
    # for certain CFG and Region
    p <- ggplotly(ggplot(filter(All_fcast_CV,CFG==input$CFG2,Region==input$Region2,Quarter=="FY18Q4",Fiscal_Wk>="W13"), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                    geom_point(size = 2) +
                    geom_line(size = 1.5,alpha=0.6) +
                    labs(title = paste("FY19Q1"), x = "Fiscal Week", y = "Part Quantity") +
                    theme_minimal(base_size = 14) +
                    scale_color_tableau('tableau10medium') +
                    scale_x_discrete(breaks = c('W01', 'W13', 'W26')) +
                    scale_y_continuous(label=comma) + expand_limits(y = 0))
    #                 guides(color=FALSE) + # remove the default legend, but this does not help with the legend order
    #                 scale_color_discrete(breaks=c("Truth","Xgboost","ARIMA","Prophet","RandomForest","LinearModel","TBATS")))
    print(p)
  })
  
  # APE values Forecast Region----
  CV_APEValues <- reactive({
    data.selected <- filter(APEresults_CV,CFG==input$CFG2,Region==input$Region2)
    APE.selected <- data.selected[,seq(5,15,2)]
    APE.Mean <- as.numeric(lapply(APE.selected,mean))
    APE.selected <- rbind(APE.selected,APE.Mean)
    APE.selected <- cbind(c('FY18Q2','FY18Q3','FY18Q4','FY19Q1','Mean'),APE.selected)
    colnames(APE.selected) <- c('Quarter','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
    APE.selected
  })
  
  # APE values Forecast Region----
  CV_APEValues_week <- reactive({
    data.selected <- filter(APEresults_CV,CFG==input$CFG2,Region==input$Region2)
    APE.selected <- data.selected[,seq(4,15,2)]
    APE.Mean <- as.numeric(lapply(APE.selected,mean))
    APE.selected <- rbind(APE.selected,APE.Mean)
    APE.selected <- cbind(c('FY18Q2','FY18Q3','FY18Q4','FY19Q1','Mean'),APE.selected)
    colnames(APE.selected) <- c('Quarter','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
    APE.selected
  })
  
  # Show the values in an HTML table ----
  output$CV_APEvalues <- renderDataTable({
    APE.selected <- CV_APEValues()
    # APE.selected %>% datatable() %>%
    #   formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2) %>%
    #   formatStyle(colnames(APE.selected)[which.min(APE.selected[5,2:7])+1], backgroundColor = 'lightblue')
    
    APE.selected %>% 
      datatable(options=list(rowCallback = JS(
        'function(row, data) {
        var num_data = data.slice(1,data.length)
        num_data.sort(function (a, b) {  return a - b;  });
        for(i=1;i < data.length; i++) {
          if(data[i]==num_data[5]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
  } else if(data[i]==num_data[4]) {
        $("td:eq("+i+")", row).css("background-color", "#fff1e6")
  } else if(data[i]==num_data[3]) {
        $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
  } else if(data[i]==num_data[2]) {
        $("td:eq("+i+")", row).css("background-color", " #ffc999")
  } else if(data[i]==num_data[1]) {
        $("td:eq("+i+")", row).css("background-color", "#ffa04d")
  } else if(data[i]==num_data[0]) {
        $("td:eq("+i+")", row).css("background-color", "#ff7700")
  }
        }
  }')),rownames = FALSE) %>%
      formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2) 
  })
  
  output$CV_APEValues_week <- renderDataTable({
    APE.selected <- CV_APEValues_week()
    APE.selected %>%  datatable(options=list(rowCallback = JS(
      'function(row, data) {
        var num_data = data.slice(1,data.length)
        num_data.sort(function (a, b) {  return a - b;  });
        for(i=1;i < data.length; i++) {
          if(data[i]==num_data[5]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
  } else if(data[i]==num_data[4]) {
      $("td:eq("+i+")", row).css("background-color", "#fff1e6")
  } else if(data[i]==num_data[3]) {
      $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
  } else if(data[i]==num_data[2]) {
      $("td:eq("+i+")", row).css("background-color", " #ffc999")
  } else if(data[i]==num_data[1]) {
      $("td:eq("+i+")", row).css("background-color", "#ffa04d")
  } else if(data[i]==num_data[0]) {
      $("td:eq("+i+")", row).css("background-color", "#ff7700")
  }
}
    }')),rownames = FALSE) %>%
      formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2)
  })
  
  # Plot Errors
  output$CV_ErrorPlotRegion <- renderPlotly({
    ggplotly(filter(Eval.CV_fcastRegion,Region==input$Region2,Results==input$Metric2) %>% 
               select(-Region,-Results) %>%
               gather(key="Model",value="Value") %>%
               ggplot(aes(Model,Value)) +
               geom_bar(stat = "identity") +
               labs(title = paste('Forecast Region Accuracy'), x = "Models", y = paste(str_replace_all(input$Metric,'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  output$CV_ErrorPlotWeekly <- renderPlotly({
    ggplotly(filter(Eval.CV_wk,Region==input$Region2,Results==input$Metric2) %>% 
               select(-Region,-Results) %>%
               gather(key="Model",value="Value") %>%
               ggplot(aes(Model,Value)) +
               geom_bar(stat = "identity") +
               labs(title = paste('Weekly Accuracy'), x = "Models", y = paste(str_replace_all(input$Metric,'_',' '),"(%)")) + 
               theme_minimal(base_size = 14) + 
               scale_fill_tableau('tableau10medium'))
  })
  
  # Page 3
  output$MAPE_CV.Table <- renderDataTable({
    if (input$QtrMean=='Quarters'){
      data.selected <- APEresults_CV
      APE.selected <- data.selected[,c(1,2,3,seq(5,15,2))]
      colnames(APE.selected) <- c("CFG",'Quarter',"Region",'Prophet','ARIMA','TBATS','lm','RF','Xgboost')
      APE.selected %>%
        datatable(options=list(rowCallback = JS(
          'function(row, data) {
          var num_data = data.slice(1,data.length)
          num_data.sort(function (a, b) {  return a - b;  });
          for(i=1;i < data.length; i++) {
          if(data[i]==num_data[7]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
          } else if(data[i]==num_data[6]) {
          $("td:eq("+i+")", row).css("background-color", "#fff1e6")
          } else if(data[i]==num_data[5]) {
          $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
          } else if(data[i]==num_data[4]) {
          $("td:eq("+i+")", row).css("background-color", " #ffc999")
          } else if(data[i]==num_data[3]) {
          $("td:eq("+i+")", row).css("background-color", "#ffa04d")
          } else if(data[i]==num_data[2]) {
          $("td:eq("+i+")", row).css("background-color", "#ff7700")
          }
          }
    }')),rownames = FALSE) %>%
        formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2)
    } else {
      MAPE_CV.Table %>%
        datatable(options=list(rowCallback = JS(
          'function(row, data) {
          var num_data = data.slice(1,data.length)
          num_data.sort(function (a, b) {  return a - b;  });
          for(i=1;i < data.length; i++) {
          if(data[i]==num_data[7]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
          } else if(data[i]==num_data[6]) {
          $("td:eq("+i+")", row).css("background-color", "#fff1e6")
          } else if(data[i]==num_data[5]) {
          $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
          } else if(data[i]==num_data[4]) {
          $("td:eq("+i+")", row).css("background-color", " #ffc999")
          } else if(data[i]==num_data[3]) {
          $("td:eq("+i+")", row).css("background-color", "#ffa04d")
          } else if(data[i]==num_data[2]) {
          $("td:eq("+i+")", row).css("background-color", "#ff7700")
          }
          }
    }'))) %>% formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2)
    }
  })
  
  output$MAPE_CV_week.Table <- renderDataTable({
    if (input$QtrMean=='Quarters'){
      data.selected <- APEresults_CV
      APE.selected <- data.selected[,c(1,2,3,seq(4,15,2))]
      colnames(APE.selected) <- c("CFG",'Quarter',"Region",'Prophet','ARIMA','TBATS','lm','RF','Xgboost')
      APE.selected %>%
        datatable(options=list(rowCallback = JS(
          'function(row, data) {
          var num_data = data.slice(1,data.length)
          num_data.sort(function (a, b) {  return a - b;  });
          for(i=1;i < data.length; i++) {
          if(data[i]==num_data[7]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
          } else if(data[i]==num_data[6]) {
          $("td:eq("+i+")", row).css("background-color", "#fff1e6")
          } else if(data[i]==num_data[5]) {
          $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
          } else if(data[i]==num_data[4]) {
          $("td:eq("+i+")", row).css("background-color", " #ffc999")
          } else if(data[i]==num_data[3]) {
          $("td:eq("+i+")", row).css("background-color", "#ffa04d")
          } else if(data[i]==num_data[2]) {
          $("td:eq("+i+")", row).css("background-color", "#ff7700")
          }
          }
    }')),rownames = FALSE) %>%
        formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2)
  } else {
    MAPE_CV_week.Table %>% 
      datatable(options=list(rowCallback = JS(
        'function(row, data) {
        var num_data = data.slice(1,data.length)
        num_data.sort(function (a, b) {  return a - b;  });
        for(i=1;i < data.length; i++) {
          if(data[i]==num_data[7]) {
          $("td:eq("+i+")", row).css("background-color", "#ffffff")
  } else if(data[i]==num_data[6]) {
        $("td:eq("+i+")", row).css("background-color", "#fff1e6")
  } else if(data[i]==num_data[5]) {
        $("td:eq("+i+")", row).css("background-color", "#ffe4cc")
  } else if(data[i]==num_data[4]) {
        $("td:eq("+i+")", row).css("background-color", " #ffc999")
  } else if(data[i]==num_data[3]) {
        $("td:eq("+i+")", row).css("background-color", "#ffa04d")
  } else if(data[i]==num_data[2]) {
        $("td:eq("+i+")", row).css("background-color", "#ff7700")
  }
        }
  }')),rownames = FALSE) %>% formatRound(columns=c('Prophet','ARIMA','TBATS','lm','RF','Xgboost'), digits=2)
  }})
}