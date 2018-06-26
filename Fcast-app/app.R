# Updated on 06/26/2018 # I want to make it better-looking tomorrow!!!

# Load library
library(shiny)
library(tidyverse)
library(ggthemes)
options(scipen = 999)
library(lubridate)
library(tidyquant)
library(plotly)
library(scales) # for percent

# Load data
CFG_fcast.joined <- readRDS('CFG_fcast_joined.rds')
ErrorResults4plot <- readRDS('ErrorResults4plot.rds')
APEreslts <- readRDS('APEreslts.rds')
# Selections for selectInput
ResultsNames <- c('Attainment_Rates_Americas','MAPE_Americas','MAPE_median_Americas','Attainment_Rates_APJ','MAPE_APJ','MAPE_median_APJ','Attainment_Rates_EMEA','MAPE_EMEA','MAPE_median_EMEA')
CFGgroups <- levels(factor(CFG_fcast.joined$CFG))
Regions <- levels(factor(CFG_fcast.joined$Region))
Models <- levels(factor(CFG_fcast.joined$Model))

# Define UI ----
ui <- fluidPage(
  titlePanel(strong("HDD Sales Data and Demand Forecast")),
  # Select CFG, Region, and Model
  fluidRow(
    sidebarPanel(selectInput("CFG", h3("CFG Selection"), 
                             choices = CFGgroups, selected = "ESG_HDD_SAS12G_1_2TB_10K_2_5"),
                 selectInput("Region", h3("Region Selection"), 
                             choices = Regions),
                 # checkboxInput('AllRegions','All Regions'),
                 # selectInput("Model", h3("Model Selection"), 
                 #             choices = Models[c(1,4:8)], selected = "Prophet"),
                 selectInput("APE", h3("APE Selection"), 
                             choices = c('Weekly','Forecast_Region'), selected = 'Forecast_Region'),
                 width = 4
    ),
    # Output the HDD Trends
    mainPanel(plotlyOutput("selected_plot")
    )
  ),
  
  fluidRow(
    # Main panel for displaying outputs ----
    mainPanel(
      h4("APE Values"),
      # Output: Table summarizing the values entered ----
      tableOutput("APEvalues"),
      width = 4
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(str_replace_all(ResultsNames[1],'_',' '),
                 plotlyOutput('ErrorPlot1')
        ),
        tabPanel(str_replace_all(ResultsNames[2],'_',' '),
                 plotlyOutput('ErrorPlot2')
        ),
        tabPanel(str_replace_all(ResultsNames[3],'_',' '),
                 plotlyOutput('ErrorPlot3')
        ),
        tabPanel(str_replace_all(ResultsNames[4],'_',' '),
                 plotlyOutput('ErrorPlot4')
        ),
        tabPanel(str_replace_all(ResultsNames[5],'_',' '),
                 plotlyOutput('ErrorPlot5')
        ),
        tabPanel(str_replace_all(ResultsNames[6],'_',' '),
                 plotlyOutput('ErrorPlot6')
        ),
        tabPanel(str_replace_all(ResultsNames[7],'_',' '),
                 plotlyOutput('ErrorPlot7')
        ),
        tabPanel(str_replace_all(ResultsNames[8],'_',' '),
                 plotlyOutput('ErrorPlot8')
        ),
        tabPanel(str_replace_all(ResultsNames[9],'_',' '),
                 plotlyOutput('ErrorPlot9')
        )
      )
    )
  )
)

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
  
  # Reactive expression to APE values ----
  APEValues <- reactive({
    data.selected <- filter(APEreslts,CFG==input$CFG,Region==input$Region)
    if (input$APE=='Forecast_Region'){
      APE.selected <- data.selected[,seq(4,16,2)]
      colnames(APE.selected) <- c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
      APE.selected
    }
    else {
      APE.selected <- data.selected[,seq(3,16,2)]
      colnames(APE.selected) <- c('MRP','Prophet','ARIMA','TBATS','lm','RF','Xgboost')
      APE.selected
    }
  })
  
  # Show the values in an HTML table ----
  output$APEvalues <- renderTable({
    APEValues()
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)