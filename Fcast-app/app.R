library(shiny)
library(tidyverse)
library(ggthemes)
options(scipen = 999)
library(lubridate)
library(tidyquant)
library(plotly)
library(scales) # for percent

#setwd("~/GitHub/ShinyPractice/Fcast-app/data")
CFG_fcast.joined <- readRDS('CFG_fcast_joined.rds')
CFGgroups <- levels(factor(CFG_fcast.joined$CFG))
Regions <- levels(factor(CFG_fcast.joined$Region))
Models <- levels(factor(CFG_fcast.joined$Model))
# CFG_fcast.joined$Region <- as.factor(CFG_fcast.joined$Region)

# Define UI ----
ui <- fluidPage(
  titlePanel(strong("HDD Sales Data and Demand Forecast")),
  
  sidebarLayout(
    sidebarPanel(selectInput("CFG", h3("CFG Selection"), 
                             choices = CFGgroups, selected = "ESG_HDD_SAS12G_1_2TB_10K_2_5"),
                 selectInput("Region", h3("Region Selection"), 
                             choices = Regions),
                 checkboxInput('AllRegions','All Regions'),
                 selectInput("Model", h3("Model Selection"), 
                             choices = Models[c(1,4:8)], selected = "Prophet")
    ),
    mainPanel(plotlyOutput("selected_plot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
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
    if (input$AllRegions){
      p <- output$selected_plot <- renderPlotly({
        # for certain CFG
        ggplotly(ggplot(filter(CFG_fcast.joined,CFG==input$CFG), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
                   geom_point(size = 2) +
                   geom_line(size = 1.5,alpha=0.6) +
                   labs(title = paste(input$CFG,input$Region,'Weekly Sales'), x = "Fiscal Week", y = "Part Quantity") +
                   theme_minimal(base_size = 14) +
                   scale_color_tableau('tableau10medium') +
                   scale_x_discrete(breaks = c('FY17W01', 'FY18W01', 'FY19W01')) +
                   scale_y_continuous(label=comma) + expand_limits(y = 0) + 
                   facet_wrap(~Region,nrow = 3, ncol = 1))
      })
    }
    print(p)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)