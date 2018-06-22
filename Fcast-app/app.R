library(shiny)
#setwd("~/GitHub/ShinyPractice/Fcast-app/data")
CFG_fcast.joined <- readRDS('CFG_fcast_joined.rds')
CFGgroups <- levels(factor(CFG_fcast.joined$CFG))
Region.groups <- c("APJ","Americas","EMEA")#levels(factor(hdd_qty$RGN_DESC))
Model.groups <- levels(factor(CFG_fcast.joined$Model))

# Define UI ----
ui <- fluidPage(
  titlePanel(strong("HDD Sales Data and Demand Forecast")),
  
  sidebarLayout(
    sidebarPanel(selectInput("CFG", h3("CFG Selection"), 
                             choices = CFGgroups, selected = "ESG_HDD_SAS12G_1_2TB_10K_2_5"),
                 selectInput("Region", h3("Region Selection"), 
                             choices = Region.groups, selected = "APJ"),
                 selectInput("Model", h3("Model Selection"), 
                             choices = Model.groups[c(1,4:8)], selected = "Prophet"),
                 submitButton("Submit"), width = 3
    ),
    mainPanel(#img(src = "ESG_HDD_SAS12G_1_2TB_10K_2_5 HDD Weekly Sales.png",width = 1200),
              #textOutput("selected_var"),
              plotlyOutput("selected_plot"),width = 9
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_plot <- renderPlotly({
      # for certain CFG
      ggplotly(ggplot(filter(CFG_fcast.joined,CFG==input$CFG), aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
      geom_point(size = 2) +
      geom_line(size = 1.5,alpha=0.6) +
      labs(title = paste(input$CFG,'Weekly Sales'), x = "Fiscal Week", y = "Part Quantity") +
      theme_minimal(base_size = 14) +
      scale_color_tableau('tableau10medium') +
      scale_x_discrete(breaks = c('FY17W01', 'FY18W01', 'FY19W01')) +
      scale_y_continuous(label=comma) + expand_limits(y = 0))# +
      #annotate("text", x = 'FY19W01', y = 25, label = "Forecast Region") #+
      # geom_rect(xmin = 52*2+7,#'FY19W07',
      #           xmax = 52*2+18,#'FY19W18',
      #           ymin = 0, ymax = 45000,
      #           fill = palette_light()[[4]], alpha = 0.01)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)