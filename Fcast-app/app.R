library(shiny)

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
                 submitButton("Submit")
    ),
    mainPanel(img(src = "ESG_HDD_SAS12G_1_2TB_10K_2_5 HDD Weekly Sales.png",width = 1200),
              textOutput("selected_var"),
              plotOutput("selected_plot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$selected_var <- renderText({ 
    paste("You have selected", input$CFG)
  })
  
  # temp_data <- filter(CFG_fcast.joined,CFG==input$CFG)# for certain CFG
  # 
  # p<- temp_data %>%
  #   ggplot(aes(x=Fiscal_Wk,y=HDD_QTY,group = Model, color = Model)) +
  #   geom_point(size = 2) +
  #   geom_line(size = 1.5,alpha=0.6) +
  #   labs(title = paste(input$CFG,'Weekly Sales'), x = "Fiscal Week", y = "Part Quantity") +
  #   theme_minimal(base_size = 14) +
  #   scale_color_tableau('tableau10medium') +
  #   scale_x_discrete(breaks = c('FY17W01', 'FY18W01', 'FY19W01')) +
  #   scale_y_continuous(label=comma) + expand_limits(y = 0)
  
  output$selected_plot <- renderPlotly({ggplotly(p,width = 999)})
}

# Run the app ----
shinyApp(ui = ui, server = server)