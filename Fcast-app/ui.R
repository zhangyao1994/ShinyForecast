# Define UI ----
ui <- navbarPage( 
  title = HTML('<a href="http://apigateway.aus.amer.dell.com/ddsc/">FcastHDD</a>'), 
  id = 'FcastHDD', position = 'fixed-top', fluid = T,
  theme = shinytheme('flatly'), 
  windowTitle = "Forecast HDD",
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI to select themes
  
  tabPanel(
    'Overview', id = 'overview',
    
    fluidPage(
      # Select CFG, Region, and Overall Metrics on One Row
      fluidRow(
        column(3,align = 'center',
               selectInput("CFG", h3("CFG Selection"), 
                           choices = CFGgroups, selected = "ESG_HDD_SAS12G_1_2TB_10K_2_5")),
        column(3,align = 'center',
               selectInput("Region", h3("Region Selection"), 
                           choices = Regions)),
        column(3,align = 'center'
               # selectInput("Metrics", h3("Overall Evaluation Metrics"), 
               #             choices = c("Attainment Rates","MAPE"))
        )
      ),
      
      fluidRow(
        column(8, h3("FY19W07-FY19W18 Forecast Result Comparison"))
      ),
      # Output the HDD Trends and APE value tables
      fluidRow( 
        mainPanel(plotlyOutput("selected_plot")),
        column(width = 4, align = 'center',
               br(),br(),
               h4("APE of Forecast Region (%)"),
               # Output: Table summarizing the values entered ----
               tableOutput("APEvalues"),
               br(),
               h4("Weekly Mean APE (%)"),
               # Output: Table summarizing the values entered ----
               tableOutput("APEValues_week"))
      ),
      
      fluidRow(column(width=10,offset = 1, hr(),br(),
                      titlePanel("Overall Evaluation for All CFGs"),
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
      ))
    )
    
    
  ),
  
  tabPanel(
    'Cross-Validation', id = 'Cross-Validation',
    fluidRow(
      column(12, align = 'center',
             h3("Four Quarterly Forecast Result Comparison"))
    ),
    
    fluidRow(
      column(width = 3, align = 'center',
             plotlyOutput("CV_plotQ1")),
      column(width = 3, align = 'center',
             plotlyOutput("CV_plotQ2")),
      column(width = 3, align = 'center',
             plotlyOutput("CV_plotQ3")),
      column(width = 3, align = 'center',
             plotlyOutput("CV_plotQ4"))
    )
  )
)