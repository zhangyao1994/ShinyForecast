# Define UI ----
ui <- fluidPage( theme = shinytheme("slate"),
                 #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI to select themes
                 titlePanel(strong("HDD Demand Forecast")),
                 # Select CFG, Region, and Model
                 sidebarLayout(
                   sidebarPanel(br(),br(),
                                selectInput("CFG", h3("CFG Selection"), 
                                            choices = CFGgroups, selected = "ESG_HDD_SAS12G_1_2TB_10K_2_5"),
                                selectInput("Region", h3("Region Selection"), 
                                            choices = Regions),
                                br(),br(),br(),
                                # checkboxInput('AllRegions','All Regions'),
                                # selectInput("Model", h3("Model Selection"), 
                                #             choices = Models[c(1,4:8)], selected = "Prophet"),
                                width = 4
                   ),
                   # Output the HDD Trends
                   mainPanel(plotlyOutput("selected_plot")
                   )
                 ),
                 
                 fluidRow(column(width = 4, align = 'center',
                                 br(),br(),
                                 h4("APE of Forecast Region (%)"),
                                 # Output: Table summarizing the values entered ----
                                 tableOutput("APEvalues"),
                                 br(),
                                 h4("Weekly Mean APE (%)"),
                                 # Output: Table summarizing the values entered ----
                                 tableOutput("APEValues_week")),
                          column(width = 8, align = 'center',
                                 plotlyOutput("CV_plot"))
                          
                          # sidebarPanel(selectInput("APE", h3("APE Selection"), 
                          #                          choices = c('Weekly','Forecast_Region'), selected = 'Forecast_Region'),
                          #              width = 4
                          # ),
                          # Main panel for displaying outputs ----
                          
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