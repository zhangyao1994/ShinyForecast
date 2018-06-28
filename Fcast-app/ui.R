enableBookmarking(store = "url")
# Define UI ----
ui <- function(request){
  tagList(
    includeCSS(path = "www/AdminLTE.css"),
    
    #includeCSS(path = "www/shinydashboard.css"),
    tags$style(type="text/css",
               '.well {background-color: white;}',
               "body {padding-top: 80px;}",
               "body {padding-top: 50px;}",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }",
               '.box.box-solid.box-primary {box-shadow: 0 10px 1px rgba(0, 0, 0, 0.1);}'
    ),
    tags$head(
      tags$script(
        HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')
      )
    ),
    
    hidden(
      div(
        id = "app-content", # this is the main app content
        
        navbarPage( 
          title = "HDD Forecast",
          id = 'FcastHDD', position = 'fixed-top', fluid = T,
          theme = shinytheme('flatly'), 
          windowTitle = "Forecast HDD",
          
          tabPanel(
            'Overview', id = 'overview',
            
            tags$div(id = "mydiv",
                     br(),
                     br()
            ),
            
            # Select CFG, Region, and Overall Metrics on One Row
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "info", solidHeader = T,
                         column(4,
                                selectInput("CFG", "CFG Selection", 
                                            choices = CFGgroups, selected = "ESG_HDD_SAS12G_1_2TB_10K_2_5")),
                         column(4,
                                selectInput("Region", "Region Selection", 
                                            choices = Regions))
                     )
              )
            ),
            
            fluidRow(
              column(12,
                     box(title = "FY19W07-FY19W18 Forecast Result Comparison",
                         width = NULL, status = "primary", solidHeader = T,
                         column(width = 8, 
                                plotlyOutput("selected_plot")
                         ),
                         column(width = 4,
                                br(),
                                h4("APE of Forecast Region (%)"),
                                # Output: Table summarizing the values entered ----
                                tableOutput("APEvalues"),
                                br(),
                                h4("Weekly Mean APE (%)"),
                                # Output: Table summarizing the values entered ----
                                tableOutput("APEValues_week"),
                                br(),
                                p("APE: Absolute Percent Error")
                         )
                     )
              )
            ),
            # Output the HDD Trends and APE value tables
            
            fluidRow(column(12,
                            box(title = "Overall Evaluation for All CFGs",
                                width = NULL, status = "primary", solidHeader = T,
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
          ),
          
          tabPanel(
            'Cross-Validation', id = 'Cross-Validation',
            
            tags$div(id = "mydiv",
                     br(),
                     br()
            ),
            # Select CFG, Region, and Overall Metrics on One Row
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "info", solidHeader = T,
                         column(4,
                                uiOutput('filter1')
                         ),
                         column(4,
                                uiOutput('filter2'))
                     )
              )
            ),
            
            fluidRow(
              column(12,
                     box(title = "Four Quarterly Forecast Result Comparison", width = NULL, solidHeader = T,
                         status = "primary",
                         column(width = 6, align = 'center',
                                plotlyOutput("CV_plotQ1")),
                         column(width = 6, align = 'center',
                                plotlyOutput("CV_plotQ2")),
                         column(width = 6, align = 'center',
                                plotlyOutput("CV_plotQ3")),
                         column(width = 6, align = 'center',
                                plotlyOutput("CV_plotQ4"))
                     )
              )
            )
          )
        )
      )
    )
  ) # end of tagList
}


