enableBookmarking(store = "url")
# Define UI ----
ui <- function(request){
  tagList(
    includeCSS(path = "www/AdminLTE.css"),
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
                                            choices = Regions)),
                         column(4,
                                selectInput("Metric", "Metric Selection", 
                                            choices = Metrics))
                     )
              )
            ),
            
            # Output the HDD Trends and APE value tables
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
            
            # Overall Evaluation for All CFGs
            fluidRow(column(12,
                            box(title = "Overall Evaluation for All CFGs",
                                width = NULL, status = "primary", solidHeader = T,
                                column(6,plotlyOutput('ErrorPlotRegion')
                                ),
                                column(6,plotlyOutput('ErrorPlotWeekly')
                                ),
                                footer = "Attainment Rate is the percentage of CFGs whose APEs are below 20% and higher is better. For MAPE and MAPE median, lower is better."
                            )
            )
            )
          ),
          
          # Page 2
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
                                uiOutput('filter2')
                         ),
                         column(4,
                                uiOutput('filter3'))
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
                                plotlyOutput("CV_plotQ4")),
                         footer = "At the beginning of FY18Q1, using FY17 weekly sales data to forecast FY18Q2 with one quarter ahead. At the beginning of FY18Q2, using FY17 and FY18Q1 weekly sales data to forecast FY18Q3 with one quarter ahead. The same for FY18Q4 and FY19Q1."
                     )
              )
            )
          )
        )
      )
    )
  ) # end of tagList
}


