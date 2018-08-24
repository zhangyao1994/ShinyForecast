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
            'Test Overview', id = 'test',
            
            tags$div(id = "mydiv",
                     br(),
                     br()
            ),
            
            # Select CFG, Region, and Overall Metrics on One Row----
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
                                selectInput("Metric", "Overall Metric Selection", 
                                            choices = Metrics))
                     )
              )
            ),
            
            # Output the HDD Trends and APE value tables----
            fluidRow(
              column(12,
                     box(title = "FY19W07-FY19W20 Forecast Result Comparison",
                         width = NULL, status = "primary", solidHeader = T,
                         p('Based on the sales data before FY19W07, different models are applied to forecast HDD demand during FY19W07-FY19W20. Forecast results are compared to MRP.'),
                         p('Note: for each row in the tables, darker blue colors indicate lower Absolute Percent Errors (APE), which are preferred.'),
                         column(width = 7, 
                                plotlyOutput("selected_plot")
                         ),
                         column(width = 5,
                                h4("Absolute Percent Error (%)"),
                                # Output: Table summarizing the values entered ----
                                dataTableOutput("APEvalues")
                         )
                     )
              )
            ),
            
            # Overall Evaluation for All CFGs----
            fluidRow(column(12,
                            box(title = "Overall Evaluation for All CFGs",
                                width = NULL, status = "primary", solidHeader = T,
                                p("Attainment Rate is the percentage of CFGs whose APEs are below 20% and higher is better. For MAPE and MAPE median, lower is better."),
                                p('"Selected" Model is selecting the model with the lowerst MAPE for each CFG during the Train Session.'),
                                
                                column(3,align = 'center'
                                ),
                                column(6,align = 'center',
                                       plotlyOutput('ErrorPlotRegion')
                                )
                                # column(6,plotlyOutput('ErrorPlotWeekly')
                                # )
                            )
            )
            )
          ),
          
          # Page 2----
          tabPanel(
            'Cross-Validation', id = 'Cross-Validation',
            
            tags$div(id = "mydiv",
                     br(),
                     br()
            ),
            
            # Select CFG, Region, and Overall Metrics on One Row----
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "info", solidHeader = T,
                         column(4,
                                uiOutput('filter1')
                         ),
                         column(4,
                                uiOutput('filter2')
                         )
                     )
              )
            ),
            
            
            fluidRow(
              column(12,
                     box(title = "Four Quarterly Forecast Result Comparison", width = 6, solidHeader = T,
                         status = "primary",
                         p('Each model is evaluated on a quarterly rolling forecasting.'),
                         p('At the beginning of FY17Q4, using FY17Q1-FY17Q3 weekly sales data to forecast FY18Q1 with one quarter ahead. At the beginning of FY18Q1, using FY17Q1-FY17Q4 weekly sales data to forecast FY18Q2 with one quarter ahead. The same for FY18Q3 and FY18Q4.'),
                         column(width = 12, align = 'center',
                                plotlyOutput("CV_plotQ1")),
                         column(width = 12, align = 'center',
                                plotlyOutput("CV_plotQ2")),
                         column(width = 12, align = 'center',
                                plotlyOutput("CV_plotQ3")),
                         column(width = 12, align = 'center',
                                plotlyOutput("CV_plotQ4"))
                     ),
                     column(6,
                            box(title = "Four Quarterly Forecast Error Comparison", width = 12, solidHeader = T,
                                status = "primary",
                                p('Note: for each row, darker orange colors indicate lower errors, which are preferred.'),
                                # Output: Table summarizing the values entered ----
                                column(width = 12, align = 'center',
                                       h4("Absolute Percent Error (%)"),
                                       dataTableOutput("CV_APEvalues"))
                                
                                # Output: Table summarizing the values entered ----
                                # column(width = 12, align = 'center',
                                #        h4("Weekly Mean APE (%)"),
                                #        dataTableOutput("CV_APEValues_week"))
                            )
                     )
              )
            )
          ),
          
          # Page 3----
          tabPanel(
            'Model Evaluation', id = 'Model-Evaluation',
            
            tags$div(id = "mydiv",
                     br(),
                     br()
            ),
            
            # Select Quarters or Mean
            fluidRow(
              column(width = 12,
                     box(width = NULL, status = "info", solidHeader = T,
                         column(4,
                                selectInput("QtrMean", 'MAPE Results: Quarters or Mean?', 
                                            choices = c('Quarters','Mean')),selected = 'Quarters'),
                         column(4,
                                uiOutput('filter3')),
                         column(4,
                                uiOutput('filter4'))
                     )
              )
            ),
            
            fluidRow(
              
              column(width = 8,
                     
                     box(title = "Cross-Validation Results: MAPE of Forecast Region (%)", width = 12, solidHeader = T,
                         status = "primary",
                         p("This page shows the evaluation results of Train Session, which is the quarterly cross-validation session."),
                         p('Note: for each row, darker orange colors indicate lower errors, which are preferred.'),
                         # footer = "* For each row, light green: the min; light blue: the 2nd min; blue: the 3rd min",
                         dataTableOutput("MAPE_CV.Table")
                     )
                     # box(title = "Cross-Validation Results: Weekly MAPE (%)", width = 12, solidHeader = T,
                     #     status = "primary",
                     #     footer = "* For each row, darker blue colors indicate lower errors, which are preferred.",
                     #     dataTableOutput("MAPE_CV_week.Table")
                     # )
              ),
              column(width = 4,
                     box(title = "Overall Evaluation for All CFGs", width = 12, solidHeader = T,
                         status = "primary",
                         p('Attainment Rate is the percentage of CFGs whose MAPEs are below 20% and higher is better. For MAPE and MAPE median, lower is better.'),
                         p('"Comb" Model is selecting the model with the lowerst MAPE for each CFG.'),
                         column(12,plotlyOutput('CV_ErrorPlotRegion')
                         )
                     )
                     # box(#collapsible = TRUE, collapsed = TRUE,# This does not work
                     #     title = "Overall Evaluation for All CFGs using Weekly errors", width = 12, solidHeader = T,
                     #     status = "primary", 
                     #     column(12,plotlyOutput('CV_ErrorPlotWeekly')
                     #     )
                     # )
              )
            ) 
          )# Page 3
        ) 
      )
    )
  ) # end of tagList
}


