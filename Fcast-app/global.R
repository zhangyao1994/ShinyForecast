# Updated on 06/28/2018 # I want to make it better-looking!!!

# Load library
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggthemes)
options(scipen = 999)
library(lubridate)
library(tidyquant)
library(plotly)
library(scales) # for percent

# Load data for 12-week forecast
CFG_fcast.joined <- readRDS('CFG_fcast_joined.rds')
# Renaming the factors helps with the legend order.
Modelnames <- levels(as.factor(CFG_fcast.joined$Model))
NewModelNames <- c("3 ARIMA","1 Truth","8 MRP_Fcast","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost")
len_Model <- length(Modelnames)
for (i_Model in 1:len_Model){
  CFG_fcast.joined$Model <- replace(CFG_fcast.joined$Model, which(Modelnames[i_Model]==CFG_fcast.joined$Model), NewModelNames[i_Model])
}
# Load APE values
APEreslts <- readRDS('APEreslts.rds')

# Load data for Cross-Validation forecast
All_fcast_CV <- readRDS('All_fcast_CV.rds')
Modelnames <- levels(as.factor(All_fcast_CV$Model))
NewModelNames <- c("3 ARIMA","4 Prophet","7 TBATS","6 LinearModel","5 RandomForest","2 Xgboost","1 Truth")
len_Model <- length(Modelnames)
for (i_Model in 1:len_Model){
  All_fcast_CV$Model <- replace(All_fcast_CV$Model, which(Modelnames[i_Model]==All_fcast_CV$Model), NewModelNames[i_Model])
}

# Load data for Overall Errors
ErrorResults4plot <- readRDS('ErrorResults4plot.rds')

# Selections for selectInput
ResultsNames <- c('Attainment_Rates_Americas','MAPE_Americas','MAPE_median_Americas','Attainment_Rates_APJ','MAPE_APJ','MAPE_median_APJ','Attainment_Rates_EMEA','MAPE_EMEA','MAPE_median_EMEA')
CFGgroups <- levels(factor(CFG_fcast.joined$CFG))
Regions <- levels(factor(CFG_fcast.joined$Region))
Models <- levels(factor(CFG_fcast.joined$Model))

# Run the app ----
# shinyApp(ui = ui, server = server)