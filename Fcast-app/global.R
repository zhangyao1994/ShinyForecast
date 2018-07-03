# Load library
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(gtable)
library(tidyverse)
library(ggthemes)
options(scipen = 999)
library(lubridate)
library(tidyquant)
library(plotly)
library(scales) # for percent
library(feather)
library(DT)
library(formattable)

# Load data: feather is faster!
# Test on the selected region: FY19W07-FY19W18
CFG_fcast.joined <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/CFG_fcast.feather")
APEresults <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/APE_values.feather")
Eval.results_fcastRegion <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/Eval.results_fcastRegion.feather")
Eval.results_wk <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/Eval.results_wk.feather")

# Cross-validation on different quarters
All_fcast_CV <- read_feather('~/GitHub/ShinyPractice/Fcast-app/data/All_fcast_cv.feather')
APEresults_CV <- read_feather('~/GitHub/ShinyPractice/Fcast-app/data/APE_values_CV.feather')

# Prepare the data table for Page 3
MAPE_CV.Table <- APEresults_CV %>% group_by(CFG,Region) %>% 
  summarise(Prophet=mean(APE_FcastRegion_Prophet,na.rm = TRUE),ARIMA=mean(APE_FcastRegion_ARIMA,na.rm = TRUE),TBATS=mean(APE_FcastRegion_TBATS,na.rm = TRUE),lm=mean(APE_FcastRegion_lm,na.rm = TRUE),RF=mean(APE_FcastRegion_RF,na.rm = TRUE),Xgboost=mean(APE_FcastRegion_Xgboost,na.rm = TRUE))

MAPE_CV_week.Table <- APEresults_CV %>% group_by(CFG,Region) %>% 
  summarise(Prophet=mean(MAPE_wk_Prophet,na.rm = TRUE),ARIMA=mean(MAPE_wk_ARIMA,na.rm = TRUE),TBATS=mean(MAPE_wk_TBATS,na.rm = TRUE),lm=mean(MAPE_wk_lm,na.rm = TRUE),RF=mean(MAPE_wk_RF,na.rm = TRUE),Xgboost=mean(MAPE_wk_Xgboost,na.rm = TRUE))

# Selections for selectInput
CFGgroups <- levels(factor(CFG_fcast.joined$CFG))
Regions <- levels(factor(CFG_fcast.joined$Region))
Models <- levels(factor(CFG_fcast.joined$Model))
Metrics <- levels(factor(Eval.results_wk$Results))

