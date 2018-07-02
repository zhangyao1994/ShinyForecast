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

# Start the clock!
ptm <- proc.time()

# Load data: feather is faster!
# load("~/GitHub/ShinyPractice/Fcast-app/data/data.RData")
CFG_fcast.joined <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/CFG_fcast.feather")
APEreslts <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/APE_values.feather")
Eval.results_fcastRegion <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/Eval.results_fcastRegion.feather")
Eval.results_wk <- read_feather("~/GitHub/ShinyPractice/Fcast-app/data/Eval.results_wk.feather")

All_fcast_CV <- read_feather('~/GitHub/ShinyPractice/Fcast-app/data/All_fcast_cv.feather')
APEreslts_CV <- read_feather('~/GitHub/ShinyPractice/Fcast-app/data/APE_values_CV.feather')

# Selections for selectInput
CFGgroups <- levels(factor(CFG_fcast.joined$CFG))
Regions <- levels(factor(CFG_fcast.joined$Region))
Models <- levels(factor(CFG_fcast.joined$Model))
Metrics <- levels(factor(Eval.results_wk$Results))

# Stop the clock
proc.time() - ptm
