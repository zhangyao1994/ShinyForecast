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

# Load data 
load("~/GitHub/ShinyPractice/Fcast-app/data/data.RData")
