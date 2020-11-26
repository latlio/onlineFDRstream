################################################################################
# A Shiny App using the onlineFDR package: Stream
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:58:34 2020 ------------------------------
################################################################################

#clear before deplyment
rm(list = ls())

#change file input
options(shiny.maxRequestSize = 500*1024^2)

# 1. Shiny ----
library(shiny)
library(shinyWidgets) #custom widgets, allows for shinydashboard elements
library(shinycssloaders) #custom loading icons
library(shinyjs) #improved user exp
library(shinyBS) #custom widgets
library(bsplus)
# library(shinyalert) 
library(shinyFeedback) #for user feedback messages
library(tippy) #for hovers
# library(highcharter) #for animated plots
library(plotly)
library(waiter) #for loading screen
library(sever) #for waiting screen
library(knitr)
library(rmarkdown)
library(markdown)
library(shinydashboard)
library(shinydashboardPlus)

# 2. Data Manipulation
library(tidyverse)
library(lubridate)
# library(reactable)

#make sure github dev version is installed
# devtools::install_github("https://github.com/dsrobertson/onlineFDR")
# library(StanHeaders)
library(onlineFDR)

source("ui.R")
source("server.R")

# runApp(shinyApp(ui, server), launch.browser = TRUE)

