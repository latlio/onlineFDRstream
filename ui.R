################################################################################
# UI of the Shiny app
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:50:19 2020 ------------------------------
################################################################################
source("src/ui-mods.R")

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
library(shinydashboard)
library(shinydashboardPlus)
# library(shinyanimate)

# 2. Data Manipulation
library(tidyverse)
library(dplyr)
library(lubridate)
# library(reactable)

#make sure github dev version is installed
# devtools::install_github("https://github.com/dsrobertson/onlineFDR")
library(StanHeaders)
library(onlineFDR)

ui <- shiny::fluidPage(
  tagList(
    includeCSS("src/styles.css"),
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus(),
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    sever::use_sever(),
    waiter::waiter_show_on_load(html = tagList(waiter::spin_fading_circles(),
                                               "Initializing onlineFDRExplore")),
    tags$head(
      tags$script(src = "src/JSModule.js"),
      tags$style(HTML("
                    @import url('//fonts.googleapis.com/css2?family=Poppins:wght@300');"),
                 HTML("
                    @import url('//fonts.googleapis.com/css2?family=Lato:wght@400');"),
                 ".bttn { vertical-align: middle; height: 30px; width: 100%; font-size: 12px; font-family: Poppins, sans-serif;}",
                 ".panel-group {font-family: Lato, sans-serif; font-size: 14px;} ",
                 ".h1 {font-family: Lato;}",
                 ".p {font-family: Lato;}")
    ),
    ####make the navbar pages####
    shiny::navbarPage(HTML(paste0("onlineFDR", tags$sub("explore"))),
                      shiny::tabPanel("Get Started",
                                      source("src/file_upload.R")$value),
                      shiny::navbarMenu("Synchronous",
                                        shiny::tabPanel("LOND",
                                                        source("src/LOND_page.R")$value), #close tabPanel
                                        shiny::tabPanel("LORD",
                                                        source("src/LORD_page.R")$value), #close tabPanel
                                        shiny::tabPanel("SAFFRON",
                                                        source("src/SAFFRON_page.R")$value), #close tabPanel
                                        shiny::tabPanel("ADDIS",
                                                        source("src/ADDIS_page.R")$value), #close tabPanel
                                        shiny::tabPanel("Alpha-investing",
                                                        source("src/Alpha_investing_page.R")$value), #close
                                        tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }")
                      ),# close navbarMenu
                      
                      shiny::navbarMenu("Asynchronous",
                                        shiny::tabPanel("LONDstar",
                                                        source("src/LONDstar_page.R")$value), #close
                                        shiny::tabPanel("LORDstar",
                                                        source("src/LORDstar_page.R")$value), #close
                                        shiny::tabPanel("SAFFRONstar",
                                                        source("src/SAFFRONstar_page.R")$value), #close
                                        shiny::tabPanel("ADDIS",
                                                        source("src/ADDIS_async_page.R")$value), #close tabPanel
                                        tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }")
                      ), #close navbarmenu
                      shiny::tabPanel("About",
                                      source("src/about_page.R")$value)
    ) ##close navbarpage
  ) ## close taglist
) ## close fluid page
