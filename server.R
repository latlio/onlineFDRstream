################################################################################
# Server logic of the Shiny app
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:57:20 2020 ------------------------------
################################################################################
source("src/server-mods.R")

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
library(onlineFDR)

#for alg recommendation feature
demodata <- read_csv("powerFDRdata.csv") %>%
  mutate(pi.vec = round(pi.vec, 2))

#for hover functionality
with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

`%!in%` = Negate(`%in%`)

server <- function(input, output, session) {
  sever()
  Sys.sleep(0.5)
  waiter_hide()
  
  #Load in data
  in_data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    shiny::validate(need(ext %in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv'), 
      "Please upload a csv file!"))
    
    data <- read_csv(input$file$datapath) %>%
      dplyr::mutate(across(any_of("date"), ~as.Date(.x, format = "%m/%d/%y")))
  })
  
  #warning if wrong file type
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    if (ext %!in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv')) {
      shiny::showNotification("Your file format is not supported. Please upload a CSV file!", type = "err", 
                              duration = NULL)
    }
  })
  
  #### LOND ####
  
  LOND_result <- callModule(LONDServer, id = "inputLOND", data = in_data)
  callModule(LONDcountServer, "LONDcount", LOND_result)
  callModule(LONDplotServer, "LONDplot", LOND_result)
  callModule(LONDcompServer, "LONDcomp", LOND_result, data = in_data)
  
  #### LORD ####
  
  LORD_result <- callModule(LORDServer, id = "inputLORD", data = in_data)
  callModule(LORDcountServer, "LORDcount", LORD_result)
  callModule(LORDplotServer, "LORDplot", LORD_result)
  callModule(LORDcompServer, "LORDcomp", LORD_result, data = in_data)
  
  # gray out inputs conditionally
  shinyjs::onclick("advanced2",
                   shinyjs::toggle(id = "advanced2", anim = TRUE))
  
  #### SAFFRON ####
  SAFFRON_result <- callModule(SAFFRONServer, id = "inputSAFFRON", data = in_data)
  callModule(SAFFRONcountServer, "SAFFRONcount", SAFFRON_result)
  callModule(SAFFRONplotServer, "SAFFRONplot", SAFFRON_result)
  callModule(SAFFRONcompServer, "SAFFRONcomp", SAFFRON_result, data = in_data)
  
  #### ADDIS Sync ####
  ADDIS_result <- callModule(ADDISServer, id = "inputADDIS", data = in_data)
  callModule(ADDIScountServer, "ADDIScount", ADDIS_result)
  callModule(ADDISplotServer, "ADDISplot", ADDIS_result)
  callModule(ADDIScompServer, "ADDIScomp", ADDIS_result, data = in_data)
  
  #### Alpha-Investing ####
  alphainvesting_result <- callModule(alphainvestingServer, id = "inputalphainvesting", data = in_data)
  callModule(alphainvestingcountServer, "alphainvestcount", alphainvesting_result)
  callModule(alphainvestingplotServer, "alphainvestplot", alphainvesting_result)
  callModule(alphainvestingcompServer, "alphainvestcomp", alphainvesting_result, data = in_data)
  
  #### ADDIS Async ####
  ADDISa_result <- callModule(ADDISServer, id = "inputADDISa", data = in_data)
  callModule(ADDIScountServer, "ADDISacount", ADDIS_result)
  callModule(ADDISplotServer, "ADDISaplot", ADDIS_result)
  callModule(ADDIScompServer, "ADDISacomp", ADDIS_result, data = in_data)
  
  #### LONDstar ####
  LONDSTAR_result <- callModule(LONDSTARServer, id = "inputLONDSTAR", data = in_data)
  callModule(LONDSTARcountServer, "LONDSTARcount", LONDSTAR_result)
  callModule(LONDSTARplotServer, "LONDSTARplot", LONDSTAR_result)
  callModule(LONDSTARcompServer, "LONDSTARcomp", LONDSTAR_result, data = in_data)
  
  #### LORDstar ####
  LORDSTAR_result <- callModule(LORDSTARServer, id = "inputLORDSTAR", data = in_data)
  callModule(LORDSTARcountServer, "LORDSTARcount", LORDSTAR_result)
  callModule(LORDSTARplotServer, "LORDSTARplot", LORDSTAR_result)
  callModule(LORDSTARcompServer, "LORDSTARcomp", LORDSTAR_result, data = in_data)
  
  #### SAFFRONstar ####
  SAFFRONSTAR_result <- callModule(SAFFRONSTARServer, id = "inputSAFFRONSTAR", data = in_data)
  callModule(SAFFRONSTARcountServer, "SAFFRONSTARcount", SAFFRONSTAR_result)
  callModule(SAFFRONSTARplotServer, "SAFFRONSTARplot", SAFFRONSTAR_result)
  callModule(SAFFRONSTARcompServer, "SAFFRONSTARcomp", SAFFRONSTAR_result, data = in_data)
  
  #### get started page ####
  observe({
    toggle(id = "novice", condition = input$checkbox)
  })
  
  filter_data <- reactive({
    size = as.numeric(input$size)
    boundstat = ifelse(input$bound == "Bounded", 1, 0)
    out <- demodata %>%
      filter(n == size,
             bound == boundstat,
             pi.vec == input$prop) %>%
      select(-c(pi.vec, n, bound)) %>%
      arrange(desc(power))
  })
  
  output$demores <- renderText({
    paste(filter_data() %>%
            head(1) %>%
            pull(procedure), "has the highest power.")
  })
  
  # output$saffronwarn <- renderText({
  #   if(input$size == 1000 & input$prop > 0.5) {
  #     paste("Using SAFFRON may overestimate the FDR.")
  #   }
  # })
  
  output$addiswarn <- renderText({
    if(input$size == 100 & input$prop == 0.4 | 
       input$size == 1000 & input$prop < 0.5 & input$prop > 0.2) {
      paste("Using ADDIS on a dataset > 100,000 may be too slow. Using onlineFDR::ADDIS() is recommended. ")
    }
  })
}
