################################################################################
# Server logic of the Shiny app
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:57:20 2020 ------------------------------
################################################################################
source("src/server-mods.R")
demodata <- read_csv("powerFDRdata.csv")

with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

server <- function(input, output, session) {
  sever()
  Sys.sleep(0.5)
  waiter_hide()
  
  #Load in data
  in_data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv", "Please upload a csv file!"))

    data <- read_csv(input$file$datapath) %>%
      dplyr::mutate(date = as.Date(.$date, format = "%m/%d/%y"))
  })
  
  #### LOND ####
  
  LOND_result <- callModule(LONDServer, id = "inputLOND", data = in_data)
  callModule(LONDcountServer, "LONDcount", LOND_result)
  callModule(LONDplotServer, "LONDplot", LOND_result)

  #### LORD ####
  
  LORD_result <- callModule(LORDServer, id = "inputLORD", data = in_data)
  callModule(LORDcountServer, "LORDcount", LORD_result)
  callModule(LORDplotServer, "LORDplot", LORD_result)
  
  #### SAFFRON ####
  SAFFRON_result <- callModule(SAFFRONServer, id = "inputSAFFRON", data = in_data)
  callModule(SAFFRONcountServer, "SAFFRONcount", SAFFRON_result)
  callModule(SAFFRONplotServer, "SAFFRONplot", SAFFRON_result)
  
  #### ADDIS Sync ####
  ADDIS_result <- callModule(ADDISServer, id = "inputADDIS", data = in_data)
  callModule(ADDIScountServer, "ADDIScount", ADDIS_result)
  callModule(ADDISplotServer, "ADDISplot", ADDIS_result)
  
  #### Alpha-Investing ####
  alphainvesting_result <- callModule(alphainvestingServer, id = "inputalphainvesting", data = in_data)
  callModule(alphainvestingcountServer, "alphainvestcount", alphainvesting_result)
  callModule(alphainvestingplotServer, "alphainvestplot", alphainvesting_result)
  
  #### ADDIS Async ####
  ADDISa_result <- callModule(ADDISServer, id = "inputADDISa", data = in_data)
  callModule(ADDIScountServer, "ADDISacount", ADDIS_result)
  callModule(ADDISplotServer, "ADDISaplot", ADDIS_result)
  
  #### LONDstar ####
  LONDSTAR_result <- callModule(LONDSTARServer, id = "inputLONDSTAR", data = in_data)
  callModule(LONDSTARcountServer, "LONDSTARcount", LONDSTAR_result)
  callModule(LONDSTARplotServer, "LONDSTARplot", LONDSTAR_result)
  
  #### LORDstar ####
  LORDSTAR_result <- callModule(LORDSTARServer, id = "inputLORDSTAR", data = in_data)
  callModule(LORDSTARcountServer, "LORDSTARcount", LORDSTAR_result)
  callModule(LORDSTARplotServer, "LORDSTARplot", LORDSTAR_result)
  
  #### SAFFRONstar ####
  SAFFRONSTAR_result <- callModule(SAFFRONSTARServer, id = "inputSAFFRONSTAR", data = in_data)
  callModule(SAFFRONSTARcountServer, "SAFFRONSTARcount", SAFFRONSTAR_result)
  callModule(SAFFRONSTARplotServer, "SAFFRONSTARplot", SAFFRONSTAR_result)
  
  #get started page
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
      arrange(dplyr::desc(power))
  })
  
  output$demores <- renderText({
  paste(filter_data() %>%
          head(1) %>%
          pull(procedure), "has the highest power.")
  })
  
  output$saffronwarn <- renderText({
    if(input$size == 100 & input$prop < 0.5 | input$size == 1000 & input$prop < 0.5){
      paste("Note: Using SAFFRON may overestimate the FDR.")
    }
  })
  
  output$addiswarn <- renderText({
    if(input$size == 100 & input$prop == 0.4 | 
       input$size == 1000 & input$prop < 0.5 & input$prop > 0.2) {
      paste("Using ADDIS on a dataset > 100,000 may be too slow. Using onlineFDR::ADDIS() is recommended. ")
    }
  })
}
