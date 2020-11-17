################################################################################
# UI modules
#
# Author: Lathan Liou
# Created: Thu Oct  1 09:52:20 2020 ------------------------------
################################################################################

#### BUILDING BLOCKS -- DOESNT WORK CURRENTLY ####
alphaUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  uiOutput(ns("alpha"))
}

depUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_dep"),
                    "Dependent:"),
        shinyWidgets::switchInput(ns("dep"), 
                                  NULL,
                                  value = FALSE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px"),
        shinyBS::bsTooltip(ns("label_dep"),
                           "Your p-values are dependent.",
                           placement = "right",
                           trigger = "hover"))
  )
}

randomUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px"),
        shinyBS::bsTooltip(ns("label_random"),
                           "The order of p-values in each batch of experiments (those that
                            have the same date) is randomized.",
                           placement = "right",
                           trigger = "hover"))
  )
}

originalUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Original:"),
        shinyWidgets::switchInput(ns("original"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False",
                                  width = "80px"))
  )
}

calculateUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(  
    div(style="display: inline-block;vertical-align:top; width: 100px;",
        shiny::actionButton(ns("go"),
                            "Calculate"))
  )
}

downloadUI <- function(id, label = NULL) {
  ns <- NS(id)
  
  tagList(
    div(style="display: inline-block;vertical-align:top; width: 100px;",
        shiny::actionButton(ns("init"), "Download", icon = icon("download")),
        shiny::downloadButton(ns("download"), "Download", style = "visibility: hidden;")
    )
  )
}

#### ALGORITHM INPUT UI ####
LONDUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

LORDUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

SAFFRONUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

ADDISUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

alphainvestingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

LONDSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

LORDSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

SAFFRONSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    fileInput(ns("paramfile"), "Please upload a CSV file of your parameters",
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    )
  )
}

#### OTHER UI ####
tableUI <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("table"))
}

summaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    # shinyanimate::withAnim(),
    shinyjs::hidden(
      div(
        id = ns("downloadbutton"),
        uiOutput(ns("count"))
      ) #close div
    )
  )
}

compareUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    p("Make sure you already clicked Calculate. Clicking the 'Compare' button compares the results of two algorithms. Parameters for the selected algorithm are assumed to be same as those for the current algorithm, otherwise they are set to the default values (see Help)."),
    column(width = 12,
           align = "center",
           div(style = "display: inline-block;vertical-align:top;text-align:center",
               strong("Pick an algorithm for comparison"),
               shiny::selectInput(ns("alg"), NULL, c("LOND", "LORD", "LORD3", "LORDdiscard", "LORDdep", "SAFFRON", "ADDIS")))),
    shinyWidgets::actionBttn(
      inputId = ns("compare"),
      label = "Compare",
      style = "fill",
      color = "primary"
    ),
    br(),
    plotlyOutput(ns("comp")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    br(),
    uiOutput(ns("compnum"))
  ) #close taglist
}

plotUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("plot")) %>%
    shinycssloaders::withSpinner(type = 6,
                                 color = "#0066CC")
}

set_html_breaks <- function(n) {
  HTML(strrep(br(), n))
}

placeholderUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder"), 
      set_html_breaks(10),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px")
  )
}

placeholder2UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder2"), 
      set_html_breaks(10),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px")
  )
}
