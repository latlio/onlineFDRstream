################################################################################
# UI of the Shiny app
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:50:19 2020 ------------------------------
################################################################################
source("src/ui-mods.R")

ui <- shiny::fluidPage(
  includeCSS("src/styles.css"),
  shinyjs::useShinyjs(),
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
  useShinyalert(),
  useShinydashboard(),
  useShinydashboardPlus(),
  useShinyFeedback(),
  use_waiter(),
  use_sever(),
  waiter_show_on_load(html = tagList(spin_fading_circles(),
                                    "Initializing onlineFDR")),
  ####make the navbar pages####
  shiny::shinyUI(
    shiny::navbarPage(HTML(paste0("onlineFDR", tags$sub("stream"))),
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
  ) ##close shinyUI
) ## close fluid page
