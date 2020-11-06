################################################################################
# SAFFRON Page
#
# Author: Lathan Liou
# Created: Tue Sep 22 14:09:34 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                box(
                  title = strong("SAFFRON"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$style(HTML("
.box.box-solid.box-primary>.box-header {
  color:#ffffff;
  background:#266EAB
                    }

.box.box-solid.box-primary{
border-bottom-color#ffffff;
border-left-color:#F7F7F7;
border-right-color:#F7F7F7;
border-top-color:#ffffff;
}
                                    ")),
                  width = 12,
                  SAFFRONUI("inputSAFFRON")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputSAFFRON"),
                                  summaryUI("SAFFRONcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputSAFFRON"),
                                  plotUI("SAFFRONplot")),
                  shiny::tabPanel("Compare"),
                  shiny::tabPanel("Code", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("./src/SAFFRON_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row