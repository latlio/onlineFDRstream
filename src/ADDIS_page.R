################################################################################
# ADDIS (Synchronous) Page
#
# Author: Lathan Liou
# Created: Tue Sep 22 15:00:38 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                
                box(
                  title = strong("ADDIS (Synchronous)"),
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
                  ADDISUI("inputADDIS")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputADDIS"),
                                  summaryUI("ADDIScount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputADDIS"),
                                  plotUI("ADDISplot")),
                  shiny::tabPanel("Compare",
                                  compareUI("ADDIScomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/ADDIS_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row