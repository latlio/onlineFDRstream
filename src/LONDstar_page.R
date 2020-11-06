################################################################################
# LONDstar Page
#
# Author: Lathan Liou
# Created: Wed Sep 30 14:25:20 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                
                box(
                  title = strong("LONDstar"),
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
                  LONDSTARUI("inputLONDSTAR")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputLONDSTAR"),
                                  summaryUI("LONDSTARcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputLONDSTAR"),
                                  plotUI("LONDSTARplot")),
                  shiny::tabPanel("Compare"),
                  shiny::tabPanel("Code", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("./src/LONDstar_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row