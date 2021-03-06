################################################################################
# Alpha_investing page
#
# Author: Lathan Liou
# Created: Wed Oct 14 16:52:11 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                
                box(
                  title = strong("Alpha-Investing"),
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
                  alphainvestingUI("inputalphainvesting")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  placeholderUI("inputalphainvesting"),
                                  summaryUI("alphainvestcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputalphainvesting"),
                                  plotUI("alphainvestplot")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/alphainvesting_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row