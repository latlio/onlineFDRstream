################################################################################
# File upload page
#
# Author: Lathan Liou
# Created: Mon Sep 21 15:02:57 2020 ------------------------------
################################################################################
fluidPage(
  fluidRow(
    h1("User Guide"),
    bsplus::bs_accordion(id = "guide") %>%
      bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
      bs_append(title = "Introduction", content = HTML("This application is designed to allow users to interactively run procedures that control the False Discovery Rate (FDR) for online hypothesis testing. Source code and additional information for this application are available via <a href=\"https://github.com/dsrobertson/onlineFDRGitHub\">GitHub</a>.")) %>%
      bs_append(title = "Application usage", content = HTML("This application takes as input a dataset with the following labelled columns (id, date, and pval). Note that the column names should be spelled exactly like that. Dates should be specified in the following format: “yyyy-mm-dd”. <br> <br>

The application provides various algorithms you can use. When you click “Calculate”, a data frame containing the following columns will appear in the Data tab: id, date, pval, alphai, and R. alphai represents the adjusted significance threshold for each sequential test. Where R equals 1 is where a null hypothesis was rejected. The data frame is interactive and clicking on the column headers will sort them. <br> <br>

The Summary tab provides a graph plotting the log adjusted test levels for the specified algorithm relative to log unadjusted and Bonferroni-adjusted test levels. <br> <br>

The “Download” button will download a csv containing the results as displayed in the Data tab. <br> <br>

For more information, check out the <a href=\"https://dsrobertson.github.io/onlineFDR/\"<Get Started></a> page in our vignette.")) %>%
      bs_append(title = "Package dependencies & credits", p(
        "shinyWidgets",
        br(),
        "shinycssloaders",
        br(),
        "shinyjs",
        br(),
        "shinyBS",
        br(),
        "bsplus",
        br(),
        "shinyalert",
        br(),
        "shinyFeedback",
        br(),
        "shinydashboard",
        br(),
        "shinydashboardPlus",
        br(),
        "tippy",
        br(),
        "waiter",
        br(),
        "sever",
        br(),
        br(),
        "tidyverse",
        br(),
        "lubridate",
        br(),
        "knitr",
        br(),
        "reactable",
        br(),
        "highcharter",
        style = "font-family: 'Consolas'")
      ) %>%
      bs_append(title = "Help & feedback", content = HTML("For additional help or to submit feedback or bug reports,
       please contact: <br>
       David Robertson <br>
       MRC Biostatistics Unit <br>
       <a href=\"mailto:david.robertson@mrc-bsu.cam.ac.uk@gmail.com\">Email</a>"))
  ),
  fluidRow(
    prettyCheckbox("checkbox",
                   strong("I am a first time user"), 
                   value = FALSE,
                   shape = "curve",
                   fill = TRUE,
                   animation = "pulse",
                   icon = icon("check"),
                   status = "info")
  ),
  hidden(
    div(
    id = "novice",
    fluidRow(
      h1("Which algorithm do I use?"),
      "Use the following demo to inform which algorithm is most appropriate to use.
    Pick a sample size that is closest to the size of your data and a proportion of expected non-null
    hypotheses. Text will populate that reports which algorithm will have the highest power given
    your specified parameters. Please then proceed to upload your dataset as a CSV file.",
      br(),
      br(),
      column(4,
             strong("Sample size"),
             shinyWidgets::radioGroupButtons("size", NULL, c(50, 100, 1000))
      ),
      column(4,
             strong("Proportion"),
             shiny::sliderInput("prop", NULL, min = 0.1, max = 1, value = 0.5, step = 0.1),
             shinyBS::bsTooltip("prop",
                                "Proportion of true non-null hypotheses",
                                placement = "right",
                                trigger = "hover")
      ),
      column(4,
             strong("Bound status"),
             shinyWidgets::radioGroupButtons("bound", NULL, c("Bounded", "Unbounded"))
      )
    ),
    fluidRow(
      column(width = 12,
             align = "center",
             boxPlus(
               title = "Your recommended algorithm", 
               closable = FALSE, 
               width = NULL,
               status = "primary", 
               solidHeader = FALSE, 
               background = "aqua",
               collapsible = TRUE,
               enable_dropdown = TRUE,
               dropdown_icon = "wrench",
               dropdown_menu = dropdownItemList(
                 dropdownItem(url = "#", name = "More information"),
                 dropdownItem(url = "#", name = "User Flowchart"),
               ),
               p(textOutput("demores"))
             )
             )
      ),
      tags$head(tags$style("#demores{font-size: 18px;
                         font-family: Arial;
                         text-align: center;")),
    br(),
    textOutput("saffronwarn"),
    tags$head(tags$style("#saffronwarn{font-size: 14px;
                         font-family: Arial;
                         text-align: center;
                         color: red")),
    textOutput("addiswarn"),
    tags$head(tags$style("#addiswarn{font-size: 14px;
                         font-family: Arial;
                         text-align: center;
                         color: red"))
    ) #close div
  ), #close hidden
  br(),
  fluidRow(
    h1("Choose a CSV file"),
    fileInput("file", NULL,
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv'))
  )
)