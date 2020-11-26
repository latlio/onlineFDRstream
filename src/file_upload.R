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
      bs_append(title = "Application usage", content = p(
        img(src = "user-diagram.png"),
        br(),
        "For more information, check out the", a(href = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html", "Get Started"), "page in our vignette."))  %>%
      bs_append(title = "How to upload your parameters", content = p(
        "Because the stream app is designed to be used as your dataset grows, you should have a CSV file of your parameters such that you can reuse them any time you want to run your algorithm of choice. Your CSV file should have two columns named 'param' and 'value'. You can specify all of your parameters, as in Example A. Bound represents the number of hypotheses you expect to test and corresponds to the betai/gammai argument in the algorithms. Not specfiying a bound will run the default unbounded procedure. You can specify no other parameter except your bound value, as in Example B, and default parameters will be used for your algorithm of choice (see Help). You can selectively specify certain parameters, as in Example C. If you do not specify a seed, a default seed value of 1 is used. Ensure that your params are spelled exactly as the arguments for your algorithm of choice (see Help).",
        img(src = "param-diagram.png")
      )) %>%
      bs_append(title = "Synchronous vs Asynchronous", content = p(
        "Synchronous algorithms are designed to be used for a sequence of tests in which each test can only start when the previous test has finished. Asynchronous algorithms are designed to be used when tests overlap in time. The asynchronous setting may be more realistic since tests are often performed to overlap to gain time efficiency and because of difficulties of coordination in a large-scale, decentralized setting.",
        img(src = "sync-diagram.png")
      )) %>%
      bs_append(title = "Help & feedback", content = HTML("For additional help or to submit feedback or bug reports,
       please contact: <br>
       David Robertson <br>
       MRC Biostatistics Unit <br>
       <a href=\"mailto:david.robertson@mrc-bsu.cam.ac.uk@gmail.com\">Email</a>"))
  ), #close fluidrow
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
  shinyjs::hidden(
    div(
      id = "novice",
      fluidRow(
        h1("Which algorithm do I use?"),
        "Use the following demo to inform which algorithm is most appropriate to use.
    Pick a sample size that is closest to the size of your data and a proportion of expected non-null
    hypotheses. Text will populate that reports which algorithm will have the highest power given
    your specified parameters. Please then proceed to upload your dataset as a CSV file.",
        p("For more information, use the", a(href = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html#which-function-do-i-use-", "flowchart"), "to help determine which algorithm to use."),
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
               align = "right",
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
                   dropdownItem(url = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html", name = "More information"),
                   dropdownItem(url = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html#which-function-do-i-use-", name = "User Flowchart"),
                 ),
                 p(textOutput("demores"))
               )
        )
      ),
      tags$head(tags$style("#demores{font-size: 18px;
                         font-family: Arial;
                         text-align: center;")),
      br(),
      # textOutput("saffronwarn"),
      # tags$head(tags$style("#saffronwarn{font-size: 14px;
      #                    font-family: Arial;
      #                    text-align: center;
      #                    color: red")),
      textOutput("addiswarn"),
      tags$head(tags$style("#addiswarn{font-size: 14px;
                         font-family: Arial;
                         text-align: center;
                         color: red"))
      
    ) #close div
  ), #close hidden
  br(),
  fluidRow(
    h1("Upload your dataset"),
    p("Ensure that your CSV file contains at the minimum, a column of p-values with the name 'pval'. If you're including dates, ensure that they are in the format YYYY-MM-DD. "),
    fileInput("file", NULL,
              multiple = FALSE,
              accept = c('text/csv', 
                         'text/comma-separated-values',
                         'text/plain',
                         '.csv'))
  )
) #close fluidpage