################################################################################
# Server Modules
#
# Author: Lathan Liou
# Created: Thu Oct  1 11:50:56 2020 ------------------------------
################################################################################

# globaldata <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     stash <- reactiveValues()
#     stash$in_data <- reactive({
#       req(input$file)
#       
#       ext <- tools::file_ext(input$file$name)
#       validate(need(ext == "csv", "Please upload a csv file!"))
#       
#       data <- read_csv(input$file$datapath)
#     })
#     return(list(getdata = reactive(stash$in_data)))
#   })
# }

is.empty <- function(x) {
  return(length(x)==0)
}

LONDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  # Run LOND algorithm
  LONDres <- eventReactive(input$go, {
    
    #check parameters
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    dep = params() %>% filter(param == "dep") %>% pull(value) %>% as.logical()
    if(is.empty(dep)) {
      dep <- FALSE
      shiny::showNotification("Missing dep, using default value of FALSE", type = "warning")
    }
    random = params() %>% filter(param == "random") %>% pull(value) %>% as.logical()
    if(is.empty(random)) {
      random <- TRUE
      shiny::showNotification("Missing random, using default value of TRUE", type = "warning")
    }
    
    original = params() %>% filter(param == "original") %>% pull(value) %>% as.logical()
    if(is.empty(original)) {
      original <- TRUE
      shiny::showNotification("Missing original, using default value of TRUE", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    else {
      shiny::showNotification("Please upload a dataset")
    }
    out <- LOND(d = data(),
                alpha = alpha,
                random = random,
                original = original)
    shiny::removeModal()
    
    return(out)
  }) #close eventReactive
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # Output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch({
                   LONDres()
                 },
                 error = function(err){
                   shiny::showNotification(paste0(err), type = "err")
                 })
               }
  )

  #download results
  output$download <- downloadHandler(
    filename = function() {
      paste("LOND-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LONDres(), file)
    }
  )
  
  return(list(LONDres = LONDres))
}

LONDtableServer <- function(input, output, session, LONDresult) {
  output$table <- renderReactable({
    reactable(LONDresult$LONDres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(LONDresult$LONDres()$R[which(LONDresult$LONDres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LOND significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                         var total = 0
                         colInfo.data.forEach(function(row) {
                          total += row[colInfo.column.id]
                          })
                          return total
                                              }")
                )
              )
    ) #close reactable
  }) #close render reactable
}

LONDcountServer <- function(input, output, session, LONDresult) {
  output$count <- renderUI({
    data <- LONDresult$LONDres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

LONDplotServer <- function(input, output, session, LONDresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LONDresult$LONDres() %>%
      mutate(index = row_number(),
             LOND = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LOND, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

LORDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  # Run LORD algorithm
  LORDres <- eventReactive(input$go, {
    
    # if(is.null(data())){
    #   shiny::showNotification("Please upload a dataset", type = "err")
    # }
    
    #check parameters
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    version = params() %>% filter(param == "version") %>% pull(value)
    if(is.empty(version)) {
      version <- "++"
      shiny::showNotification("Missing version, using default value of ++", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.05/10
      shiny::showNotification("Missing w0, using default value of 0.005", type = "warning")
    }
    
    b0 = params() %>% filter(param == "b0") %>% pull(value) %>% as.numeric()
    if(is.empty(b0)) {
      b0 <- 0.045
      shiny::showNotification("Missing b0, using default value of 0.045", type = "warning")
    }
    
    tau.discard = params() %>% filter(param == "tau.discard") %>% pull(value) %>% as.numeric()
    if(is.empty(tau.discard)) {
      tau.discard <- 0.5
      shiny::showNotification("Missing tau.discard, using default value of 0.5", type = "warning")
    }
    
    random = params() %>% filter(param == "random") %>% pull(value) %>% as.logical()
    if(is.empty(random)) {
      random <- TRUE
      shiny::showNotification("Missing random, using default value of TRUE", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- LORD(d = data(),
                   alpha = alpha,
                   version = version,
                   w0 = w0,
                   b0 = b0,
                   tau.discard = tau.discard,
                   random = random)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  # Output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch({
                   LORDres()
                 },
                 error = function(err){
                   shiny::showNotification(paste0(err), type = "err")
                 })
               }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(LORDres = LORDres))
}

LORDtableServer <- function(input, output, session, LORDresult) {
  output$table <- renderReactable({
    reactable(LORDresult$LORDres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(LORDresult$LORDres()$R[which(LORDresult$LORDres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LORD significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LORDcountServer <- function(input, output, session, LORDresult) {
  output$count <- renderUI({
    data <- LORDresult$LORDres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

LORDplotServer <- function(input, output, session, LORDresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LORDresult$LORDres() %>%
      mutate(index = row_number(),
             LORD = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORD, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

SAFFRONServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  SAFFRONres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.05/2
      shiny::showNotification("Missing w0, using default value of 0.025", type = "warning")
    }
    
    lambda = params() %>% filter(param == "lambda") %>% pull(value) %>% as.numeric()
    if(is.empty(lambda)) {
      lambda <- 0.5
      shiny::showNotification("Missing lambda, using default value of 0.5", type = "warning")
    }
    
    random = params() %>% filter(param == "random") %>% pull(value) %>% as.logical()
    if(is.empty(random)) {
      random <- TRUE
      shiny::showNotification("Missing random, using default value of TRUE", type = "warning")
    }
    
    discard = params() %>% filter(param == "discard") %>% pull(value) %>% as.logical()
    if(is.empty(discard)) {
      discard <- FALSE
      shiny::showNotification("Missing discard, using default value of FALSE", type = "warning")
    }
    
    tau.discard = params() %>% filter(param == "tau.discard") %>% pull(value) %>% as.numeric()
    if(is.empty(tau.discard)) {
      tau.discard <- 0.5
      shiny::showNotification("Missing tau.discard, using default value of 0.5", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- SAFFRON(d = data(),
                      alpha = alpha,
                      w0 = w0,
                      lambda = lambda,
                      random = random,
                      discard = discard,
                      tau.discard = tau.discard)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch({
                   SAFFRONres()
                 },
                 error = function(err){
                   shiny::showNotification(paste0(err), type = "err")
                 })
               }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(SAFFRONres = SAFFRONres))
}

SAFFRONtableServer <- function(input, output, session, SAFFRONresult) {
  output$table <- renderReactable({
    reactable(SAFFRONresult$SAFFRONres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(SAFFRONresult$SAFFRONres()$R[which(SAFFRONresult$SAFFRONres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "SAFFRON significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

SAFFRONcountServer <- function(input, output, session, SAFFRONresult) {
  output$count <- renderUI({
    data <- SAFFRONresult$SAFFRONres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

SAFFRONplotServer <- function(input, output, session, SAFFRONresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- SAFFRONresult$SAFFRONres() %>%
      mutate(index = row_number(),
             SAFFRON = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRON, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

ADDISServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  ADDISres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.5*0.5*0.05/2
      shiny::showNotification("Missing w0, using default value of 0.00625", type = "warning")
    }
    
    lambda = params() %>% filter(param == "lambda") %>% pull(value) %>% as.numeric()
    if(is.empty(lambda)) {
      lambda <- 0.5
      shiny::showNotification("Missing lambda, using default value of 0.5", type = "warning")
    }
    
    tau = params() %>% filter(param == "tau") %>% pull(value) %>% as.numeric()
    if(is.empty(tau)) {
      tau <- 0.5
      shiny::showNotification("Missing tau, using default value of 0.5", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- ADDIS(d = data(),
                    alpha = alpha,
                    w0 = w0,
                    lambda = lambda,
                    tau = tau,
                    async = FALSE)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch({
                   ADDISres()
                 },
                 error = function(err){
                   shiny::showNotification(paste0(err), type = "err")
                 })
               }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(ADDISres = ADDISres))
}

ADDIStableServer <- function(input, output, session, ADDISresult) {
  output$table <- renderReactable({
    reactable(ADDISresult$ADDISres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(ADDISresult$ADDISres()$R[which(ADDISresult$ADDISres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "ADDIS significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

ADDIScountServer <- function(input, output, session, ADDISresult) {
  output$count <- renderUI({
    data <- ADDISresult$ADDISres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

ADDISplotServer <- function(input, output, session, ADDISresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- ADDISresult$ADDISres() %>%
      mutate(index = row_number(),
             ADDIS = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

ADDISaServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  ADDISres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.5*0.5*0.05/2
      shiny::showNotification("Missing w0, using default value of 0.00625", type = "warning")
    }
    
    lambda = params() %>% filter(param == "lambda") %>% pull(value) %>% as.numeric()
    if(is.empty(lambda)) {
      lambda <- 0.5
      shiny::showNotification("Missing lambda, using default value of 0.5", type = "warning")
    }
    
    tau = params() %>% filter(param == "tau") %>% pull(value) %>% as.numeric()
    if(is.empty(tau)) {
      tau <- 0.5
      shiny::showNotification("Missing tau, using default value of 0.5", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- ADDIS(d = data(),
                    alpha = alpha,
                    w0 = w0,
                    lambda = lambda,
                    tau = tau,
                    async = TRUE)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch({
                   ADDISres()
                 },
                 error = function(err){
                   shiny::showNotification(paste0(err), type = "err")
                 })
               }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(ADDISres = ADDISres))
}

alphainvestingServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  alphainvestingres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.05/2
      shiny::showNotification("Missing w0, using default value of 0.025", type = "warning")
    }
    
    random = params() %>% filter(param == "random") %>% pull(value) %>% as.logical()
    if(is.empty(random)) {
      random <- TRUE
      shiny::showNotification("Missing random, using default value of TRUE", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- Alpha_investing(d = data(),
                              alpha = alpha,
                              w0 = w0,
                              random = random)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch({
                   alphainvestingres()
                 },
                 error = function(err){
                   shiny::showNotification(paste0(err), type = "err")
                 })
               }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(alphainvestingres = alphainvestingres))
}

alphainvestingtableServer <- function(input, output, session, alphainvestingresult) {
  output$table <- renderReactable({
    reactable(alphainvestingresult$alphainvestingres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(alphainvestingresult$alphainvestingres()$R[which(alphainvestingresult$alphainvestingres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "Alpha_investing significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

alphainvestingcountServer <- function(input, output, session, alphainvestingresult) {
  output$count <- renderUI({
    data <- alphainvestingresult$alphainvestingres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

alphainvestingplotServer <- function(input, output, session, alphainvestingresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- alphainvestingresult$alphainvestingres() %>%
      mutate(index = row_number(),
             Alpha_investing = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(Alpha_investing, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

LONDSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  LONDSTARres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    version = params() %>% filter(param == "version") %>% pull(value)
    if(is.empty(version)) {
      version <- "async"
      shiny::showNotification("Missing version, using default value of async", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- myLONDstar(d = data(),
                         alpha = alpha,
                         version = version)
    shiny::removeModal()
    
    output
  })

  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch(
                   LONDSTARres(),
                   # warning = function(warn){
                   #   shiny::showNotification(paste0(warn), type = "warning")
                   # },
                   error = function(err){
                     shiny::showNotification(paste0(err), type = "err")
                   }
                   )
               }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(LONDSTARres = LONDSTARres))
}

LONDSTARtableServer <- function(input, output, session, LONDSTARresult) {
  output$table <- renderReactable({
    reactable(LONDSTARresult$LONDSTARres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(LONDSTARresult$LONDSTARres()$R[which(LONDSTARresult$LONDSTARres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LONDstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LONDSTARcountServer <- function(input, output, session, LONDSTARresult) {
  output$count <- renderUI({
    data <- LONDSTARresult$LONDSTARres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

LONDSTARplotServer <- function(input, output, session, LONDSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LONDSTARresult$LONDSTARres() %>%
      mutate(index = row_number(),
             LONDstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LONDstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

LORDSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  LORDSTARres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    version = params() %>% filter(param == "version") %>% pull(value)
    if(is.empty(version)) {
      version <- "async"
      shiny::showNotification("Missing version, using default value of async", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.05/10
      shiny::showNotification("Missing w0, using default value of 0.005", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    output <- myLORDstar(d = data(),
                         alpha = alpha,
                         version = version,
                         w0 = w0)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch(
                   LORDSTARres(),
                   # warning = function(warn){
                   #   shiny::showNotification(paste0(warn), type = "warning")
                   # },
                   error = function(err){
                     shiny::showNotification(paste0(err), type = "err")
                   }
                 )
               }
  )

  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(LORDSTARres = LORDSTARres))
}

LORDSTARtableServer <- function(input, output, session, LORDSTARresult) {
  output$table <- renderReactable({
    reactable(LORDSTARresult$LORDSTARres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(LORDSTARresult$LORDSTARres()$R[which(LORDSTARrsult$LORDSTARres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LORDstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LORDSTARcountServer <- function(input, output, session, LORDSTARresult) {
  output$count <- renderUI({
    data <- LORDSTARresult$LORDSTARres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

LORDSTARplotServer <- function(input, output, session, LORDSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LORDSTARresult$LORDsres() %>%
      mutate(index = row_number(),
             LORDstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORDstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}

SAFFRONSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  params <- reactive({
    req(input$paramfile)
    
    ext <- tools::file_ext(input$paramfile$name)
    validate(need(ext == "csv", "Please upload a csv file!"))
    
    out_params <- read_csv(input$paramfile$datapath)
  })
  
  SAFFRONSTARres <- eventReactive(input$go, {
    
    alpha = params() %>% filter(param == "alpha") %>% pull(value) %>% as.numeric()
    if(is.empty(alpha)) {
      alpha <- 0.05
      shiny::showNotification("Missing alpha, using default value of 0.05", type = "warning")
    }
    
    version = params() %>% filter(param == "version") %>% pull(value)
    if(is.empty(version)) {
      version <- "async"
      shiny::showNotification("Missing version, using default value of async", type = "warning")
    }
    
    w0 = params() %>% filter(param == "w0") %>% pull(value) %>% as.numeric()
    if(is.empty(w0)) {
      w0 <- 0.05/10
      shiny::showNotification("Missing w0, using default value of 0.005", type = "warning")
    }
    
    lambda = params() %>% filter(param == "lambda") %>% pull(value) %>% as.numeric()
    if(is.empty(lambda)) {
      lambda <- 0.5
      shiny::showNotification("Missing lambda, using default value of 0.5", type = "warning")
    }
    
    discard = params() %>% filter(param == "discard") %>% pull(value) %>% as.logical()
    if(is.empty(discard)) {
      discard <- FALSE
      shiny::showNotification("Missing discard, using default value of FALSE", type = "warning")
    }
    
    tau.discard = params() %>% filter(param == "tau.discard") %>% pull(value) %>% as.numeric()
    if(is.empty(tau.discard)) {
      tau.discard <- 0.5
      shiny::showNotification("Missing tau.discard, using default value of 0.5", type = "warning")
    }
    
    seed = params() %>% filter(param == "seed") %>% pull(value) %>% as.numeric()
    if(is.empty(seed)) {
      seed <- 1
      shiny::showNotification("Missing seed, using default value of 1", type = "warning")
    }
    
    set.seed(seed)
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Calculating..."))
    }
    
    output <- mySAFFRONstar(pval = data(),
                          alpha = alpha,
                          version = version,
                          w0 = w0,
                          lambda = lambda,
                          discard = discard,
                          tau.discard = tau.discard)
    shiny::removeModal()
    
    output
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    }
  })
  
  #output error messages
  observeEvent(input$go,
               if(is.null(data())){
                 shiny::showNotification("Please upload a dataset", type = "err")
               }
               else {
                 tryCatch(
                   SAFFRONSTARres(),
                   # warning = function(warn){
                   #   shiny::showNotification(paste0(warn), type = "warning")
                   # },
                   error = function(err){
                     shiny::showNotification(paste0(err), type = "err")
                   }
                 )
               }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(LORDres(), file)
    }
  )
  
  return(list(SAFFRONSTARres = SAFFRONSTARres))
}

SAFFRONSTARtableServer <- function(input, output, session, SAFFRONSTARresult) {
  output$table <- renderReactable({
    reactable(SAFFRONSTARresult$SAFFRONSTARres(),
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                if(SAFFRONSTARresult$SAFFRONSTARres()$R[which(SAFFRONSTARresult$SAFFRONSTARres()$pval == value)] == 1) {
                                  color <- "#008000"
                                }
                                else {
                                  color <- "#e00000"
                                }
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "SAFFRONstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

SAFFRONSTARcountServer <- function(input, output, session, SAFFRONSTARresult) {
  output$count <- renderUI({
    data <- SAFFRONSTARresult$SAFFRONSTARres()
    if(sum(data$R) == 1) {
      div(
        id = "test",
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        id = "test2",
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected"),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    }
  })
}

SAFFRONSTARplotServer <- function(input, output, session, SAFFRONSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- SAFFRONSTARresult$SAFFRONSTARres() %>%
      mutate(index = row_number(),
             SAFFRONstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRONstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
}
