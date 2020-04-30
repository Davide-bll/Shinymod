# load functions -----
# folder <- "C://Users//Utente//Desktop//shiny_modules//functions"

# source(paste(folder, "utils.R", sep = "//"))
# source(paste(folder, "graphics.R", sep = "//"))
# source(paste(folder, "datatransformation.R", sep = "//"))
# source(paste(folder, "functioncalls.R", sep = "//"))

# univariate testing and data transformation-----

# create UI module
#' Data transformation module
#' @description Ui module for simulating data and apply to them transformation. 
#' @param id 
#'
#' @return Ui Module
#' @import dplyr
#' @export
#'
dataTransformUi <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    sidebarPanel(
      textInput(ns("sim_data"), label = "Generate Data, use R-syntax"),
      textInput(ns("formula"),  label = "Custom formula, es: x^2, NA if not used"),
      checkboxInput(ns("man"), "Add Manual transformation"),
      uiOutput(ns("man_t")),
      actionButton(ns("generate"), "Generate") 
      
    ),
    mainPanel(htmlOutput(ns("test")), 
              plotOutput(ns("n_plot")))
  )
}

# server logic
#' DataTransform server function
#'
#'
#' @return A Server function
#' @import dplyr
#' @export
#'
#' @examples
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#'   dataTransformUi("prova"))
#' 
#' server <- function(input, output, session) {
#'   callModule(dataTransform, "prova")
#' }
#' 
#' shinyApp(ui, server )
#'}
dataTransform <- function(input, output, session) {

  # reactive list of data
  data <- reactiveVal()
  
  # simulated data
  sim_data <- reactive({
    req(input$sim_data) 
    parse(text = input$sim_data)
  })
  
  # additional formula
  formula <- reactive({
    req(input$formula)
    input$formula
  })
  
  # observer for data generation
  observeEvent(input$generate, {
    # old data
    x <- eval(sim_data())  
    add_formula <- if_else(formula() == "NA", FALSE, TRUE)
    if(add_formula) x <- eval(parse(text = formula()))  
    
    # list results
    res <- list(old = x, BoxCox = my_boxcox(x))
    
    if(input$man) {
      man <- eval(parse(text = input$man_t))
      res <- c(res, list(manual = man))
    }
    
    data(res)
    
  })
  
  # additional UI 
  output$man_t <- renderUI({
    ns <- session$ns
    req(input$man)
    
    if(input$man) {
      textInput(ns("man_t"), "Create your own transformation, use R-syntax")
    }
  })
  
  # text results-----
  output$test <- renderText({
    # p value of shapiro test
    # shapiro test
    req(data())
    
    
    st_old <- shapiro.test(data()[["old"]])
    st_bc <- shapiro.test(data()[["BoxCox"]][["transformed"]])
    pval_old <- st_old[["p.value"]] %>% round(digits = 2)
    pval_bc <- st_bc[["p.value"]] %>% round(digits = 2)
    l <- data()[["BoxCox"]][["lambda"]][["lambda"]] %>% round(digits = 2)
    
    res <- paste("<p> Shapiro test P-values </p>", 
                 "<p> <b>original data </b> :</p>", pval_old,
                 "<p> <b>Box Cox </b> transformed data:</p>", pval_bc, 
                 "<p> lambda choosen is  ", l, "</p>", sep = " ")
    
    if("manual" %in% names(data())) {
      
      st_man <- shapiro.test(data()[["manual"]])
      pval_man <- st_man[["p.value"]] %>% round(digits = 2)
      
      res <- paste(res, "<p> <b>Manual transformed  </b> data:", pval_man, sep = " ")
      
    }
    
    res
  })
  
  # plots results----
  output$n_plot <- renderPlot({
    req(data())
    
    titles <- list(scatter = "Values", 
                   hist = "Histogram ", qqnorm = "Theoretical vs Sample quantiles")
    
    # select data to plot
    datalist <- map(data(), look_for, look = "transformed")
    
    # fix titles according to data
    titles_list <- map(names(datalist), ~map(titles,  paste, .x)) %>% 
      setNames(nm = names(datalist))
    
    # generate plot list
    plots <-  map2(.x = datalist, 
                   .y = titles_list, 
                   ~normal_plots(x = .x, title_list = .y)) %>% setNames(nm = names(datalist))
    
    # create grid of plots
    plots_vec <- unlist(plots, recursive = FALSE)
    expandCall(x = list(nrow = length(plots)), f = "gridExtra::grid.arrange", plots_vec)
  })
  
}

