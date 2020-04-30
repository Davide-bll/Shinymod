# filter panel 

##################################r 
# apply reactively some filter
##################################r

# # load functions----
# folder <- "C://Users//Utente//Desktop//shiny_modules//functions"
# source(paste(folder, "utils.R", sep = "//"))

# UI function----
#' Ui Module for filtering
#' @description @seealso \link{filterpanel}
#' @param id 
#'
#' @return A Ui module
#' @import dplyr
#' @export
#'
filterpanelUi <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    checkboxGroupInput(ns("flags"), label = "Choose action",
                       choices = c("filter", "select")),
    uiOutput(ns("action")),      # action button: apply the filters
    uiOutput(ns("params")),
    uiOutput(ns("values")),
    uiOutput(ns("excluded")),
    uiOutput(ns("recovery")))     # recovery the input data
  
}

# SERVER function----
#' Filter Shiny Module
#'
#' @param data A Dataframe to filter. Accept numerical and chr vars
#'
#' @return A reactive Dataframe
#' @import dplyr
#' @export
#'
#' @examples
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#' 
#'   filterpanelUi("prova"),
#' 
#'   mainPanel(tableOutput("tbl"))
#' 
#' )
#' 
#' server <- function(input, output, session) {
#' 
#'   dt <- callModule(filterpanel, "prova", data = CO2)
#' 
#'   output$tbl <- renderTable({
#' 
#'     dt()
#'   })
#' 
#' }
#' shinyApp(ui, server)
#' 
#'}
filterpanel <- function(input, output, session, data) {
  
  # detect types of data
  l_vars <- extract_types(data)
  
  # initialise filtered data ----
  new_data <- reactiveVal({ data })
  
  output$action <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$flags)) actionButton(ns("action"), "DO ACTION")
    
  })
  
  # initialize output
  observeEvent(input$action,{
    
    if("filter" %in%  input$flags){
      new_val <-  user_filter(new_data(), input$params, input$values)
      new_data(new_val)
    }
    
  })
  
  observeEvent(input$action, {
    
    vars <- input$excluded
    if(length(vars > 0)) {
      new_val <- new_data() %>% select(-vars)
      new_data(new_val)
    }
    
  })
  
  # generate UIs ----
  # choose the variable to which apply the filter
  
  # filter ui----
  output$params <- renderUI({
    ns <- session$ns
    
    # consider only num vars or date vars
    choices <- c(l_vars[["num_vars"]], l_vars[["date_vars"]], l_vars[["chc_vars"]])
    req(input$flags)
    test <- "filter" %in% input$flags
    if(test) {
      selectInput(ns("params"), "Choose var to apply filter", choices = choices)
    }
    
  })
  
  # reactive param
  params <- reactive({
    req(input$params)
    
    input$params
  })
  
  # select the range of params
  output$values <- renderUI({
    
    ns <- session$ns
    req(input$flags)
    test <- ("filter" %in% input$flags) & (!is.null(input$params))
    if(test) {
      vals <- new_data()[[params()]]
      if(is.numeric(vals) | is.datetime(vals)) {
        sliderInput(ns("values"), "Choose range" , min = min(vals), max = max(vals), value = c(min(vals), max(vals)))
        
      } else if(is.character(vals) | is.factor(vals)) {
        
        selectInput(ns("values"), "choose levels", choices = unique(vals), multiple = TRUE)
      }
    }
    
    
  })
  
  # select function  UI----
  output$excluded <- renderUI({
    ns <- session$ns
    
    req(input$flags)
    test <- "select" %in% input$flags
    
    if(test) {
      selectInput(ns("excluded"), "Select vars to exclude", choices = names(new_data()), multiple = TRUE)
    }
    
  })
  
  # recovery button -----
  #UI
  output$recovery <- renderUI({
    ns <- session$ns
    
    # if an action is made
    if(!is.null(input$flags)) {
      actionButton(ns("recovery"), "Recovery Data")
    }
  })
  
  # recovery observer
  observeEvent(input$recovery, {
    new_val <- data
    new_data(new_val)
  })
  
  return(new_data)
}

# example----

