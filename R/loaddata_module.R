# LOAD DATA SHINY ---------

# Module UI CSV files Input
#' Ui function foa loading csv Data 
#'
#' @param id Id module
#'
#' @return UI module
#' @import shiny
#' @export
#'
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )), 
    selectInput(ns("sep"), "sep", c(
      "comma"     = ",", 
      "semicolon" = ";", 
      "space"     = " "
    ))
  )
}




# Module server function: csv 
#' Server function of Load - data CSV
#' @description @seealso \link{csvFileInput}
#' @param stringsAsFactors FALSE by default.
#'
#' @return A reactive Dataframe
#' @import shiny utils
#' @export
#'
csvFile <- function(input, output, session, stringsAsFactors = FALSE) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote  = input$quote,
             sep    = input$sep,
             stringsAsFactors = stringsAsFactors)
  })
  
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

# proviamo con i file txt
# Module UI function
#'UI:Txt Input 
#'
#' @param id 
#' @return UI Module
#' @import shiny
#' @export 
#'
txtFileInput <- function(id, label = "TXT file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

# Module server function
#' Server function for loading txt files
#' @param stringsAsFactors FALSE by default
#'
#' @return A A reactiv dataframe
#' @import utils shiny
#' @export
#'
txtFile <- function(input, output, session, stringsAsFactors = FALSE) {
  # The selected file, if any
  userFile <- reactive({
    
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    validate(need(userFile(), message = FALSE))
    
    
    read.table(userFile()$datapath,
               header = input$heading,
               quote = input$quote,
               stringsAsFactors = stringsAsFactors, 
               fill = TRUE, 
               row.names = NULL)
  })
  
  
  # Return the reactive that yields the data frame
  return(dataframe)
}


# read feather fie
#' Load Feather file in shiny
#'
#' @param id 
#'
#' @return A UI module
#' @import shiny
#' @export
#'
featherFileInput <- function(id, label = "FEATHER format") {
  
  ns <- NS(id)
  
  
  tagList(
    fileInput(ns("file"), label)
  )
  

}

# server function
#'Server function of  load Feather data module
#'
#' @return A Reactive Dataframe
#' @import shiny
#' @export
#'
featherFile <- function(input, output, session) {
  inputfile <- reactive({
    
    validate(need(input$file, message = "Please choose a file"))
    
    input$file
  })
  
  dataframe <- reactive({
    
    feather::read_feather(inputfile()$datapath)
  })
  
  return(dataframe)
}

# UI function 
#' Load Data UI module
#' @description Load Data in shiny. Choose between .txt, .csv, .FEATHER format
#'
#' @param id UI identification 
#'
#' @return Ui function.
#' @import shiny
#' @export
loadfileUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    selectInput(ns("format"), "Select the format file", 
                choices = c("csv", "txt", "feather"), 
                selected = "txt"), 
    
    uiOutput(ns("datafile"))
  )
  
}


#' Load Data in Shiny. Server function
#' @description Choose between .txt, .csv, .FEATHER format. 
#' @return A reactive Dataframe.
#' @import shiny
#' @export
#' @examples
#' require(shiny)
#' if (interactive()) {
#' ui <- 
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         loadfileUI(id = "read")
#'     ), 
#'       mainPanel(tableOutput("tbl"))
#'   )
#' )
#' 
#' 
#' server <- function(input, output, session) {
#'   
#'   data <- callModule(loadfile, "read")
#'   
#'   output$tbl <- renderTable({
#'     validate(need(data(), message = FALSE))
#'     
#'     head(data()())
#'   })
#' }
#' 
#'shinyApp(ui, server)
#' }
loadfile <- function(input, output, session) {
  # detect which case
  inputfile <- reactive({
    
    validate(need(input$format, message = FALSE))
    
    
    switch(input$format, 
           csv     = csvFile, 
           txt     = txtFile, 
           feather = featherFile)
  })
  
  # generate ui momdule
  output$datafile <-  renderUI({
    validate(need(input$format, message = FALSE))
    
    ns <- session$ns
    
    UImodule <- paste0(input$format, "FileInput")
    label <-    paste(input$format, "Format", sep = "  ")
    
    # call the UI module
    do.call(what = UImodule, 
            args =list(id = ns("datafile"), label = label))
    
    
    
    
  })
  
  
  datafile <- reactive({
    
    validate(need(inputfile(), message = FALSE))
    
    # call the server module 
    callModule(inputfile(), "datafile")
    
  })
  
  return(datafile)
}

