# Variance module: 
# numerical summaries
# pca on normal and scaled data

# makes sure the function in  "C://Users//Utente//Desktop//shiny_modules//function//utils.R"
# are loaded 


# require(dplyr)
# 
# # load functions----
# folder <- "C://Users//Utente//Desktop//shiny_modules//functions"
# source(paste(folder, "utils.R", sep = "//"))

# UI function ----
#' PCA shiny module
#'
#' @param id Module identification
#'
#' @return Shiny Ui module
#' @import shiny
#' @export
#'
pcaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarPanel(
          selectInput(ns("type_pca"), "PCA TYPE:",
                      choices = c("normal", "scaled"), 
                      selected = "normal"), 
          uiOutput(ns("k_c")),
          selectInput(ns("plot2"), "Choose the type of visualisation:", 
                      choices =c("cum_sd" ,"stat_2c"), 
                      selected = "cum_sd"), 
          textOutput(ns("stats")), 
          checkboxInput(ns("exclude"), "exclude some vars? ", value = FALSE), 
          uiOutput(ns("vars_excluded"))
        ), 
    mainPanel(
      plotOutput(ns("plot1")), 
      plotOutput(ns("plot2"))
      )
  )
}

# server logic -----
#' Perform PCA analysis on shiny
#'
#' @param data  A Dataframe
#'
#' @return
#' @import dplyr shiny
#' @export
#'
#' @examples
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#'   title = "Principal Component Analysis",
#'   pcaUI("pca")
#' 
#'   )
#' 
#' server <- function(input, output, session) {
#' 
#'   callModule(pca, "pca", data = mtcars)
#' }
#' shinyApp(ui, server)
#'} 
pca <- function(input, output, session, data) {

  # detect types of columns
  l_vars <- extract_types(data)
  
  # consider only num vars
  data <- data[l_vars[["num_vars"]]]

  # logical must some numerical vars be excluded?
  output$vars_excluded <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$exclude, message = FALSE))
    
    if(input$exclude) {
      selectInput(ns("vars_excluded"), "Select the vars to be excluded", 
                  choices  = names(data), 
                  multiple = TRUE)
    }
  })
  # reactive dataframe----
  r_data <- reactive({
    
    res <- data
    

    if(!is.null(input$vars_excluded)) {
      excluded <- input$vars_excluded
      
      res <- data %>%
        select(-excluded)
      
    }
    
    res
  })
  # get PC's
  # reactive pca object -------
  l_pca <- reactive({

      # main statistics
  # covariance Matrix
  S <- r_data() %>% 
        as.matrix() %>%
        cov()
  
  # generalized Variance
  Gv <- eigen(S)$values %>% prod()
  
  # total variance
  Tv <- eigen(S)$values %>% sum()
    
    validate(need(input$type_pca, message = FALSE))
    
    cor <- switch(input$type_pca, 
                  normal = FALSE, 
                  scaled = TRUE)
    
    df_pca <- r_data() %>%
              princomp(scores = TRUE, cor = cor)
  
  # loadings
  df_load <- df_pca$loadings
  
  # scores
  df_scores <- df_pca$scores
  
  # explained cumulative std
  cum_var <- cumsum(df_pca$sdev/ sum(df_pca$sdev)) 
  
  # list of results
  list( S         = S, 
        Gv        = Gv, 
        Tv        = Tv,
        df_pca    = df_pca, 
        df_load   = df_load, 
        df_scores = df_scores,
        cum_var   = cum_var)
  })
  
  output$k_c <- renderUI({
    
    
    ns <- session$ns
    
    sliderInput(ns("k_c"), "Select the component:", min = 1, max = ncol(r_data()), 
                value = 1, step = 1)
    
    
  })  
  
  output$stats <- renderText ({
  paste("Total Variance:", as.character(round(l_pca()[["Tv"]], digits = 1)),
          "    Generalized Variance:", as.character(round(l_pca()[["Gv"]], digits = 1)))
    
  })

  # first plot-----
  output$plot1 <- renderPlot({
  
  
  validate(need(input$k_c, message = FALSE))  
  
  loads <- l_pca()[["df_load"]]  
  # first  k components    
  graphics::barplot(loads[, input$k_c], 
        main  = paste0("PCA  component: ", input$k_c), 
        col = "red")
    
  })
  
  output$plot2 <- renderPlot({
    
    validate(need(input$plot2, message = FALSE))
    
    
    if(input$plot2 == "stat_2c") {
      scores <-l_pca()[["df_scores"]]
      graphics::plot(scores[, 1:2], main = "Stat-Units against first two PC")              
    }
    if(input$plot2 == "cum_sd") {
      
      df <- l_pca()[["cum_var"]]
      graphics::plot(df, main = "Cumulative Standar Deviation",
           type = "b", 
           xlab = "Principal Components", 
           ylab = "Cumulative Standard Deviation",
           ylim = c(0:1))
    }
  })
}

