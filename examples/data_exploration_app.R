remove(list = ls())

# an example of data exploration:

# require scripts for UI modules ---------

require(shinymod)
require(shiny)
require(dplyr)

# define UI  -----
ui <- fluidPage(
  tabsetPanel(
    # tab1
    tabPanel("Load Data", 
             loadfileUI("load"),
             sliderInput("size", "Set max limit size (MB)", min = 1, max = 10000, value = 30),
             mainPanel(
               tableOutput("tbl1")
             )
    ),
    # tab2 
    tabPanel("Filter Data", 
             filterpanelUi("filter"),
             mainPanel(
               tableOutput("tbl2"), 
               textOutput("txt1")
             )
    ), 
    # tab 3
    tabPanel("Data Exploration", 
             actionButton("start", "Start Exploration"),
             typeplotUi("type")),
    # tab 4
    tabPanel("PCA",
             pcaUI("pca")))
  
)

# define server logic-----
server <- function(input, output, session) {
  observe({
    validate(need(input$size, message = FALSE))
    
    maxSize <- input$size
    options(shiny.maxRequestSize=maxSize*1024^2)
  }) 
  
  #  tab 1: load Data-----
  df <- callModule(loadfile, "load")
  
  output$tbl1 <- renderTable({
    validate(need(df(), message = FALSE))
    
    head(df()())
  })
  # tab 2: filter Data----
  
  df_filtered <- reactive(callModule(filterpanel, "filter", data = df()()))
  
  observe({
    output$tbl2 <- renderTable({
      if(!is.null(df_filtered()))
        
        # plot first 50 rows
        df_filtered()() %>% slice(1:50)
    })
    # output text
    output$txt1 <- renderText({
      validate(need(df_filtered, message = FALSE))
      
      nobs <- nrow(df_filtered()()) 
      
      paste("Number of obs:", nobs, sep = " ")  
    })
    
  })
  # tab 3 and 4: Data exploration ------
  observeEvent(input$start, {
    
    if(!is.null(df_filtered()())) {
      data <- df_filtered()()
      
      # second tab
      callModule(typeplot, "type", data = data)
      
      # third tab
      callModule(pca, "pca", data = data)
    }
    
  })
  
}

shinyApp(ui, server)
