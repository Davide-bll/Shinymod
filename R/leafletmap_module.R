# module for leaflet map

# # load functions
# folder <- "C://Users//Utente//Desktop//shiny_modules//functions"
# 
# source(paste(folder, "utils.R", sep = "//"))
# source(paste(folder, "maptools.R", sep = "//"))
# 


#' Ui module for Shiny interactive map
#' @description @seealso \link{leafletMap}
#' @param id shiny modue identification
#'
#' @return Server function
#' @import shiny
#' @export
#'
leafletMapUi <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    
    fluidRow(
      # first column---------
      column(
        width = 4,
        # date input: consider observation from start_end, to end_date
        uiOutput(ns("time")),
        uiOutput(ns("pvec")),
        uiOutput(ns("tvec"))
      ),
      # second column----
      column(
        width = 4,
        uiOutput(ns("criteria"))), 
      
      
      # third column------
      column(
        width = 4, 
        # colors scheme: select between different scales of color
        selectInput(ns("colors"), "Color Scheme of Points",
                    rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
        checkboxInput(ns("traj"), "Show trajectories", FALSE),    
        checkboxInput(ns("legend"), "Show legend", FALSE),
      ),
      
      # interactive map output----
      leafletOutput(ns("my_leaf")))
    
  )
  
  
}

# server function of shiny_APP maputo data exploration


# server logic------------------
#' Leaflet map module, Server logic 
#'
#' @param data A Dataframe, is supposed to contains "lat" and "lng" cols, 
#' otherwise things won't work
#' @param date_col Chr. Name of the column identifying Date time reference.
#' @param points_col Chr.Name of the colum of the Points in the map. (circles)
#' The trajectories(polylines) are always represented with respect to this column reference.
#' @param v_anim num. Speed of the time animation. \code{1} means 5 hours. 
#' @param traj_col Column idenifying the trajectories
#'
#' @return A shiny Module
#' @import dplyr leaflet purrr RColorBrewer shiny
#' @export
#' @examples 
#' 
#' require(shiny)
#' require(dplyr)
#' if(interactive()) {
#' df <- feather::read_feather("C://Users//Utente//Desktop//Maputo//transport_mode_detection//data_speed_acc_new.feather") %>%
#'   slice(1:100)
#' df <- rename(df, time = datetime, pid  = person_id, tid  = trajectory_id,)
#' ui <- fluidPage(
#'   leafletMapUi("map")
#' )
#' server <- function(input, output, session) {
#'   callModule(leafletMap, "map", data = df, v_anim = 1, date_col = "time", points_col = "pid", traj_col = "tid")
#' }
#' shinyApp(ui, server)
#'}
leafletMap <- function(input, output, session, data, date_col = "datetime", 
                       points_col = "person_id", 
                       v_anim = 24, 
                       traj_col = "trajectory_id") {
  # define UI parameters
  min_date <- data[[date_col]] %>% min()
  max_date <- data[[date_col]] %>% max()
  l_vars <- extract_types(data)
  
  output$time <- renderUI({
    ns <- session$ns
    
    
    sliderInput(inputId = ns("time"), label = "Choose Time Interval", 
                min     = min_date, 
                max     = max_date,
                value   = c(min_date, max_date),
                step    = 3600 * v_anim * 5, # set to increment by v_anim hours, adjust appropriately
                animate = TRUE)
  })
  
  output$pvec <- renderUI({
    
    ns <- session$ns
    
    min_p <- data[[points_col]] %>% min()
    max_p <- data[[points_col]] %>% max()
    
    sliderInput(ns("pvec"), "Choose ID Persons", 
                min       = min_p, 
                max       = max_p, 
                value     = c(0,1), 
                animate   = FALSE, 
                dragRange = TRUE)
    
  })  
  
  # REACTIVE VARIABLES  --------------
  pvec <- reactive({ input$pvec })
  
  # user input: final time of date
  end_date <- reactive({
    req(input$time)
    c(min(input$time), max(input$time))
  })
  
  
  output$tvec <- renderUI({
    # # find consistent trajectories
    ns <- session$ns
    
    req(end_date, pvec)
    l_values <- user_filter_m(data, list(date_col, points_col), list(end_date(), pvec())) %>% 
      pull(!!sym(traj_col)) %>% unique()
    
    min_val <- ifelse(length(l_values > 0), min(l_values), 0)  
    max_val <- ifelse(length(l_values > 0), max(l_values), 1)  
    
    sliderInput(ns("tvec"), "Choose range of trajectories", 
                min       = min_val, 
                max       = max_val, 
                value     = c(min_val , max_val),
                step      = 1,
                animate   = TRUE, 
                dragRange = TRUE)
    
  })
  
  # trajectory  reactive vals
  tvec <- reactive({ 
    
    if(!is.null(input$tvec))  { 
      c(input$tvec[[1]],input$tvec[[2]])
    } else return(c(0,1))
    
  })
  
  output$criteria <- renderUI({
    ns <- session$ns
    
    choices <- l_vars[["chc_vars"]]
    selectInput(ns("criteria"), "Choose a grouping criteria", choices = choices)
    
  })  
  
  # color palette: based on the trajectorys selected
  colorpal <- reactive({
    colorFactor(input$colors, domain = NULL)
  })
  
  # color palette function: is not reactive, and it's for the trajectories
  colorpal2 <- colorFactor(rownames(subset(brewer.pal.info, category %in% c("seq", "div")))[5], domain = NULL)
  
  # criteria of grouping
  criteria <- reactive({
    validate(need(input$criteria, message = FALSE))
    input$criteria
  })
  
  
  # filtered data input-----------
  df_filtered_history <- reactiveVal(data)
  
  
  observe({
    
    new_val <- user_filter_m(data, params = list(date_col, points_col, traj_col), 
                             vals   = list(end_date(), pvec(), tvec())) %>% 
      arrange(!!sym(points_col), !!sym(date_col))
    
    df_filtered_history(new_val)
  })
  
  
  # create static map-------------
  output$my_leaf <- renderLeaflet({
    
    center_lat <- data[["lat"]] %>% mean()
    center_lng <- data[["lng"]] %>% mean()
    leaflet() %>%
      addTiles() %>%
      # initial center of the map
      setView(lat = center_lat, lng = center_lng, zoom = 13)
  })
  
  #  add circles: ----------------
  observe({
    
    # create arguments
    pal <- colorpal()
    proxy <-     leafletProxy(mapId = "my_leaf") %>%
      clearMarkers() # clear previous markers, otherwise the circles won't be reactive
    
    addCircles_map(proxy, data = df_filtered_history(), points_col = points_col, pal = pal)
    
    
  })
  
  #  add polylines--------------
  observe({
    
    # if the trajectory input is true
    if(input$traj) {
      
      grps <- df_filtered_history()[[criteria()]]  %>% unique()
      
      clrs <- colorpal2(grps)
      
      
      # clear map
      proxy <- leafletProxy("my_leaf") %>% 
        clearShapes()
      
      # create list of dataframe: splitting rule -> person id
      l_data <- split(df_filtered_history(), df_filtered_history()[[points_col]]) 
      
      
      
      criteria <- criteria()
      
      # add trajectories for every person selected
      map(.x = l_data,
          wrap_myleaflet, 
          gruppo_list = grps, 
          color_list  = clrs, 
          label       = as.character(criteria), 
          proxy       = proxy, 
          criteria    = criteria, 
          traj_col    = traj_col)
      
      
      
    } else {
      # clear trajcetories otherwise
      leafletProxy("my_leaf") %>% clearShapes()
    }
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    
    proxy <- leafletProxy("my_leaf", data = df_filtered_history())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend & input$traj) {
      
      grps <- df_filtered_history()[[criteria()]]  %>% unique()
      criteria <- criteria()
      proxy %>% addLegend(position = "topright",
                          colors = colorpal2(grps), 
                          labels = grps, 
                          title = paste(criteria, "Legend ", sep = " "))
    }
  })
  
  
}



