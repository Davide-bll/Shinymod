# UI module: histogram, scatter 1/2 vars, boxplot
# define UI
#' Automatic Data Visualization in Shiny
#'
#' @param id Identification Module
#'
#' @return Ui module
#' @export
#'
typeplotUi <- function(id) {
  ns <- NS(id) 
  
  tagList(
    
    # Panels on the left---------------
    sidebarPanel(
      selectInput(ns("type_plot"), "Choose a type of visualisation:", 
                  choices = c("boxplot", "histogram",  
                               "plot_1var", "plot_2var"), 
                  selected = "histogram"),
      
      uiOutput(ns("custom")),
      uiOutput(ns("width")),
      uiOutput(ns("grp")),
      uiOutput(ns("custom2")), 
      uiOutput(ns("width2")),
      uiOutput(ns("grp_selection")),
      uiOutput(ns("x_col")),
      uiOutput(ns("set_axis1")),
      uiOutput(ns("y_axis1")), 
      uiOutput(ns("x_col2")),
      uiOutput(ns("set_axis2")),
      uiOutput(ns("y_axis2")),
      uiOutput(ns("x_axis2"))
      ),
  
    # output ----------------
    mainPanel(
      
      plotOutput(ns("plot_1")),
      plotOutput(ns("plot_2")))
  )
}

# server function of type plot  UI
#' Server function of Data Visualization in Shiny
#'
#' @param data A Dataframe
#'
#' @return A Shiny server module
#' @import dplyr graphics shiny 
#' @export
#'
#' @examples
#' # an example 
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#' 
#'   typeplotUi("type")
#' 
#' 
#' )
#' 
#' server <- function(input, output, session) {
#' 
#'   callModule(typeplot, "type", data = CO2)
#' 
#' }
#' 
#' 
#' shinyApp(ui, server)
#' 
#'}
typeplot <- function(input, output, session, data) {
  
  # change factpr to charachter
  data <- data %>% mutate_if(is.factor, as.character)
  # extract different type of vars: chc, num, date/datetime  
  l_vars <- extract_types(data)

  # reactive variables-----    
  type_pl <- reactive({
    validate(need(input$type_plot, message = FALSE))
    input$type_plot
    
    })

  # dynamic UI:  conditional panels---------------
  # customize width of histogram
  output$x_col <- renderUI({
    
      ns <- session$ns
      
      test <- logi_test(type_pl(), "histogram", "plot_1var", "plot_2var", type = "any")
      # if histogram is present
      if(isTRUE(test)) {   
        selectInput(ns("x_col"), "Variable to show:",
                    choices = l_vars[["num_vars"]])
      }
  })
  
  xcol <- reactive({
    
    validate(need(input$x_col, message = FALSE))
    input$x_col
  })
  # customize histogram?
  output$custom <- renderUI({
    ns <- session$ns
    
    test <- logi_test(type_pl(), "histogram")
    if(isTRUE(test)) {
      checkboxInput(ns("custom"), "Customize", FALSE)
    } 
  })

    # choose width of the first histogram classes
  output$width <- renderUI({
    ns <- session$ns
    validate(need(input$custom, message = FALSE))
    
    if(input$custom && type_pl() == "histogram") {
      
    sliderInput(ns("width"), "Change Width", min = 0, max = 150, value = 50)  
    }
  
  })
  
  # limit y axis?
  output$set_axis1 <- renderUI({
    
    ns <- session$ns
    
    test <- logi_test(type_pl(), "plot_1var")
    if(isTRUE(test)) {
      checkboxInput(ns("set_axis1"), "limit values Y-axis", FALSE)
    } 
  })
  
  output$y_axis1 <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$set_axis1, message = FALSE))
    
    if(input$set_axis1 && type_pl() == "plot_1var") {
      # find min and max vals
      min_val <- data[[xcol()]] %>% min
      max_val <- data[[xcol()]] %>% max
      
      sliderInput(ns("y_axis1"), paste0("Change range of ", xcol(), "-axis" ), min = min_val, max = max_val, 
                  value = c(min_val, max_val))  
    }
    
    
  })
  
  # limits for y of scatter plot 1var
  y_axis1 <- reactive({
    validate(need(input$y_axis1, message = FALSE))
    
    c(min(input$y_axis1) , max(input$y_axis1))
    
  })
  
   # grp selection
  output$grp <- renderUI({ 
    
      ns <- session$ns
      test <- logi_test(type_pl(), "histogram", "plot_1var", "plot_2var", type = "any")
      
      
      
      if(isTRUE(test)) {
        
        selectInput(ns("grp"), "Choose a Grouping Variable:", 
                    choices = c(l_vars[["chc_vars"]], "aggregate"), 
                    selected = "aggregate") }
  
  })
  
  grp_var <- reactive({
    
    validate(need(input$grp, message = FALSE))    
    input$grp
    
    })
  
  # customize histogram 2 classes width?
  output$custom2 <- renderUI({
    
    
    ns <- session$ns
    
    test <- all(logi_test(type_pl(), "histogram"), 
                logi_test(grp_var(), "aggregate", type = "!"))
    
    if(isTRUE(test)) {
      checkboxInput(ns("custom2"), "Customize grouped histogram", FALSE)
    } 
    
  })

  
  output$width2 <- renderUI({
    ns <- session$ns
    
    validate(need(input$custom2, message = FALSE))
    
    test <- all(logi_test(type_pl(), "histogram"), 
                logi_test(grp_var(), "aggregate", type = "!"))
    
    if(input$custom2 && test) {
      
      sliderInput(ns("width2"), "Change Width plot 2", min = 0, max = 150, value = 30)  
    }
  })
  
  # choose the level of the selcted group
  output$grp_selection <- renderUI({
    
    # if it makes sense evaluating groupvar
    ns <- session$ns
    test <- all(logi_test(grp_var(), "aggregate", type = "!"), logi_test(type_pl(), "histogram"))
      
        if(isTRUE(test)) {
        
          grp_sel <- data[[grp_var()]] %>% unique()
          selectInput(ns("grp_selection"), paste("Choose the Level of", grp_var(), sep = ": "),
                      choices = grp_sel)
        
      }
    
  })
  
  
  
  output$x_col2 <- renderUI({
    
    ns <- session$ns
    

      
      # evaluate logical condition: plot2 var present
      test <- logi_test(type_pl(), "plot_2var")
      
      if(isTRUE(test)) {  
        
        selectInput(ns("x_col2"), "Second Variable to show:",
                    choices = c(l_vars[["date_vars"]], l_vars[["num_vars"]]))
      }

    
  })
  

  xcol2 <- reactive({
    
    req(input$x_col2)
    input$x_col2
  })


  # change limits of axis 2? 
  output$set_axis2 <- renderUI({
    
    ns <- session$ns
    
    test <- logi_test(type_pl(), "plot_2var")
    if(isTRUE(test)) {
      checkboxInput(ns("set_axis2"), "limit values of  XY-axis", FALSE)
    } 
  })
  
  # UI y_axis 2
  output$y_axis2 <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$set_axis2, message = FALSE))
    
    if(input$set_axis2 && type_pl() == "plot_2var") {
      # find min and max vals
      min_val <- data[[xcol()]] %>% min
      max_val <- data[[xcol()]] %>% max
      
      sliderInput(ns("y_axis2"), paste0("Change range of ", xcol(), "-axis" ), min = min_val, max = max_val, 
                  value = c(min_val, max_val))  
    }
    
    
  })
  
  # limits for y of scatter plot 2var
  y_axis2 <- reactive({
    validate(need(input$y_axis2, message = FALSE))
    
    c(min(input$y_axis2),  max(input$y_axis2))
    
  })
  
  # UI: set x axis 2
  output$x_axis2 <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$set_axis2, message = FALSE))
    
    if(input$set_axis2) {
      # find min and max vals
      min_val <- data[[xcol2()]] %>% min
      max_val <- data[[xcol2()]] %>% max
      
      sliderInput(ns("x_axis2"), paste0("Change range of ", xcol2(), "-axis" ), min = min_val, max = max_val, 
                  value = c(min_val, max_val))  
    }
    
    
  })
  
  # limits for x of scatter plot 1var
  x_axis2 <- reactive({
    validate(need(input$x_axis2, message = FALSE))
    
    c(min(input$x_axis2) , max(input$x_axis2))
    
  })
  
  
  ###### dyamic list of data ------
  #they change depending on the grouping variable 
  
  l_data <- reactive({  
  
    if(grp_var() != "aggregate") {
      l_data <- split(data, data[[grp_var()]])   
      return(l_data)
        
      }
  })
  
  
  
  ######  plot 1 window:   --------
  #### aggregated plot: boxplot, histogram,  plot of xvars 
  ###  loadings of the vars for the first k pca
  output$plot_1 <- renderPlot ({
      
      if(type_pl() == "histogram") {
        
        width <- 30
        
        # check if custom control is activated
        if(isTruthy(input$custom)) {
        if(input$custom) {
          validate(need(input$width, message = FALSE))
          
          width <- input$width 
        }
        }
        
        validate(need(xcol(), message = FALSE))
        # generate histogram
          df_hist(data, xcol(), custom = width) %>% plot()
          
      }  

      
      if(type_pl() == "boxplot") {
        
        # do boxplot aggregated (of the selected numerical vars)
        data[l_vars[["num_vars"]]] %>%
          graphics::boxplot(main = "Vars Box plot")
      }
      
      if(type_pl() == "plot_1var") {
        # by default, don't do any filter
        y_lim <- NULL
        
        # if setyaxis is tru
        if(isTRUE(input$set_axis1)) {
          y_lim <- y_axis1()
        }
        scatter_plot(data, y = xcol(), y_limits = y_lim) %>% plot()
        
      }
      
      if(type_pl() == "plot_2var"){
        # by default, don't do any filter
        x_lim <- NULL
        y_lim <- NULL
        
        # if setyaxis is tru
        if(isTRUE(input$set_axis2)) {
          
          y_lim <- y_axis2()
          x_lim <- x_axis2()
          
        }
        
          
          scatter_plot(data, x = xcol2(), y = xcol(), x_limits = x_lim, y_limits = y_lim) %>% plot()
        
      }

      
    
    
    
  })
  
  ######  plot 2 window:  ----------
  ####### grouped histogram, boxplot, plot

  
  output$plot_2 <-  renderPlot ({
  
  # wait for input.grp_var    
  validate(need(input$grp, message = FALSE))
    
      if(grp_var() != "aggregate") {

        if(type_pl() == "histogram") {
          
          if(isTruthy(input$grp_selection)) {
            width2 <- 30

            if(input$custom2) {
              validate(need(input$width2, message = FALSE))
              width2 <- input$width2
            } 

            lev1 <- input$grp_selection
            lev2 <- xcol()
            df <- l_data()[[lev1]]
            
            # grouped histograms
            df_hist(df, x_col = lev2, title = lev1, custom = width2) %>% plot()
          }
        }
        
        if(type_pl() == "plot_1var"){
          
          # by default, don't do any filter
          y_lim = NULL
          
          # if setyaxis is tru
          if(isTRUE(input$set_axis1)) {
            y_lim <- y_axis1()
          }
          
          
          scatter_plot(data, y = xcol(), color = grp_var(), y_limits = y_lim) %>% plot()
          
        }
        
        if(type_pl() == "plot_2var") {
          # by default, don't do any filter
          x_lim <- NULL
          y_lim <- NULL
          
          # if setyaxis is tru
          if(isTRUE(input$set_axis2)) {
            y_lim <- y_axis2()
            x_lim <- x_axis2()
          }
          
          

            scatter_plot(data, x = xcol2(), y = xcol(), color = grp_var(), 
                                     x_limits = x_lim, y_limits = y_lim) %>% plot()

        }
        
      }

  })

}


