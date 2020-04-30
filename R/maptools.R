
# reactive map -> add trajectory according to user input
# USE ONLY in a reactive enviroment!
#' Add polylines to an existing map
#'
#' @param data Data containing lynes, and lng, lat cols
#' @param grp_f Levels of the group accepted. The group is identified by \code{criteria}
#' @param clr chr. Identy the colors of the lines
#' @param proxy Existing Leaflet map.
#' @param label Chr. Label pf the line
#' @param criteria Chr. Name of the column identifying the filter group.
#' @param traj_col Chr. Name of the column identifying the Trajectories
#'
#' @return A Leaflet map
#' @import dplyr purrr leaflet RColorBrewer
#' @export
my_leafletproxy <- function(data, grp_f, clr = "blue", proxy, label, criteria, traj_col) {
  
  # fiter data accordingly to "grp_f"
  # this function can be improved by adding an input of the column variable 
  # that determines the filter, namely, trajectory_id should be passed as input
  data <- data %>% filter(!!sym(criteria) %in% grp_f)
  
  proxy %>%  
    addPolylines(lng   = ~lng,
                 lat   = ~lat,
                 data  = data, 
                 label = ~paste0(label, grp_f), 
                 color = clr, 
                 
                 popup = ~paste0("Trajectory: ", data[[traj_col]]))
}

# myleaflet function wrap
#' Wrap of my leaflet. The Arguments are directly passed in \link{my_leafletproxy}
#'This is used to distinct different "types" of Trajectories
#' @param gruppo_list chr or list. Passed to map
#' @param color_list chr or list. Passed to map
#'
#' @return list of updated leaflet maps
#' @import purrr
#' @export
#'
wrap_myleaflet <- function(data, gruppo_list, color_list, proxy, label, criteria, traj_col) {
  map2(.x = gruppo_list, 
       .y = color_list, 
       ~my_leafletproxy(data  = data, 
                        grp_f = .x, 
                        clr   = .y, 
                        label = label, 
                        proxy = proxy, 
                        criteria = criteria, 
                        traj_col = traj_col
       ))
}

# add circles to an existig map (proxy argument)
# run only in reactive enviroments 
# if not constant, radius and label formula must be passed as expression
# pal is the colorpalette function
#'add circles to an existig map (proxy argument)
#'
#' @param proxy Existing Map
#' @param data Dataframe containing "lat" and "lng"
#' @param points_col Chr. Column which Identify different points
#' @param pal Colorpalette function
#' @param radius an expression (formula)
#' @param label_formula an expression for the label. By default is used the \code{points_col} column
#'
#' @return A Leaflet Map
#' @import dplyr leaflet
#' @export
#'
addCircles_map <- function(proxy, 
                           data, 
                           points_col, 
                           pal, radius = rlang::expr(10), 
                           label_formula = rlang::expr(~paste0("Person ID: ",as.character(data[[points_col]])))) {
  
  
  proxy %>%  
    addCircleMarkers(
      data      = data,
      fillColor = ~pal(data[[points_col]]),
      color     = ~pal(data[[points_col]]),
      popup     = ~paste(paste0("Lat: ",round(lat, digits = 3)), 
                         paste0("Lng: ", round(lng, digits = 3)), 
                         sep = "<br>"),
      label     = eval(label_formula), 
      radius    = eval(radius)) 
  
}
