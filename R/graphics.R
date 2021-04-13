# customize scatter plots
# histograms
# check normal hypotesy
# create an histogram object from the column "x_col" of the dataframe "df"
#'create an histogram object 
#'
#' @param df df
#' @param x_col col to represent 
#' @param title optional title
#' @param custom optional cusyom
#' @import ggplot2
#' @export
#' @return histogram object
#'
# df_hist <- function(df, x_col, title = "Aggregate", custom = NULL) {
#   
#   if(is.null(custom)) {
#     n <- min(100, nrow(df)/3)
#     custom <- (max(df[[x_col]]) - min(df[[x_col]]))/n
#   }
#   
#   # create histogram object
#   df_h <- df %>%
#     ggplot(aes_string(x = x_col)) +
#     geom_histogram(fill ="darkblue", color = "black", 
#                    binwidth = custom) 
#     labs(title = paste(x_col, title, sep = "  -  "))
#   
#   df_h
# }
df_hist <- function(df, x_col, title = "Aggregate", custom = NULL) {
  
  if(is.null(custom)) {
    df_h <- df %>%
      ggplot(aes_string(x = x_col)) +
      geom_histogram(fill ="darkblue", color = "black", 
                     binwidth = custom) + 
      labs(title = paste(x_col, title, sep = "  -  "))
  } else {
    
  # create histogram object
  df_h <- df %>%
    ggplot(aes_string(x = x_col)) +
    geom_histogram(fill ="darkblue", color = "black") + 
    labs(title = paste(x_col, title, sep = "  -  "))
  }
  
  
  df_h
}

# from a numerical vector x, return a list different of plots verifing normal hyp
# scatter okit if x vs y 
# x limits and y limits are additional argment according to which df will be filtered
# if "color" is not null, then the scatter is colored wrt "color"
#' Scatter plot
#'
#' @param df dataframe
#' @param x col name of x axis
#' @param y col name of y axis
#' @param x_limits interval to filter x axis
#' @param y_limits interval to filter y axis
#' @param color chr.  Name of the variable to which apply color logic.
#' @param wrap chr. column to which apply facet_wrap, y scale is free 
#' @param title Optional.
#'
#' @import ggplot2 dplyr
#' @export
#' @return ggplot object
#'
scatter_plot <- function(df, x = NULL, y, x_limits = NULL, y_limits = NULL, 
                         color = NULL, title = "Scatter", wrap = NULL) {
  
  
  if(!is.null(x_limits)) {
    df <- df %>% filter(!!sym(x) <= max(x_limits) & !!sym(x) >= min(x_limits))
  }
  
  if(!is.null(y_limits)) {
    df <- df %>% filter(!!sym(y) <= max(y_limits) & !!sym(y) >= min(y_limits))
  }
  
  if(is.null(x)) {
    x <- 1:nrow(df)
  }
  
  if(is.null(color)) {
    
    res <- df %>%
      ggplot(aes_string(x = x, y = y)) + 
      geom_point() + 
      geom_line() +
      labs(xlab = element_blank(), 
           title = title)
  } else {
    
    res <- df %>%
      ggplot(aes_string(x = x, y = y, color = color)) + 
      geom_point() + 
      geom_line() +
      labs(xlab = element_blank(), 
           title = title)
    
  }
  if(!is.null(wrap)) res <- res + facet_wrap(wrap, scales = "free_y")
  
  return(res)
}


#' Graphical representation of Normal assumption, Univariate data
#'
#' @param x Num Vector
#' @param title_list Optional
#' @return list of plots: Scatter of \code{x}, histogram of \code{x}, qqplot of \code{x}
#' @export
#' @import ggplot2 dplyr
#'
normal_plots <- function(x, title_list = list(hist = "histogram", 
                                              scatter = "scatter", 
                                              qqnorm = "Theoretical vs Sample quantiles")) {
  
  if(!("hist" %in% names(title_list) & "scatter" %in% names(title_list) & "qqnorm" %in% names(title_list))) {
    
    names(title_list) <- c("hist", "scatter", "qqnorm")
  }
  
  df <- tibble::enframe(x, name = "obs", value = "value")  
  # scatter of x
  sx <- scatter_plot(df, y = "value", title = title_list[["scatter"]])
  
  # histogram of x
  hx <-   df_hist(df, "value", title = title_list[["hist"]])
  
  # qq norm
  qqn <- qqnorm(scale(x)) %>% 
    as_tibble() %>% 
    rename(theoretical = x, 
           sample_scaled = y) 
  
  qqn <- scatter_plot(qqn, x = "theoretical", y = "sample_scaled", 
                      title = title_list[["qqnorm"]]) 
  
  qqn <- qqn + geom_abline(slope = 1, intercept = 0)
  # results
  list(scatter = sx, 
       hist    = hx, 
       qqnorm  = qqn 
  )  
  
}



