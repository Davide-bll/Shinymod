#  UTILS:
# create new columns using case_when
# deal with different types of vars
# filter a reactieve input in a dataframe



# 1:logical----
# check if the arguments are equal to the x input, according to the type function.



#' Generalise logical statements
#' look if x is equal to the dots arguments. If ... is a list, then you can specify
#' a function rule in type, passing by name.
#'
#' @param x A character
#' @param ... A vector of character
#' @param type a function
#'
#' @return A logical
#' @import purrr shiny
#' @export
#'
#' @examples logi_test("ciao", c("come", "va", "ciao"))
#'           logi_test("ciao", c("come", "va", "ciao"), type = "all")
logi_test <- function(x, ..., type = "any") {

  arg <- list(...)

  # if x is created
  if(shiny::isTruthy(x)) {
    res <- purrr::map_lgl(arg, ~isTRUE(x == .x))
    # action on the result
    res <-  do.call(type, list(res))

    res
  }
}


# 2: detect type of vars----


#' Detect Date-Datetime object
#'
#' @param x 
#'
#' @return logical
#' @export
#'
is.datetime <- function(x) {
  lubridate::is.Date(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXt(x)
}

#' Extract class of variables from a dataframe
#' @description Extract class of variables from a dataframe. Factors are considered character
#' @param x Dataframe
#' @param num_to_chr Character string. Numerical Columns ending/starting with \code{num_to_chc}
#' will be considered character.
#'
#' @return list of numerical, character, datevars
#' @import dplyr
#' @export
#'
extract_types <- function(x, num_to_chr = "id") {

  # numerical vars
  num_vars <- x %>% select_if(is.numeric) %>% names()

  # find character vars

  chc_vars <- x %>% select_if(~(is.character(.) | is.factor(.))) %>% names()
  # date- date/time vars
  date_vars <- x %>% select_if(is.datetime) %>% names()

  # data consistency : if starts or ends with "id" don t consider them num
  add_vars <- num_vars[(endsWith(num_vars, num_to_chr) | startsWith(num_vars, num_to_chr))]

  # update chc_vars
  chc_vars <- c(chc_vars, add_vars)

  # update num vars
  num_vars <- num_vars[!(endsWith(num_vars, "id") | startsWith(num_vars, "id"))]

  list(
    chc_vars  = chc_vars,
    num_vars  = num_vars,
    date_vars = date_vars
  )
}

#' Extract class of variables from a dataframe dataframe
#'
#' @param x df
#'
#' @return list
#' @import dplyr
#' @export
#'
extract_types_original <- function(x) {

  # numerical vars
  num_vars <- x %>% select_if(is.numeric) %>% names()

  # find character vars
  chc_vars <- x %>% select_if(~(is.character(.) | is.factor(.))) %>% names()

  # date- date/time vars
  date_vars <- x %>% select_if(is.datetime) %>% names()


  list(
    chc_vars  = chc_vars,
    num_vars  = num_vars,
    date_vars = date_vars
  )
}



# 3 filtering-----
# userfilter
#' Filter chr or num vars
#'
#' @param x df
#' @param param col name 
#' @param val numeric interval or charachter string
#'
#' @import dplyr
#' @export
#' @return filtered df
#'
user_filter <- function(x, param, val) {
  # works for numerical and datevars
  param <- sym(param)
  if(is.numeric(val) | is.datetime(val)) {
    x <- x %>%
      filter(!!param <= max(val) & !!param >= min(val))
  }
  
  if(is.character(val)) {
    x <- x %>% filter(!!param %in% val)
  }
  
  x
}

# apply simaultaneous filter
# 
#' Simultaneous filter
#'
#' @param x df
#' @param params list of params 
#' @param vals  list of vals
#' @export
#'
#' @return filtered df
#'
user_filter_m <- function(x, params, vals) {

  for (i in (1:length(params))) {

    x <- user_filter(x, params[[i]], vals[[i]])
  }
  x
}

# look for a specific element inside a list
#'look for a specific element inside a list
#'
#' @param x list
#' @param look name of elemet in the list to look for
#' @export
#'
#' @return list
#'
look_for <- function(x, look = "transformed") {
  if(look %in% names(x)) {
    return(x[[look]])
  } else {
    x
  }
}
