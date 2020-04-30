#  function calls ----

# manipulate calls of a functions (USE CAREFULLY)

# modify the function call x 
#' Modify call of a function
#'
#' @param x function call object
#' @param param chr. Named argument of the function.
#' @param val New val of the argument.
#'
#' @return a function call
#' @export
#'
.modify_call <- function(x, param, val) {
  
  x[[param]] <- val
  x
}


#' modify the function call x with a list of args
#'
#' @param x A function Call
#' @param args list of Arguments, the names correspond to the names of the formals of the function.
#'
#' @return A function call
#' @export
#'
modify_call <- function(x, args) {
  
  # args must have names
  stopifnot(!is.null(names(args)))
  
  # set parameters
  params <- names(args)
  
  
  # assign the arguments
  for (i in 1 : length(args)) {
    x <- .modify_call(x, param =params[[i]], val = args[[i]])
  } 
  
  x
}

# extract elemts of a list and paste them in a character vector
.expand_list_args <- function(x,   nm = deparse(substitute(x))) {
  vc <- NULL
  
  for (i in names(x)){
    vc <- c(vc, i)
    
  }
  
  paste0(nm, "$", vc,  collapse = " ,")
  
}

# .extract_list_args for a list of lists
expand_list_args <- function(x, sep = ", ") {
  res <- NULL
  
  for (i in 1:length(x)) {
    nm <- deparse(substitute(x[[i]]))
    res <- c(res, .expand_list_args(x[[i]], nm = nm))
  }
  
  res
}

# manipulate the call of a function:
# x are arguments of named arguments
# f is the name of a function. is equivalent to:
# f( names(x)[[1]] = x[[1]], ...., names(x)[[n]] = x[[n]])
# if you want to provide additional unnaed arguments, 
# you must pass them with dots

# expandCall(x = list(n = 100, sd = 1), f = "rnorm")
# is equivalent to rnorm(n = 100, sd = 1)
# expandCall(x = list(nrow = 2), f = "grid.arrange", plot_list1, plo_list2)

expandCall <- function(x, f, ...) {
  
  
  named_args <- purrr::imap(.x = x, ~paste0(.y, " = ", .x)) %>% paste(collapse = ",")
  add <- list(...)
  
  if(length(add) > 0) {
    args <- expand_list_args(add)
    args <- paste(args, collapse = ",")
    args <- paste0(args, ",", named_args)
    
  } else {
    args <- named_args
  }
  
  
  # generate expression
  expr <- paste0(f, "(", args, ")")
  # evaluate expression
  eval(parse(text = expr))
  
}
