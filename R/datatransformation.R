# DATA Transformation and testing----


# box cox transformation of a numeric vector x
# in case x is negative then a constant is added 
#'BoxCox transformation of x 
#'
#' @param x num vector
#' @export
#' @return list. transformed x, lambda optimal, formula if \code{x} has negative values
my_boxcox <- function(x) {
  
  formula <- case_when(
    all(x > 0)  ~ "x", 
    any(x <= 0) ~ "x + abs(min(x)) + 1"
  )  
  
  
  x <- eval(parse(text = formula))
  
  lambda <- car::powerTransform(x)
  x_t <- car::bcPower(x, lambda = lambda$lambda)
  
  list(transformed = x_t, 
       lambda      = lambda, 
       formula     = formula)
  
}
