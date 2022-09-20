#' Coef
#'
#' @param object e
#' @param ... arguments to be passed to methods
#'
#' @return coeffients
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' coef(s)
coef.linreg <- function(object, ...) {
  return(object$coefficients)
}
