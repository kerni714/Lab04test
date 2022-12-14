#' Residuals
#'
#' @param object object of type linreg
#' @param ... arguments to be passed to methods
#'
#' @return residuals
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' residuals(s)
residuals.linreg <- function(object, ...) {
  return(object$residuals)
}
