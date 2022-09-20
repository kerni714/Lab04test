#' Residuals
#'
#' @param linreg_object object of type linreg
#'
#' @return residuals
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' residuals(s)
residuals.linreg <- function(linreg_object) {
  return(linreg_object$residuals)
}
