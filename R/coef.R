#' Coef
#'
#' @param linreg_object e
#'
#' @return coeffients
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' coef(s)
coef.linreg <- function(linreg_object) {
  return(linreg_object$coefficients)
}
