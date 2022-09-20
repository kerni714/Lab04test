#' pred
#'
#' @param linreg_object - object of type linreg
#'
#' @return fitted values
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' pred(s)
pred <- function(linreg_object) {
  return(linreg_object$fitted_values)
}
