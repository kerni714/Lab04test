#' pred
#'
#' @param object - object of type linreg
#' @return fitted values
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' pred(s)
pred <- function(object) {
  return(object$fitted_values)
}
