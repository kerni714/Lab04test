#' Print
#'
#' @param x object of type linreg
#' @param ... arguments to be passed to methods
#'
#' @return prints output
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' print(s)
print.linreg <-function (x, ...){

  cat("", sep="\n\n")

  cat("Call:")

  cat("", sep="\n\n")

  print(x$call)

  cat("", sep="\n\n")

  cat("Coefficients:")
  cat("", sep="\n\n")
  print(x$coefficients)
  #cat(linreg_object$Coefficients)

}
