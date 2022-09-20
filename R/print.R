#' Print
#'
#' @param linreg_object object of type linreg
#'
#' @return prints output
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' print(s)
print.linreg <-function (linreg_object){

  cat("", sep="\n\n")

  cat("Call:")

  cat("", sep="\n\n")

  print(linreg_object$call)

  cat("", sep="\n\n")

  cat("Coefficients:")
  cat("", sep="\n\n")
  print(linreg_object$coefficients)
  #cat(linreg_object$Coefficients)

}
