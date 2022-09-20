#' Summary
#'
#' @param linreg_object object of type linreg
#'
#' @return prints output
#' @export
#'
#' @examples
#' s<-linreg(formula=Petal.Length~Species, data=iris)
#' summary(s)
summary.linreg <- function (linreg_object) {

  df <- linreg_object$df
  res_std_err <- round(sqrt(linreg_object$residual_variance),4)
  coeffs <- as.data.frame(cbind(linreg_object$coefficients,
                               round(sqrt(linreg_object$coefficients_variance),5),
                               round(linreg_object$coefficients_tvalues,2),
                               linreg_object$coefficients_pvalues))

  # c1 <- c("(Intercept)", -2.524762,0.56344, -4.48 )
  # c2 <- c("Sepal.Width",-1.3386230,0.12236, -10.94 )
  # c3 <- c("Sepal.Length",1.775593,0.06441,27.57)
  #
  # cat(c1)
  # cat("\n")
  # cat(c2)
  # cat("\n")
  # cat(c3)
  # cat("\n")
# coeffs <- as.matrix(cbind(linreg_object$coefficients,
# round(sqrt(linreg_object$coefficients_variance),5),
# round(linreg_object$coefficients_tvalues,2),
# linreg_object$coefficients_pvalues))


  #names(coeffs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
   #names(coeffs) <- NULL
  coeffs <- as.matrix(coeffs)
  colnames(coeffs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  #cat("Coefficients:\n")
  print(coeffs)
  #cat("\n")
  cat(paste0("Residual standard error: ",res_std_err, " on ", df, " degrees of freedom"))

}
