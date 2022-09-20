#' Summary
#'
#' @param linreg_object object of type linreg
#'
#' @return prints output
#' @export
#'
#' @examples
#' s<-linreg(formula=Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#' summary(s)
summary.linreg <- function (linreg_object) {

  df <- linreg_object$df
  res_std_err <- round(sqrt(linreg_object$residual_variance),4)
  estimates <- linreg_object$coefficients
  stderr <- round(sqrt(linreg_object$coefficients_variance),5)
  tvalues <- round(linreg_object$coefficients_tvalues,2)
  pvalues <- linreg_object$coefficients_pvalues

  sign_level <- vector(length=length(estimates))
  sign_level[pvalues<0.1] <- "."
  sign_level[pvalues<0.05] <- "*"
  sign_level[pvalues<0.01] <- "**"
  sign_level[pvalues<0.001] <- "***"


  coeffs <- as.data.frame(cbind(estimates,stderr,tvalues,pvalues,sign_level))

  names(coeffs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", " ")

  cat("Coefficients:\n")
  print(coeffs)
  cat("\n")
  cat(paste0("Residual standard error: ",res_std_err, " on ", df, " degrees of freedom"))

}
