#' Summary
#'
#' @param object object of type linreg
#' @param ... arguments to be passed to methods
#'
#' @return prints output
#' @export
#'
#' @method summary linreg
#'
#' @examples
#' s<-linreg(formula=Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#' summary(s)
summary.linreg <- function (object, ...) {

  df <- object$df
  res_std_err <- round(sqrt(object$residual_variance),4)
  estimates <- object$coefficients
  stderr <- round(sqrt(object$coefficients_variance),5)
  tvalues <- round(object$coefficients_tvalues,2)
  pvalues <- object$coefficients_pvalues

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
