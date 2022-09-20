#' Linreg
#'
#' @param formula formula object
#' @param data dataframe
#'
#' @return object of type linreg
#' @export
#'
#' @examples
#' data(iris)
#' s<-linreg(formula=Petal.Length~Species, data=iris)
linreg <- function(formula, data) {
  #- Checks
  #- Add checks that variables in formula are in data
  #- Add checks for type of variables in formula
  stopifnot(class(formula)=="formula",
            is.data.frame(data))

  # Retreive function call
  #call <- list(call=linreg(formula, data))
  call <- match.call()
  # The first step in the function is to use the function
  #model.matrix() to create the matrix X (independent variables) and the
  # pick out the dependent variable y using all.vars().

  #- Retrive y
  y <- data[,all.vars(expr=formula)[1]]
  #- Check of y numeric
  stopifnot(is.numeric(y))

  X <- stats::model.matrix(object=formula, data=data)

  XtX_inv <- solve(t(X)%*%X)
  Xty <- t(X)%*%y
  beta_hat_1 <- XtX_inv%*%Xty

  beta_hat <- as.vector(beta_hat_1)
  names(beta_hat) <- dimnames(beta_hat_1)[[1]]

  y_hat <- X%*%beta_hat
  y_hat <- as.vector(y_hat)

  #- Residuals
  e_hat <- y-y_hat

  #- Degrees of freedom
  n <- length(y)
  p <- length(beta_hat)
  df <- n-p

  #- Residual variance
  var_hat <- ((t(e_hat)%*%e_hat)/df)[1,1]

  #- Variance beta_hat
  #beta_hat_var <- var_hat[[1]]*XtX_inv
  beta_hat_var <- diag(var_hat*XtX_inv)

  #- T-values for the coefficients
  t_beta <- beta_hat/sqrt(beta_hat_var)

  #- Calculate p-values
  p_values <- 2*stats::pt(abs(t_beta),df, lower.tail = FALSE)

  # The function should return an object with of
  # class linreg either as an S3 class or an RC class
  linres <- list(call = call,
                 formula = formula,
                 coefficients = beta_hat,
                 fitted_values = y_hat,
                 residuals = e_hat,
                 df = df,
                 residual_variance = var_hat,
                 coefficients_variance = beta_hat_var,
                 coefficients_tvalues = t_beta,
                 coefficients_pvalues = p_values
  )
  class(linres) <- "linreg"
  return(linres)
}
