% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.linreg}
\alias{print.linreg}
\title{Print}
\usage{
\method{print}{linreg}(x, ...)
}
\arguments{
\item{x}{object of type linreg}

\item{...}{arguments to be passed to methods}
}
\value{
prints output
}
\description{
Print
}
\examples{
data(iris)
s<-linreg(formula=Petal.Length~Species, data=iris)
print(s)
}
