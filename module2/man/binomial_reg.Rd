% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bino_reg.R
\name{binomial_reg}
\alias{binomial_reg}
\title{Binomial Regression via Newton-Raphson}
\usage{
binomial_reg(X, Y, n, intercept = TRUE, tol = 1e-10, max.iter = 100)
}
\arguments{
\item{X}{A matrix of covariates}

\item{Y}{A numeric vector of counts of successes}

\item{n}{A numeric vector of binomial denominators (trials)}

\item{intercept}{Logical indicating whether to include an intercept}

\item{tol}{Convergence tolerance (default: 1e-10)}

\item{max.iter}{Maximum iterations (default: 100)}
}
\value{
A numeric vector of estimated coefficients
}
\description{
Fits a binomial regression model using Newton-Raphson optimization.
}
\examples{
data(mtcars)
# Use only non-collinear predictors and remove intercept
X <- model.matrix(~ 0 + hp + wt + gear, data = mtcars)
Y <- mtcars$vs
n <- rep(1, nrow(mtcars))
fit <- binomial_reg(X, Y, n, intercept = FALSE)  # Disable intercept addition
print(fit)
}
