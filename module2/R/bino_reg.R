#' First Derivative of the Log-Likelihood (Score Function)
#'
#' Computes the gradient of the log-likelihood function for binomial regression.
#'
#' @importFrom stats binomial dnorm glm.fit pchisq rnorm runif
#'
#' @param X A matrix of covariates
#' @param Y A numeric vector of counts of "successes"
#' @param beta.t A numeric vector of current coefficient estimates
#' @param n A numeric vector of binomial denominators (trials)
#' @return A numeric matrix representing the gradient vector
#' @export
#' @examples
#' # Using mtcars dataset
#' data(mtcars)
#'
#' # Create design matrix and response
#' X <- model.matrix(~ hp + wt + cyl, data = mtcars)
#' Y <- mtcars$vs  # engine type (0 = V-shaped, 1 = straight)
#' n <- rep(1, nrow(mtcars))  # Bernoulli trials
#'
#' # Compute score at initial parameters
#' beta_init <- rep(0, ncol(X))
#' score <- beta1der(X, Y, beta_init, n)
#' head(score)
beta1der <- function(X, Y, beta.t, n) {
  # Input validation
  if (!is.matrix(X)) {
    stop("X must be a numeric matrix")
  }
  if (!is.numeric(Y) || !is.numeric(beta.t) || !is.numeric(n)) {
    stop("Y, beta.t and n must be numeric vectors")
  }
  if (ncol(X) != length(beta.t)) {
    stop("Number of columns in X must match length of beta.t")
  }
  if (any(Y > n)) {
    stop("Counts in Y cannot exceed trial numbers in n")
  }
  if (any(Y < 0)) {
    stop("Counts in Y cannot be negative")
  }

  p <- exp(X %*% beta.t) / (1 + exp(X %*% beta.t))
  S <- Y - n * p
  t(X) %*% S
}

#' Second Derivative of the Log-Likelihood (Negative Observed Information)
#'
#' Computes the Hessian matrix for binomial regression.
#'
#' @param X A matrix of covariates
#' @param beta.t A numeric vector of current coefficient estimates
#' @param n A numeric vector of binomial denominators (trials)
#' @return A numeric matrix representing the negative observed information
#' @export
#' @examples
#' data(mtcars)
#' X <- model.matrix(~ hp + wt + cyl, data = mtcars)
#' n <- rep(1, nrow(mtcars))  # Bernoulli trials
#' beta_init <- rep(0, ncol(X))
#'
#' # Compute Hessian
#' hessian <- beta2der(X, beta_init, n)
#' eigen(hessian)$values  # Should all be negative
beta2der <- function(X, beta.t, n) {
  # Reuse input validation from beta1der
  if (!is.matrix(X)) stop("X must be a numeric matrix")
  if (!is.numeric(beta.t) || !is.numeric(n)) stop("beta.t and n must be numeric vectors")
  if (ncol(X) != length(beta.t)) stop("Number of columns in X must match length of beta.t")

  p <- exp(X %*% beta.t) / (1 + exp(X %*% beta.t))
  V <- diag(as.vector(n * p * (1 - p)))  # More numerically stable version
  -t(X) %*% V %*% X
}


#' Binomial Regression via Newton-Raphson
#'
#' Fits a binomial regression model using Newton-Raphson optimization.
#'
#' @param X A matrix of covariates
#' @param Y A numeric vector of counts of successes
#' @param n A numeric vector of binomial denominators (trials)
#' @param intercept Logical indicating whether to include an intercept
#' @param tol Convergence tolerance (default: 1e-10)
#' @param max.iter Maximum iterations (default: 100)
#' @return A numeric vector of estimated coefficients
#' @export
#' @examples
#' data(mtcars)
#' # Use only non-collinear predictors and remove intercept
#' X <- model.matrix(~ 0 + hp + wt + gear, data = mtcars)
#' Y <- mtcars$vs
#' n <- rep(1, nrow(mtcars))
#' fit <- binomial_reg(X, Y, n, intercept = FALSE)  # Disable intercept addition
#' print(fit)
binomial_reg <- function(X, Y, n, intercept = TRUE, tol = 1e-10, max.iter = 100) {
  # Input validation
  if (!is.matrix(X)) stop("X must be a numeric matrix")
  if (!is.numeric(Y) || !is.numeric(n)) stop("Y and n must be numeric vectors")
  if (any(Y > n)) stop("Counts in Y cannot exceed trial numbers in n")
  if (any(Y < 0)) stop("Counts in Y cannot be negative")
  if (any(n <= 0)) stop("Trial numbers in n must be positive")
  if (!is.logical(intercept)) stop("intercept must be logical (TRUE/FALSE)")
  if (tol <= 0) stop("Tolerance must be positive")
  if (max.iter <= 0) stop("Maximum iterations must be positive")

  if (intercept) X <- cbind(1, X)

  # Handle extreme cases with warning
  if (all(Y == 0)) warning("All observations are failures - model may be unstable")
  if (all(Y == n)) warning("All observations are successes - model may be unstable")

  # Initialize with transformed response
  Y.trf <- ifelse(Y == n, n - 0.01, Y)
  Y.trf <- ifelse(Y == 0, 0.01, Y.trf)
  Y.trf <- log(Y.trf/(n - Y.trf))

  # Check for perfect separation
  if (any(is.infinite(Y.trf))) {
    warning("Perfect separation detected - coefficients may diverge")
  }

  beta.t <- tryCatch(
    solve(t(X) %*% X) %*% t(X) %*% Y.trf,
    error = function(e) {
      stop("Design matrix is singular - check for collinear predictors")
    }
  )

  # Newton-Raphson iterations
  iter <- 0
  converged <- FALSE
  while (iter < max.iter && !converged) {
    iter <- iter + 1
    der1 <- beta1der(X, Y, beta.t, n)
    der2 <- beta2der(X, beta.t, n)

    tryCatch({
      new.beta <- beta.t - solve(der2) %*% der1
    }, error = function(e) {
      stop("Hessian matrix is singular at iteration ", iter,
           " - check for collinearity or separation")
    })

    if (mean(abs(new.beta - beta.t)) < tol) {
      converged <- TRUE
    }
    beta.t <- new.beta
  }

  if (!converged) {
    warning("Algorithm failed to converge within ", max.iter, " iterations")
  }

  beta.t
}
#' Wald Test for Binomial Regression Coefficients
#'
#' Performs Wald tests for linear hypotheses.
#'
#' @param X Design matrix used in model
#' @param beta.mle Vector of MLE estimates
#' @param n Vector of binomial denominators
#' @param contrast Contrast matrix specifying hypothesis
#' @param null.value Null value to test against (default: 0)
#' @return A list containing chi-square statistic, df, and p-value
#' @export
#' @examples
#' data(mtcars)
#' # Model without intercept
#' X <- model.matrix(~ 0 + hp + wt, data = mtcars)
#' Y <- mtcars$vs
#' n <- rep(1, nrow(mtcars))
#' fit <- binomial_reg(X, Y, n, intercept = FALSE)
#'
#' # Wald test for mpg coefficient
#' contrast <- matrix(0, nrow = 1, ncol = ncol(X))
#' contrast[1, 2] <- 1  # Test mpg coefficient
#' wald_test(X, fit, n, contrast)
wald_test <- function(X, beta.mle, n, contrast, null.value = 0) {
  # Input validation
  if (!is.matrix(X)) stop("X must be a numeric matrix")
  if (!is.numeric(beta.mle)) stop("beta.mle must be a numeric vector")
  if (!is.numeric(n)) stop("n must be a numeric vector")
  if (!is.matrix(contrast)) stop("contrast must be a matrix")
  if (ncol(contrast) != length(beta.mle)) {
    stop("Number of columns in contrast must match length of beta.mle")
  }
  if (length(null.value) != 1 || !is.numeric(null.value)) {
    stop("null.value must be a single numeric value")
  }

  # Safe computation with error handling
  result <- tryCatch({
    l <- contrast %*% beta.mle - null.value
    hessian <- -beta2der(X, beta.mle, n)
    info.inv <- solve(hessian)
    m <- contrast %*% info.inv %*% t(contrast)

    # Calculate p-value
    1 - pchisq(l %*% solve(m) %*% l, nrow(contrast))
  }, error = function(e) {
    stop("Wald test computation failed: ", e$message)
  })

  return(result)
}
