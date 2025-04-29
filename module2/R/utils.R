# Internal Utility Functions for module2 Package

#' Validate Design Matrix
#'
#' Checks if input is a valid numeric matrix with no missing values.
#' @param X Design matrix to validate
#' @noRd
validate_matrix <- function(X) {
  if (!is.matrix(X)) {
    stop("Design matrix must be a numeric matrix", call. = FALSE)
  }
  if (!is.numeric(X)) {
    stop("Design matrix must contain only numeric values", call. = FALSE)
  }
  if (anyNA(X)) {
    stop("Design matrix contains missing values (NA)", call. = FALSE)
  }
}

#' Check Binary Response Vector
#'
#' Validates that a vector contains only 0s and 1s.
#' @param y Response vector to check
#' @noRd
check_binary <- function(y) {
  if (!all(y %in% c(0, 1))) {
    stop("Response vector must contain only 0s and 1s", call. = FALSE)
  }
}

#' Check Count Response Vector
#'
#' Validates counts are non-negative and don't exceed trials.
#' @param y Count vector
#' @param n Trials vector
#' @noRd
check_counts <- function(y, n) {
  if (any(y < 0)) {
    stop("Counts cannot be negative", call. = FALSE)
  }
  if (any(y > n)) {
    stop("Counts cannot exceed trial numbers", call. = FALSE)
  }
}

#' Validate Prior Specification
#'
#' Checks proper structure of Bayesian prior specification.
#' @param prior Prior list object
#' @param p Number of predictors (including intercept)
#' @noRd
validate_prior <- function(prior, p) {
  if (!all(c("prior_mean", "prior_sd", "prior_indices") %in% names(prior))) {
    stop("Prior must contain 'prior_mean', 'prior_sd', and 'prior_indices'", call. = FALSE)
  }
  if (length(prior$prior_mean) != length(prior$prior_sd)) {
    stop("Prior means and standard deviations must have equal length", call. = FALSE)
  }
  if (any(prior$prior_sd <= 0)) {
    stop("Prior standard deviations must be positive", call. = FALSE)
  }
  if (any(prior$prior_indices < 1 | prior$prior_indices > p)) {
    stop("Prior indices must be between 1 and ", p, call. = FALSE)
  }
}

#' Safe Logistic Function
#'
#' Numerically stable implementation of logistic function.
#' @param x Input value
#' @return Logistic transformation
#' @noRd
safe_logistic <- function(x) {
  ifelse(x > 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))
}

#' Check for Separation
#'
#' Detects perfect separation in logistic/binomial models.
#' @param X Design matrix
#' @param y Response vector
#' @return Logical indicating if separation exists
#' @noRd
check_separation <- function(X, y) {
  tryCatch({
    fit <- glm.fit(cbind(1, X), y, family = binomial())
    FALSE
  }, warning = function(w) {
    grepl("fitted probabilities numerically 0 or 1 occurred", w$message)
  })
}
