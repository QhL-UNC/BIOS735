#' Bayesian Logistic Regression via Metropolis-Hastings MCMC
#'
#' Fits a Bayesian logistic regression model using random walk Metropolis-Hastings MCMC.
#' Uses mtcars data as an example implementation.
#'
#' @importFrom stats dnorm runif
#'
#' @param X Numeric matrix of predictors (n x p)
#' @param y Binary response vector (TRUE/FALSE or 0/1) of length n
#' @param N Total number of MCMC iterations
#' @param proposal_sd Standard deviation for random walk proposals
#' @param prior Either NULL for flat priors or a list containing:
#' \itemize{
#'   \item prior_mean: Vector of prior means
#'   \item prior_sd: Vector of prior standard deviations
#'   \item prior_indices: Indices of coefficients to apply priors (1 = intercept)
#' }
#' @param cat Logical indicating whether to show progress messages (default: FALSE)
#' 
#' @return Matrix of MCMC samples (N x p) with columns named:
#' \itemize{
#'   \item beta_0: Intercept
#'   \item beta_1: First predictor
#'   \item ...: Subsequent predictors
#' }
#' @export
#'
#' @examples
#' # Using mtcars data - predict automatic transmission (am = 1)
#' data(mtcars)
#' X <- as.matrix(mtcars[, c("hp", "wt")])  # Predictors
#' y <- mtcars$am == 1                      # Binary outcome
#'
#' # Define informative priors (for intercept, hp, and wt)
#' prior_example <- list(
#'   prior_mean = c(-5, 0.02, -1),   # Prior means
#'   prior_sd = c(2, 0.01, 0.5),     # Prior standard deviations
#'   prior_indices = c(1, 2, 3)       # Apply to intercept (1), hp (2), wt (3)
#' )
#'
#' # Run MCMC (short chain for example)
#' set.seed(123)
#' mcmc_samples <- bayes_logistic(
#'   X = X,
#'   y = y,
#'   N = 5000,
#'   proposal_sd = 0.1,
#'   prior = prior_example,
#' )
#'
#' # Check posterior means
#' colMeans(mcmc_samples)
#'
#' # Trace plot for hp coefficient
#' plot(mcmc_samples[, "beta_1"], type = "l",
#'      main = "Trace plot: Horsepower (hp) coefficient",
#'      ylab = "Parameter value")

bayes_logistic <- function(X, y, N, proposal_sd, prior, cat = FALSE) {
  # 1. Check X is a numeric matrix
  if (!is.matrix(X) || !is.numeric(X)) {
    stop("X must be a numeric matrix.")
  }
  
  # 2. Check length of y equals rows of X
  if (length(y) != nrow(X)) {
    stop(
      "Length of y (", length(y), ") ", 
      "does not match number of rows in X (", nrow(X), ")."
    )
  }
  
  # 3. Check MCMC iterations (N)
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N %% 1 != 0) {
    stop("N must be a positive integer (number of MCMC iterations).")
  }
  
  # 4. Check proposal_sd is valid
  if (!is.numeric(proposal_sd) || length(proposal_sd) != 1 || proposal_sd <= 0) {
    stop("proposal_sd must be a single positive numeric value.")
  }
  
  # 5. Validate prior structure (if provided)
  if (!missing(prior) && !all(is.na(prior))) {
    if (!is.list(prior) || length(prior) != 3) {
      stop("prior must be a list of 3 elements: prior_mean (numeric), prior_sd (numeric), prior_indices (integer)")
    }
    
    # Check SDs are positive
    if (any(prior[[2]] <= 0)) {
      stop("All prior_sd values must be positive")
    }
  }

  # ==================== MODEL SETUP ====================
  # Add intercept and initialize
  X <- cbind(1, X)
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)
  beta_mcmc <- matrix(NA, nrow = N, ncol = p)
  accept_counts <- rep(0, N)
  
  # ==================== MCMC CORE ====================
  # Corrected log_posterior function
  log_posterior <- function(beta) {
    eta <- X %*% beta
    
    # Vectorized log-likelihood calculation with overflow protection
    log_lik <- sum(
      ifelse(y,
             ifelse(eta > 20, eta, eta - log1p(exp(eta))),
             ifelse(eta < -20, -eta, -log1p(exp(eta)))
      )
    )
    
    # Initialize log_prior properly
    log_prior <- 0
    
    # Add prior contribution if specified
    if (!all(is.na(prior))) {
      prior_mean <- prior[[1]]
      prior_sd <- prior[[2]]
      prior_indices <- prior[[3]]
      
      for (i in seq_along(prior_indices)) {
        j <- prior_indices[i]
        log_prior <- log_prior + dnorm(beta[j], prior_mean[i], prior_sd[i], log = TRUE)
      }
    }
    
    return(log_lik + log_prior)
  }
  
  c_post_lik <- log_posterior(beta)
  if (is.infinite(c_post_lik)) {
    stop("Initial log-posterior is infinite - check predictors/priors")
  }
  
  # ==================== MCMC LOOP ====================
  for (i in 1:N) {
    beta_prop <- beta + rnorm(p, 0, proposal_sd)
    
    # Numerical stability check
    if (any(is.na(beta_prop))) {
      warning("NA in proposal - skipping iteration ", i)
      beta_mcmc[i,] <- beta
      next
    }
    
    p_post_lik <- log_posterior(beta_prop)
    
    # Metropolis-Hastings
    if (!is.na(p_post_lik)) {
      log_accept_ratio <- p_post_lik - c_post_lik
      if (log(runif(1)) < log_accept_ratio) {
        beta <- beta_prop
        c_post_lik <- p_post_lik
        accept_counts[i] <- 1
      }
    }
    
    beta_mcmc[i,] <- beta
    
    # Progress reporting
    if (cat && (i %% 1000 == 0)) {
      message(sprintf("Iteration %d (Acceptance: %.1f%%)", 
                      i, 100*mean(accept_counts[1:i])))
    }
  }
  
  # ==================== POST-PROCESSING ====================
  accept_rate <- mean(accept_counts)
  if (accept_rate < 0.15 || accept_rate > 0.5) {  # Widen acceptable range
    warning(sprintf("Final acceptance rate %.1f%% (target 15-50%%)", 100*accept_rate))
  }
  
  if (any(is.na(beta_mcmc))) {
    warning("NA values detected in MCMC output - check for numerical instability")
  }
  
  colnames(beta_mcmc) <- paste0("beta_", 0:(p-1))
  return(beta_mcmc)
}
