mcmc_run <- function(X, y, N, proposal_sd, prior, cat = FALSE) {
  
  # add intercept
  X <- cbind(1, X)
  n <- nrow(X)
  p <- ncol(X)
  
  # initialize betas 
  beta <- rep(0, p)
  beta_mcmc <- matrix(NA, nrow = N, ncol = p)
  
  log_posterior <- function(beta) {
    p <- X %*% beta 
    log_lik <- sum(p * y - log(1 + exp(p)))
    log_prior <- 0
    if (!all(is.na(prior))) {
      prior_mean <- prior[[1]]
      prior_sd <- prior[[2]]
      prior_indices <- prior[[3]]
      for (i in seq_along(prior_indices)) {
        j <- prior_indices[i]
        log_prior <- log_prior + dnorm(beta[j], mean = prior_mean[i],
                                       sd = prior_sd[i], log = TRUE)
      }
    }
    return(log_lik + log_prior)
  }
  
  c_post_lik <- log_posterior(beta)
  for (i in 1:N) {
    beta_prop <- beta + rnorm(p, mean = 0, sd = proposal_sd)
    p_post_lik <- log_posterior(beta_prop)
    
    # Metropolis-Hastings acceptance
    log_accept_ratio <- p_post_lik - c_post_lik
    if (log(runif(1)) < log_accept_ratio) {
      beta <- beta_prop
      c_post_lik <- p_post_lik
    }
    beta_mcmc[i, ] <- beta
    if (cat & (i %% 1000 == 0)) {
      print(i)
    }
  }
  colnames(beta_mcmc) <- paste0("beta_", 0:(p - 1))
  
  return(beta_mcmc)
}

