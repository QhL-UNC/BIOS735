# Setup test data
data(mtcars)
X <- as.matrix(mtcars[, c("hp", "wt")])
y <- mtcars$am == 1
prior_example <- list(
  prior_mean = c(-5, 0.02, -1),
  prior_sd = c(2, 0.01, 0.5),
  prior_indices = c(1, 2, 3)  # intercept + 2 predictors
)
test_that("produces valid MCMC output", {
  set.seed(123)
  # Test with flat priors (using NA for no prior)
  mcmc_flat <- bayes_logistic(X, y, N = 100, proposal_sd = 0.1, prior = NA)
  expect_true(is.matrix(mcmc_flat))
  expect_equal(dim(mcmc_flat), c(100, 3))
  expect_equal(colnames(mcmc_flat), c("beta_0", "beta_1", "beta_2"))

  # Test with informative priors
  mcmc_prior <- bayes_logistic(X, y, N = 100, proposal_sd = 0.02, prior = prior_example)
  expect_equal(dim(mcmc_prior), c(100, 3))
})

test_that("handles input validation correctly", {
  # Invalid X
  expect_error(bayes_logistic(as.data.frame(X), y, N = 100, proposal_sd = 0.05),
               "X must be a numeric matrix")

  # Dimension mismatch
  expect_error(bayes_logistic(X, y[1:10], N = 100, proposal_sd = 0.05),
               "Length of y \\(10\\) does not match number of rows in X \\(32\\)")

  # Invalid iterations
  expect_error(bayes_logistic(X, y, N = -100, proposal_sd = 0.05),
               "N must be a positive integer")

  # Invalid prior structure
  bad_prior <- list(prior_mean = c(0,0))  # incomplete prior
  expect_error(bayes_logistic(X, y, N = 100, proposal_sd = 0.05, prior = bad_prior),
               "prior must be a list of 3 elements")
})

test_that("produces reasonable parameter estimates", {
  set.seed(123)
  mcmc_samples <- bayes_logistic(X, y, N = 500, proposal_sd = 0.05, prior = NA)

  # Check posterior means are finite
  post_means <- colMeans(mcmc_samples)
  expect_true(all(is.finite(post_means)))

})

test_that("responds to prior specification", {
  set.seed(123)
  strong_prior <- list(
    prior_mean = c(0, 0, 0),  # Prior centered at 0
    prior_sd = c(0.1, 0.01, 0.1),  # Tight priors
    prior_indices = 1:3
  )

  mcmc_strong <- bayes_logistic(X, y, N = 500, proposal_sd = 0.02, prior = strong_prior)
  post_means_strong <- colMeans(mcmc_strong)

  # Compare with flat prior results
  mcmc_flat <- bayes_logistic(X, y, N = 500, proposal_sd = 0.05, prior = NA)
  post_means_flat <- colMeans(mcmc_flat)

  # Prior should shrink estimates toward 0
  expect_true(mean(abs(post_means_strong)) < mean(abs(post_means_flat)))
})
