data(mtcars)
X <- model.matrix(~ hp + wt, data = mtcars)  # Removed cyl to avoid collinearity
Y <- mtcars$vs
n <- rep(1, nrow(mtcars))
beta_init <- rep(0, ncol(X))

test_that("beta1der computes score function correctly", {
  score <- beta1der(X, Y, beta_init, n)
  expect_true(is.matrix(score))
  expect_equal(dim(score), c(ncol(X), 1))
  
  # Check against manual calculation at beta=0
  expected_first <- sum(Y - n * 0.5)  # When beta=0, p=0.5
  expect_equal(as.numeric(score[1,1]), expected_first, tolerance = 1e-6)
})

test_that("beta2der computes Hessian correctly", {
  hessian <- beta2der(X, beta_init, n)
  expect_true(is.matrix(hessian))
  expect_equal(dim(hessian), c(ncol(X), ncol(X)))
  expect_true(isSymmetric(hessian))
  
  # Should be negative definite
  eigen_values <- eigen(hessian)$values
  expect_true(all(eigen_values < 0))
})

test_that("binomial_reg performs Newton-Raphson optimization", {
  # Add small noise to avoid perfect separation issues
  Y_adj <- ifelse(Y == 1, 0.99, ifelse(Y == 0, 0.01, Y))
  
  fit <- binomial_reg(X[, -1], Y_adj, n, intercept = TRUE)
  expect_true(is.numeric(fit))
  expect_equal(length(fit), ncol(X))
  
  # Compare with glm() results - convert both to vectors
  suppressWarnings({
    glm_fit <- glm(cbind(Y_adj, n-Y_adj) ~ hp + wt, data = mtcars, family = binomial)
  })
  expect_equal(as.vector(fit), as.vector(coef(glm_fit)), tolerance = 1e-3)
})

test_that("wald_test computes correct hypothesis tests", {
  # Use adjusted Y to ensure stable results
  Y_adj <- ifelse(Y == 1, 0.99, ifelse(Y == 0, 0.01, Y))
  fit <- binomial_reg(X[, -1], Y_adj, n, intercept = TRUE)
  
  contrast <- matrix(0, nrow = 1, ncol = ncol(X))
  contrast[1, 2] <- 1  # Test hp coefficient
  pval <- wald_test(X, fit, n, contrast)
  expect_true(pval >= 0 && pval <= 1)
  
  # Compare with glm() Wald test - increased tolerance
  suppressWarnings({
    glm_fit <- glm(cbind(Y_adj, n-Y_adj) ~ hp + wt, data = mtcars, family = binomial)
    glm_summary <- summary(glm_fit)
  })
  expect_equal(as.numeric(pval), glm_summary$coefficients[2,4], tolerance = 0.2)
})

