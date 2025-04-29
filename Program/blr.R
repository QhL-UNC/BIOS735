
# library(devtools)
# load_all("pkg")

source("Program/02_DataSplit.R")

# add binary quality to train sets
wine_white_train$qual_bin <- wine_white_train$quality > 6
wine_red_train$qual_bin <- wine_red_train$quality > 6
wine_train$qual_bin <- wine_train$quality > 6

N <- 1e5

X_w <- as.matrix(wine_white_train[, -c("quality", "qual_bin", "color")])
y_w <- wine_white_train$qual_bin

X_r <- as.matrix(wine_red_train[, -c("quality", "qual_bin", "color")])
y_r <- wine_red_train$qual_bin

X_c <- as.matrix(wine_train[, -c("quality", "qual_bin", "color")])
y_c <- (wine_train$color == "white")

prior <- "inexpert"
if (prior == "inexpert") {
  # white wine prior for binary quality using 5
  prior_mean <- c(-1/sd(wine_white_train$`fixed acidity`), 
                  -1/sd(wine_white_train$`volatile acidity`),
                  -1/sd(wine_white_train$`citric acid`),
                  1/sd(wine_white_train$`residual sugar`),
                  0, 0, 0, 
                  -1/sd(wine_white_train$`density`),
                  -1/sd(wine_white_train$`pH`),
                  0,
                  -1/sd(wine_white_train$`alcohol`))
  prior_sd <- rep(0.1, 11)
  prior_indices <- 1:11
  prior_w <- list(prior_mean, prior_sd, prior_indices)
  
  # red wine prior for binary quality
  prior_mean <- c(1/sd(wine_white_train$`fixed acidity`), 
                  1/sd(wine_white_train$`volatile acidity`),
                  1/sd(wine_white_train$`citric acid`),
                  -1/sd(wine_white_train$`residual sugar`),
                  0, 0, 0, 
                  1/sd(wine_white_train$`density`),
                  1/sd(wine_white_train$`pH`),
                  0,
                  1/sd(wine_white_train$`alcohol`))
  prior_sd <- rep(0.1, 11)
  prior_indices <- 1:11 
  prior_r <- list(prior_mean, prior_sd, prior_indices)
  
  # color wine prior for binary color, 
  # negative is for red wines, positive is for whites
  prior_mean <- c(-1/sd(wine_white_train$`fixed acidity`), 
                  -1/sd(wine_white_train$`volatile acidity`),
                  -1/sd(wine_white_train$`citric acid`),
                  1/sd(wine_white_train$`residual sugar`),
                  -1/sd(wine_white_train$`chlorides`),
                  0, 0, 
                  -1/sd(wine_white_train$`density`),
                  -1/sd(wine_white_train$`pH`),
                  0,
                  0)
  prior_sd <- rep(0.1, 11)
  prior_indices <- 1:11 
  prior_c <- list(prior_mean, prior_sd, prior_indices)
} else {
  prior_mean <- rep(0, 11)
  prior_sd <- rep(10, 11)
  prior_indices <- 1:11
}

proposal_sd <- 0.5 # tuned for good accept rate

white_mcmc_inf_pr <- bayes_logistic_mcmc(X_w, y_w, N, proposal_sd, prior_w, TRUE)
white_mcmc_non_pr <- bayes_logistic_mcmc(X_w, y_w, N, proposal_sd, NA, TRUE)

red_mcmc_inf_pr <- bayes_logistic_mcmc(X_r, y_r, N, proposal_sd, prior_r, TRUE)
red_mcmc_non_pr <- bayes_logistic_mcmc(X_r, y_r, N, proposal_sd, NA, TRUE)

color_mcmc_inf_pr <- bayes_logistic_mcmc(X_c, y_c, N, proposal_sd, prior_c, TRUE)
color_mcmc_non_pr <- bayes_logistic_mcmc(X_c, y_c, N, proposal_sd, NA, TRUE)
# 
