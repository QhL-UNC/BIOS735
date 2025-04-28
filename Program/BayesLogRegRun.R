# code transformations
wine_white_train$qual_bin <- wine_white_train$quality > 6
wine_red_train$qual_bin <- wine_red_train$quality > 6

# simulation parameters
N <- 1e5
proposal_sd <- 0.1
X_w <- as.matrix(wine_white_train[, -c("quality", "qual_bin", "color")])
y_w <- wine_white_train$qual_bin

X_r <- as.matrix(wine_red_train[, -c("quality", "qual_bin", "color")])
y_r <- wine_red_train$qual_bin

X_q <- as.matrix(wine_train[, -c("quality", "color")])
y_q <- (wine_train$color == "red")


# prior def
# https://etd.ohiolink.edu/acprod/odb_etd/ws/send_file/send?accession=ysu1298055470&disposition=inline
table(wine_white_train$quality)
table(wine_red_train$quality)
# based off above model where quality 8 is treated as reference group, use model from quality 5 comparison as prior

# intercept, alc, sulphase, volatile acidity, chloride, total sulfur dioxide
# free sulfur dioxide, pH

# informative normal prior from previous study, noninformative priors
# for all variables not included within their analysis
prior <- "norm"
if (prior == "norm") {
  # white wine prior for binary quality using 5
  prior_mean <- c(9.892, -2.185, -6.683, 2.125, 39.505, 0.040, -0.033, 5.200)
  prior_sd <- sqrt(3918)*c(1.723, 0.262, 1.520, 1.947, 16.531, 0.016, 0.039, 1.896)
  prior_indices <- c(1, 11, 10, 2, 5, 7, 6, 9)  
  prior_w <- list(prior_mean, prior_sd, prior_indices)
  
  # red wine prior for binary quality
  prior_mean <- c(-881.509, -1.577, 6.123, -0.475, -0.19, 967.449, -13.315, 0.776, -1.699)
  prior_sd <- sqrt(1279)*c(1.563, 1.021, 5.417, 0.300, 0.035, 690.205, 0.933, 4.711, 0.380)
  prior_indices <- c(1, 11, 10, 2, 5, 7, 6, 9)  
  prior_r <- list(prior_mean, prior_sd, prior_indices)
} 

red_mcmc_inf_pr <- mcmc_run(X_w, y_w, N, proposal_sd = 0.1, prior_w, TRUE)
white_mcmc_inf_pr <- mcmc_run(X_w, y_w, N, proposal_sd = 0.1, prior_r, TRUE)

red_mcmc_no_pr <- mcmc_run(X_r, y_r, N, proposal_sd = 0.1, NA, TRUE)
white_mcmc_no_pr <- mcmc_run(X_r, y_r, N, proposal_sd = 0.1, NA, TRUE)

color_mcmc <- mcmc_run(X_q, y_q, N, proposal_sd = 0.1, NA, TRUE)



# some plotting tools 
# x <- 1:1e6
# plot(x, beta_mcmc[,1], type = "l")
# for (i in 2:12) {
#   lines(x, beta_mcmc[, i], type = "l", col = i)
# }