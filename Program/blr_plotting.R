library(ggplot2)

plot_mcmc_trace <- function(mcmc_samp, plot_title) {
  x <- 1:1e5
  
  df <- as.data.frame(mcmc_samp)
  
  colnames(df) <- c("Intercept", "Fixed Acidity", "Volatile Acidity",
                    "Citric Acid", "Residual Sugar", "Chlorides",
                    "Free SO2", "Total SO2", "Density", "pH",
                    "Sulphates", "Alcohol")
  
  df <- df %>%
    mutate(iter = 1:n())
  
  df_long <- df %>%
    pivot_longer(
      cols = -iter,
      names_to = "param",
      values_to = "est"
    )
  
  ggplot(df_long, aes(x = iter, y = est, col = param)) +
    geom_line(alpha = 0.8) + 
    labs(
      x = "Iteration",
      y = "Parameter Estimate",
      title = plot_title,
      color = "Parameter"
    ) +
    theme_minimal()
}

plot_mcmc_trace(white_mcmc_inf_pr, 
                "MCMC Trace Plot for Predicting White Wine\nQuality with Informative Prior")
plot_mcmc_trace(white_mcmc_non_pr, 
                "MCMC Trace Plot for Predicting White Wine\nQuality with Non-Informative Prior")
plot_mcmc_trace(red_mcmc_inf_pr, 
                "MCMC Trace Plot for Predicting Red Wine\nQuality with Informative Prior")
plot_mcmc_trace(red_mcmc_non_pr, 
                "MCMC Trace Plot for Predicting Red Wine\nQuality with Non-Informative Prior")
plot_mcmc_trace(color_mcmc_inf_pr,
                "MCMC Trace Plot for Predicting Wine Color\nwith Informative Prior")
plot_mcmc_trace(color_mcmc_non_pr, 
                "MCMC Trace Plot for Predicting Wine Color\nwith Non-Informative Prior")

