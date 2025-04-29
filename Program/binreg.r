setwd("BIOS735")
require(tidyverse); require(devtools)
load_all("module2/")
data <- read.csv("Data/wine_train.csv") %>% 
  select(-color) %>%
  mutate(quality = quality - 3) %>%
  mutate(across(.cols = c(-quality), .fns = ~ (. - mean(.)) / sd(.)))

# 1. Model fitting
X <- data %>% select(-quality) %>% as.matrix()
Y <- data$quality %>% as.matrix()
(beta.mle <- binomial_reg(X, Y, 6))

# 2. Plot NR iterations
X1 <- cbind(1, X); tol <- 1e-10
Y.trf <- ifelse(Y == 6, 6 - 0.01, Y)
Y.trf <- ifelse(Y == 0, 0.01, Y.trf)
Y.trf <- log(Y.trf/(6 - Y.trf))
beta.t <- solve(t(X1)%*%X1)%*%t(X1)%*%Y.trf
beta.df <- beta.t
while (T) {
  der1 <- beta1der(X1, Y, beta.t, 6)
  der2 <- beta2der(X1, beta.t, 6)
  new.beta <- beta.t - solve(der2)%*%der1
  if (mean(abs(new.beta - beta.t)) < tol)
    break;
  beta.t <- new.beta
  beta.df <- cbind(beta.df, new.beta)
}
beta.df <- as.data.frame(beta.df)
colnames(beta.df) <- paste("Iter", 1:ncol(beta.df)-1)
hm.df <- beta.df %>% as.data.frame() %>% round(3) %>% 
  mutate(vars = c("intercept", rownames(beta.t)[-1])) %>%
  pivot_longer(cols = -vars)
hm.df$vars <- factor(hm.df$vars, levels = c("intercept", rownames(beta.t)[-1]))
hm.df$name <- factor(hm.df$name, levels = colnames(beta.df))
ggplot(hm.df, aes(x = vars, y = name, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white") +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(y = "Iteration", x = "Variables", 
       title = "Coefficient Estimates from Newton-Raphson at Each Iteration") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("Result/beta.png", width = 10, height = 3, device = "png")

# 3. Plot MAE
new.data <- read.csv("Data/wine_test.csv") %>% 
  select(-color) %>% mutate(quality = quality - 3) %>%
  mutate(across(.cols = c(-quality), .fns = ~ (. - mean(.)) / sd(.)))
X.new <- new.data %>% select(-quality) %>% as.matrix()
mean(abs(6/(1 + exp(-cbind(1, X.new) %*% beta.mle)) - new.data$quality))
Y.pred <- round(6/(1 + exp(-cbind(1, X.new) %*% beta.mle)))
mean(abs(Y.pred - new.data$quality))
pred.df <- new.data %>% mutate(quality.pred = Y.pred)
mae.df <- pred.df %>% group_by(quality) %>% 
  summarise(mae = mean(abs(quality.pred - quality)), count = n()) %>%
  mutate(count_scaled = count / max(count) * max(mae))
long_df <- mae.df %>% pivot_longer(cols = c(mae, count_scaled), names_to = "metric", values_to = "value")
ggplot(long_df, aes(x = factor(quality + 3), y = value, fill = metric)) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.7), width = .5) +
  scale_fill_manual(values = c("mae" = "green", "count_scaled" = "grey"),
                    labels = c("Count", "Mean Absolute Error")) +
  scale_y_continuous(
    name = "Mean Absolute Error",
    sec.axis = sec_axis(~ . * max(mae.df$count) / max(mae.df$mae), name = "Count")
  ) +
  labs(x = "Wine Quality", fill = "", 
       title = "Mean Absolute Prediction Error by Wine Quality Scores") +
  theme_minimal()
ggsave("Result/mae.png", width = 10, height = 4, device = "png")

# 4. p-values
pvals <- sapply(1:nrow(beta.mle), function(i) {
  contrast <- matrix(0, nrow = 1, ncol = nrow(beta.mle))
  contrast[1, i] <- 1
  wald_test(cbind(1, X), beta.mle, 6, contrast)
})
names(pvals) <- rownames(beta.mle)
# Significant p-values
pvals[pvals < 5e-2]
cols <- c("intercept", rownames(beta.mle)[-1])
pvals %>% tibble() %>% mutate(
  names = factor(cols, levels = cols),
  logpval = log10(`.`)
) %>%
  ggplot(aes(x=names, y=logpval)) + 
  geom_segment(aes(x = names, xend = names, y = 0, yend = logpval)) +
  geom_point() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  labs(x = "Features", y = "Log P-values", 
       title = "Wald Test Log P-Values for Each Feature")
ggsave("Result/pval.png", width = 10, height = 3, device = "png")
