# -------------------------------------------------------------------
# Purpose: This R script is for SVC model for BIOS 735 Group Project
# Author: Qinghua Li
# Date: 4.19.2025
# Email: qinghua_li@med.unc.edu
# Note: 
#     1. Wine color and binary wine quality will be used as outcomes,
#        respectively
#     2. Support Vector Classification (SVC) will be used for both
#        outcomes
#     3. Dataset was split into training and test at a ratio 8:2. First, 
#        training subset will be used to train the model using cross 
#        validation, and the final model will be tested using test subset.
# -------------------------------------------------------------------


# ---- libraries ----
library(tidyverse)     # data management
library(caret)         # SVM model and tuning 
library(e1071)         # SVM model
library(ggplot2)


# ---- paths & data ----
path <- getwd()
wine_train <- read.csv(file = paste0(path, "/Data/wine_train_scaled.csv")) %>% 
  mutate(color = as.factor(color),
         quality_7 = as.factor(quality_7))
wine_test <- read.csv(file = paste0(path, "/Data/wine_test_scaled.csv")) %>% 
  mutate(color = as.factor(color),
         quality_7 = as.factor(quality_7))


# ---- models ----
# 1. SVM with linear classifier 
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 1.1 Wine color as outcome
svm1 <- train(color ~ . - quality_7 - quality, 
              data = wine_train, 
              method = "svmLinear",
              trControl = train_control,
              tuneGrid = expand.grid(C = seq(0.1, 5, length = 20)))


best_c <- svm1$bestTune$C
svm_final <- svm(color ~ . -quality_7 - quality, 
                 data = wine_test, 
                 kernel = "linear",
                 cost = best_c,
                 type = "C-classification")

# Fitted values
fitted_values <- svm_final$fitted

# Observed values
observed_value <- wine_test %>% select(color) %>% pull() %>% as.factor()

# Confusion matrix
cm <- confusionMatrix(fitted_values, observed_value)
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
cat("Precision is ", precision, ", and recall is", recall)



# 1.2 Wine quality (binary) as outcome
svm1 <- train(quality_7 ~ . - color - quality, 
              data = wine_train, 
              method = "svmLinear",
              trControl = train_control,
              tuneGrid = expand.grid(C = seq(0.1, 5, length = 20)))


best_c <- svm1$bestTune$C
svm_final <- svm(quality_7 ~ . - color - quality, 
                 data = wine_test, 
                 kernel = "linear",
                 cost = best_c,
                 type = "C-classification")

# Fitted values
fitted_values <- svm_final$fitted

# Observed values
observed_value <- wine_test %>% select(quality_7) %>% pull() %>% as.factor()

# Confusion matrix
cm <- confusionMatrix(fitted_values, observed_value, positive = "1")
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
cat("Precision is ", precision, ", and recall is", recall)
print(cm)



# ---- radial kernel -----
# 1.1 Wine color as outcome
svm1 <- train(color ~ . - quality_7 - quality, 
              data = wine_train, 
              method = "svmRadial",
              trControl = train_control,
              tuneLength = 5)

# Fitted values
pred <- predict(svm1, newdata = wine_test)

# Observed values
observed_value <- wine_test %>% select(color) %>% pull() %>% as.factor()

# Confusion matrix
cm <- confusionMatrix(pred, observed_value)
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
cat("Precision is ", precision, ", and recall is", recall)

# 1.2 Wine quality (binary) as outcome
svm1 <- train(quality_7 ~ . - color - quality, 
              data = wine_train, 
              method = "svmRadial",
              trControl = train_control,
              tuneLength = 5)

# predicted values
pred <- predict(svm1, newdata = wine_test)

# Observed values
observed_value <- wine_test %>% select(quality_7) %>% pull() %>% as.factor()

# Confusion matrix
cm <- confusionMatrix(pred, observed_value, positive = "1")
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
cat("Precision is ", precision, ", and recall is", recall)
print(cm)



# ---- simulated data -----
set.seed(2)
n <- 100
x1 <- c(rnorm(n),rnorm(n,5))
x2 <- c(rnorm(n),rnorm(n,5))
x1 <- scale(x1)
x2 <- scale(x2)
y <- factor(rep(c(-1,1),each=n))
dat <- data.frame(y,x1,x2)
x <- data.frame(x1,x2)

s <- seq(from=-2,to=2,length=400)

rfit <- train(x, y, method="svmRadial")
rsv <- as.data.frame(x[rfit$finalModel@SVindex,])
grid <- expand.grid(x1=s,x2=s)
grid$y <- predict(rfit, newdata=grid)
grid$yy <- 2*(as.numeric(grid$y) - 1.5)
ggplot(dat, aes(x1,x2,col=y)) + geom_point() + 
  geom_point(data=rsv, col="black", size=5, shape=21) +
  geom_contour(data=grid, aes(x1,x2,z=yy), breaks=0, col="black") +
  geom_raster(data=grid, aes(x1,x2,fill=y), alpha=.2) + 
  theme_minimal()