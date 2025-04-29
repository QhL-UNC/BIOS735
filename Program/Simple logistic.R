library(tidyverse)
library(caret)
train_data <- read.csv("wine_train_scaled.csv")
test_data <- read.csv("wine_test_scaled.csv")

formula <- as.formula(
  paste(
    "(color=='red') ~",
    paste(setdiff(names(train_data), c("quality_7","color","quality")), collapse = " + ")
  )
)
formula1 <- as.formula(
  paste(
    "quality_7 ~",
    paste(setdiff(names(train_data), c("quality_7","quality","color")), collapse = " + ")
  )
)

#Logistic
model<-glm(formula,family = "binomial",train_data)
predicted_classes<-ifelse(predict(model,test_data, type = "response")>0.5,"red","white")
# make sure they’re factors with same levels:
pred_f <- factor(predicted_classes, levels = c("red","white"))
act_f  <- factor(test_data$color,    levels = c("red","white"))

# compute with “red” as positive:
cm <- confusionMatrix(pred_f, act_f, positive = "red")

# extract:
cm$overall["Accuracy"]
cm$byClass["Pos Pred Value"]  
cm$byClass["Sensitivity"]
cm$table

# 1. Extract absolute coefficients
imp <- abs(coef(model)[-1])

# 2. Build and order the data frame
imp_df <- data.frame(
  predictor  = names(imp),
  importance = as.numeric(imp),
  row.names  = NULL
)
imp_df <- imp_df[order(imp_df$importance, decreasing = TRUE), ]

# 3. Convert predictor to factor for plotting order
imp_df$predictor <- factor(imp_df$predictor, levels = imp_df$predictor)

ggplot(imp_df, aes(x = predictor, y = importance, fill = importance)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low  = "purple",
    high = "blue",
    guide = FALSE
  ) +
  labs(
    x     = "Predictor",
    y     = "Absolute Coefficient",
    title = "Variable Importance from Scaled Logistic Model"
  ) +
  theme_minimal(base_size = 14)



#Logistic
model<-glm(formula1,family = "binomial",train_data)
# make sure they’re factors with same levels:
probs <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(probs > 0.5, "1", "0")
pred_f <- factor(predicted_classes)
act_f  <- factor(test_data$quality_7)

# compute with “red” as positive:
cm <- confusionMatrix(pred_f, act_f, positive = "1")

# extract:
cm$overall["Accuracy"]
cm$byClass["Pos Pred Value"]  #precision
cm$byClass["Sensitivity"]   #recall
cm$table


# 1. Extract absolute coefficients
imp <- abs(coef(model)[-1])

# 2. Build and order the data frame
imp_df <- data.frame(
  predictor  = names(imp),
  importance = as.numeric(imp),
  row.names  = NULL
)
imp_df <- imp_df[order(imp_df$importance, decreasing = TRUE), ]

# 3. Convert predictor to factor for plotting order
imp_df$predictor <- factor(imp_df$predictor, levels = imp_df$predictor)

ggplot(imp_df, aes(x = predictor, y = importance, fill = importance)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low  = "purple",
    high = "blue",
    guide = FALSE
  ) +
  labs(
    x     = "Predictor",
    y     = "Absolute Coefficient",
    title = "Variable Importance from Scaled Logistic Model"
  ) +
  theme_minimal(base_size = 14)