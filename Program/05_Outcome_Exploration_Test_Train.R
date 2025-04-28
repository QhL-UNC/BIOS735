# -------------------------------------------------------------------
# Purpose: Outcome exploration for BIOS 735 Group Project
# Author: Qinghua Li
# Date: 4.21.2025
# Email: qinghua_li@med.unc.edu
# Note: 
#     1. Wine color and binary wine quality % in test and training
#     2. Wine color and wine quality in total dataset
# -------------------------------------------------------------------


# ---- libraries ----
library(tidyverse)     # data management
library(ggplot2)       # plot
library(table1)        # Summary statistics


# ---- paths & data ----
path <- getwd()
wine_train <- read.csv(file = paste0(path, "/Data/wine_train_scaled.csv")) %>% 
  mutate(color = as.factor(color),
         quality_7 = as.factor(quality_7))
wine_test <- read.csv(file = paste0(path, "/Data/wine_test_scaled.csv")) %>% 
  mutate(color = as.factor(color),
         quality_7 = as.factor(quality_7))

wine_df <- bind_rows(wine_test, wine_train)

# ---- outcome exploration ---- 
# 1. Total data set 
# 1.1 Wine color 
table1(~ color, data = wine_df)

# 1.2 Binary wine quality 
table1(~ quality_7, data = wine_df)

# 1.3 wine quality histogram
wine_df %>% 
  ggplot(aes(x = quality)) + 
  geom_histogram(binwidth = 1, color = "gray", fill = "darkgray") + 
  labs(x = "Wine quality",
       y = "Count",
       title = "Wine quality histogram using all observations") + 
  theme_minimal()

# 2. Training set
table1(~ color, data = wine_train)
table1(~ quality_7, data = wine_train)
table1(~ quality, data = wine_train)

wine_train %>% 
  ggplot(aes(x = quality)) + 
  geom_histogram(binwidth = 1, color = "gray", fill = "darkgray") + 
  labs(x = "Wine quality",
       y = "Count",
       title = "Wine quality histogram using training set") + 
  theme_minimal()

# 3. Test set
table1(~ color, data = wine_test)
table1(~ quality_7, data = wine_test)
table1(~ quality, data = wine_test)

wine_test %>% 
  ggplot(aes(x = quality)) + 
  geom_histogram(binwidth = 1, color = "gray", fill = "darkgray") + 
  labs(x = "Wine quality",
       y = "Count",
       title = "Wine quality histogram using test set") + 
  theme_minimal()
