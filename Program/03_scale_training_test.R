# -------------------------------------
# Scale the training and test subsets
# 4/17/2025

# ---- load in library ----
library(tidyverse)
library(readr)

# ---- load in data ----
path <- getwd()
data_path <- paste0(path, "/Data/")

wine_train_scaled <- read.csv(file = paste0(data_path, "wine_train.csv")) %>% 
  mutate_at(c(1:11), ~as.numeric(scale(.)))

wine_test_scaled <- read.csv(file = paste0(data_path, "wine_test.csv")) %>% 
  mutate_at(c(1:11), ~as.numeric(scale(.)))

write_csv(wine_test_scaled, file = paste0(data_path, "wine_test_scaled.csv"))
write_csv(wine_train_scaled, file = paste0(data_path, "wine_train_scaled.csv"))
