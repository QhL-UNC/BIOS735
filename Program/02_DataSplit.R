# -----------------------------------------------------------
# Purpose: Split data into train vs. test with 8:2
# Author: Qinghua Li
# Email: qinghua_li@med.unc.edu
# Note: White and Red wine data will be splited into train
#       and test with 8:2 ratio, respectively. 
#       Train_white merge with train_red, 
#       test_white merge with test_red
#
# -----------------------------------------------------------


# ---- load in library ----
library(tidyverse)
library(data.table)



# ---- load in data ----
path <- getwd()
data_path <- gsub("/\\w+$", "", path)
data_path <- paste0(data_path, "/Data/")

wine_white <- fread(paste0(data_path, "winequality-white.csv")) %>% 
  mutate(color = "white",
         quality_7 = ifelse(quality < 7, "0", "1"),
         quality_7 = factor(quality_7, levels = c("0", "1")))

wine_red <- fread(paste0(data_path, "winequality-red.csv")) %>% 
  mutate(color = "red",
         quality_7 = ifelse(quality < 7, "0", "1"),
         quality_7 = factor(quality_7, levels = c("0", "1")))


# split size into train and test with 8:2 ratio
set.seed(123)
sample_white <- floor(0.8*nrow(wine_white))
white_train_ind <- sample(seq_len(nrow(wine_white)), size = sample_white)
wine_white_train <- wine_white[white_train_ind, ]
wine_white_test <- wine_white[-white_train_ind, ]

sample_red <- floor(0.8*nrow(wine_red))
red_train_ind <- sample(seq_len(nrow(wine_red)), size = sample_red)
wine_red_train <- wine_red[red_train_ind, ]
wine_red_test <- wine_red[-red_train_ind, ]


wine_train <- bind_rows(wine_red_train, wine_white_train)
wine_test <- bind_rows(wine_red_test, wine_white_test)

write_csv(wine_test, file = paste0(data_path, "wine_test.csv"))
write_csv(wine_train, file = paste0(data_path, "wine_train.csv"))


# table1::table1(~ quality_7, data = wine_train)

# ---- Correlation ----
## training dataset will be used to explore correlation among 11 continuous vars
## pearson correlation will be used
cor_df <- wine_train %>% 
  select(c(1:11)) %>% 
  as.matrix() %>% 
  cor()

library(corrplot)

jpeg(filename = "Result/correlation_train.jpeg", 
     width = 680, 
     height = 680,
     quality = 300)
corrplot(corr = cor_df, 
         method = "color", 
         addCoef.col = "darkgray",
         number.cex = 0.5)
dev.off()
