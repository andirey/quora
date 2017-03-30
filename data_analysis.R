#
# Data Analysis
#

library(ggplot2)
library(dplyr)
library(feather)

# 1. Setups ---------------------------------------------------------------

path_data <- "../data/in/"

fname_train_csv <- paste0(path_data, "train.csv")
fname_test_csv <- paste0(path_data, "test.csv")

# 2. Read data ------------------------------------------------------------

# 2.1. Train data set
if(file.exists(fname_train_csv)){
  df_train_csv <- read.csv(fname_train_csv)  
}

# 2.2. Test data set
if(file.exists(fname_train_csv)){
  df_test_csv <- read.csv(fname_test_csv)  
}

