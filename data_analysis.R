#
# Data Analysis
#

library(ggplot2)
library(dplyr)
library(h2o)
library(tidytext)
# library(data.table)
library(dtplyr)
library(topicmodels)
library(randomForest)
library(tm)


# 1. Setups ---------------------------------------------------------------

path_data <- "../data/in/"

fname_train_csv <- paste0(path_data, "train.csv")
fname_test_csv <- paste0(path_data, "test.csv")

# 2. Start H2O ------------------------------------------------------------

# Run on http://127.0.0.1:54321/
localH2O = h2o.init()
h2o.init()

# 3. Read data ------------------------------------------------------------

# 3.1. Train data set
if(file.exists(fname_train_csv)){
  df_train <- h2o.importFile(fname_train_csv) %>% as.data.frame()
  # df_train <- read.csv(fname_train_csv)
}

# 3.2. Test data set
if(file.exists(fname_train_csv)){
  df_test <- h2o.importFile(fname_test_csv) %>% as.data.frame()
  # df_test <- read.csv(fname_test_csv)
}

# 4. Feature Engineering --------------------------------------------------

# 4.1. Change column names
colnames(df_train) <- c("id", "id_q1", "id_q2", "q1", "q2", "is_dupl")
colnames(df_test) <- c("id", "q1", "q2")

# 4.2. Clean up data
clean_up <- function(df){
  # clean
  df <- tolower(df)
  df <- gsub("<img src.*?>", "", df, perl = TRUE)
  df <- gsub("http\\S+", "", df, perl = TRUE)
  df <- gsub("\\[math\\]", "", df, perl = TRUE)
  df <- gsub("<.*?>", "", df, perl = TRUE)
  df <- gsub("\n", " ", df, perl = TRUE)
  df <- gsub("\\s+", " ", df, perl = TRUE)
  # using tm_map to remove stopwords
  docs <- Corpus(VectorSource(df)) %>% 
    tm_map(removeWords, stopwords('en')) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace)
  # result
  # data_content <- data.frame(text = docs[[2]]$content, stringsAsFactors = FALSE)
  data_content <- data.frame(text = unlist(sapply(docs, "content")), stringsAsFactors=F)
  
  return(data_content)
}

df_train$q1 <- clean_up(df_train$q1)
df_train$q2 <- clean_up(df_train$q2)

df_test$q1 <- clean_up(df_test$q1)
df_test$q2 <- clean_up(df_test$q2)

# df_test <- df_test %>%
#   mutate(
#     q1 = clean_up(q1),
#     q2 = clean_up(q2))

head(df_test,3)


# str(docs[[2]]$content)
# xxx <- sapply(docs, function(i) i)
# data_content <- data.frame(text = xxx, stringsAsFactors = FALSE)




