#
# Modeling
#

# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidytext)
library(randomForest)
library(tm)
library(data.table)
library(dtplyr)
library(feather)

# library(topicmodels)

# 1. Setups ---------------------------------------------------------------

# 1.1. Path to data files
path_data <- "data/in/"

# 1.2. CSV file names
fname_train_csv <- paste0(path_data, "train.csv")
fname_test_csv <- paste0(path_data, "test.csv")

# 1.3. Feather file names
fname_train_feather <- paste0(path_data, "train.feather")
fname_test_feather <- paste0(path_data, "test.feather")

# 1.4. "Clean" feather file names
fname_train_feather_clean <- paste0(path_data, "train.feather.clean")
fname_test_feather_clean <- paste0(path_data, "test.feather.clean")

# 2. Read data ------------------------------------------------------------

# 2.1. Train data set
if(!file.exists(fname_train_feather)){
  df_train <- read.csv(fname_train_csv)
  write_feather(df_train, fname_train_feather)
} else {
  df_train <- read_feather(fname_train_feather)
}

# 2.2. Test data set
if(!file.exists(fname_test_feather)){
  df_test <- read.csv(fname_test_csv)
  write_feather(df_test, fname_test_feather)
} else {
  df_test <- read_feather(fname_test_feather)
}

# 2.4. Change column names

colnames(df_train) <- c("id", "id_q1", "id_q2", "q1", "q2", "is_duplicate")
colnames(df_test) <- c("id", "q1", "q2")

# 3. Sampling -------------------------------------------------------------

# TODO: comment for production

df_train_sample <- df_train[sample(1:nrow(df_train), 1000, replace = FALSE), ]
df_test_sample <- df_test[sample(1:nrow(df_test), 100, replace = FALSE), ]

# 4. Feature Engineering --------------------------------------------------

# 4.2. Clean up data

# 4.2.1. Function
clean_up <- function(df){
  # 1. clean
  df <- tolower(df)
  df <- gsub("<img src.*?>", "", df, perl = TRUE)
  df <- gsub("http\\S+", "", df, perl = TRUE)
  df <- gsub("\\[math\\]", "", df, perl = TRUE)
  df <- gsub("<.*?>", "", df, perl = TRUE)
  df <- gsub("\n", " ", df, perl = TRUE)
  df <- gsub("\\s+", " ", df, perl = TRUE)
  # 2. using tm_map to remove stopwords
  docs <- Corpus(VectorSource(df)) %>% 
    tm_map(removeWords, stopwords('en')) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace)
  # 3. create result
  result <- unlist(data.frame(text = unlist(sapply(docs, "content")), stringsAsFactors=F))
  # 4. return result
  return(result)
}

# 4.2.2. Clean train data set
if(!file.exists(fname_train_feather_clean)){
  # 1. calculate
  df_train_sample <- df_train_sample %>% 
    mutate(
      q1 = clean_up(q1),
      q2 = clean_up(q2))
  # 2. write
  write_feather(df_train_sample, fname_train_feather_clean)
} else {
  # 3. read
  df_train_sample <- read_feather(fname_train_feather_clean)
}

# 4.2.3. Clean test data set
if(!file.exists(fname_test_feather_clean)){
  # 1. calculate
  # 1. calculate
  df_test_sample <- df_test_sample %>% 
    mutate(
      q1 = clean_up(q1),
      q2 = clean_up(q2))
  # 2. write
  write_feather(df_test_sample, fname_test_feather_clean)
} else {
  # 3. read
  df_test_sample <- read_feather(fname_test_feather_clean)
}

# head(df_test_sample,3)
  
# 5. Generate tokens ----------------------------------------------------------

# 5.1. Question #1

# 5.1.1. Create tokens
tokens_q1 <- df_train_sample %>% 
  unnest_tokens(word, q1, drop = FALSE, token = "regex", pattern = " ") %>% 
  count(id, word) %>% 
  ungroup()

# 5.1.2. Join two data sets
tokens_q1 <- tokens_q1 %>% 
  left_join(df_train_sample, by = c("id" = "id"))

# 5.1.3. Change column names
colnames(tokens_q1)[1:3] <- c("id1", "word1", "n1")

# 5.1.4. Add "word" from main sample
tokens_q1 <- tokens_q1 %>% 
  subset(select = c("id1", "q1", "word1", "n1"))

# 5.1.5. Calculate tf-idf weights
tf_idf_q1 <- tokens_q1 %>% bind_tf_idf(word1, q1, n1) %>%
  select(id1, q1, word1, tf, idf, tf_idf)

# 5.2. Question #2

# 5.2.1. Create tokens
tokens_q2 <- df_train_sample %>% 
  unnest_tokens(word, q2, drop = FALSE, token = "regex", pattern = " ") %>% 
  count(id, word) %>% 
  ungroup()

# 5.2.2. Join two data sets
tokens_q2 <- tokens_q2 %>% 
  left_join(df_train_sample, by = c("id" = "id"))

# 5.2.3. Change column names
colnames(tokens_q2)[1:3] <- c("id2", "word2", "n2")

# 5.2.4. Add "word" from main sample
tokens_q2 <- tokens_q2 %>% 
  subset(select = c("id2", "q2", "word2", "n2"))

# 5.2.5. Calculate tf-idf weights
tf_idf_q2 <- tokens_q2 %>% bind_tf_idf(word2, q2, n2) %>%
  select(id2, q2, word2, tf, idf, tf_idf)

# 6. Features for prediction ---------------------------------------------------

# TODO: from here
# data.table learn more

ans <- tf_idf_q1[ , c("len_common_words", "ratio_commonality", "diff_len") := func(.SD),
                  key = id1,  .SDcols = c(colnames(tf_idf_q1))]

