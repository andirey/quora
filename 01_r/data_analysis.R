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
path_data <- "../data/in/"

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

ans <- tf_idf_q1[ , c("len_common_words", 
                      "ratio_commonality", 
                      "diff_len") := func(.SD), 
                  key = id1, 
                  .SDcols = c(colnames(tf_idf_q1))]



func <- function(x){
  
  # Boolean vector to subset the q2 for same id 
  id_check <- x$id1[1] == tf_idf_q2$id2
  
  words1 <- x$word1
  
  words2 <- tf_idf_q2$word2[id_check]
  
  # List of common words in both
  common <- intersect(words1, words2)
  
  # Words not present in question1
  uncommon_q1 <- setdiff(words1, words2)
  
  # Words not present in question2
  uncommon_q2 <- setdiff(words2, words1)
  
  len_common_words <- length(common)
  
  len_q1 <- nchar(x$q1[1])
  len_q2 <- nchar(tf_idf_q2$q2[id_check][1])
  
  # Difference in length of characters
  diff_len <- abs(len_q1 - len_q2)
  
  tfidf_wt1 <- x$tf_idf
  tfidf_wt2 <- tf_idf_q2$tf_idf[id_check]
  
  # Calculate how similar both questions are based on tfidf weights
  # 1. Positive effect for the common words
  # 2. Negative exposure for the uncommon words
  
  w1_shared_wts <- tfidf_wt1[match(common, words1)]
  w1_unshared_wts <- tfidf_wt1[match(uncommon.q1, words1)]
  
  w2_shared_wts <- tfidf_wt2[match(common, words2)]
  w2_unshared_wts <- tfidf_wt2[match(uncommon.q2, words2)]
  
  ratio_commonality = (sum(c(w1_shared_wts,w2_shared_wts))-sum(c(w1_unshared_wts,w2_unshared_wts)))/(sum(tfidf_wt1, tfidf_wt2))
  
  # Return result as a list
  return(list(len_common_words, ratio_commonality, diff_len))
}

