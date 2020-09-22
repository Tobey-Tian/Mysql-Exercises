library(tidyverse)
library(tm)
library(data.table)
library(tidytext)
library(caret)

data <- fread("/Users/apple/Desktop/201909-201912/BUS-256A/hw6/data_reviews_classified.csv")
colnames(data) <- c('text', 'sentiment', 'id')
data[ , sentiment := ifelse(sentiment == 'positive', 1, 0)]

expansion <- function(text) {
  text <- gsub("won't", "will not", text)
  text <- gsub("can't", "can not", text)
  text <- gsub("n't", " not", text)
  text <- gsub("'ll", " will", text)
  text <- gsub("'re", " are", text)
  text <- gsub("'ve", " have", text)
  text <- gsub("'m", " am", text)
  text <- gsub("'d", " would", text)
  # 's could be 'is' or could be possessive: it has no expansion
  text <- gsub("'s", "", text)
  return(text)
}

data[ , text_clean := {temp = gsub('<br />', ' ', text); temp1 = tolower(temp); temp2 = expansion(temp1); temp3 = gsub('[.?!,()]', ',', temp2); temp4 = gsub('[^a-zA-Z0-9 ,]', ' ', temp3)}]

data_split <- data[ , .(sentence = unlist(strsplit(text_clean, split = ','))), by = id]
data_split <- data_split[sentence != '']
data_split[ , sentence := paste(' ', sentence, ' '), by = sentence]
data_split[ , sentence := gsub(pattern = " no ", replacement = " not ", sentence)]
data_split[grepl(' not ', sentence) , sentence := {temp1 = strsplit(sentence, split = 'not'); temp2 = strsplit(temp1[[1]][2], ' ')[[1]]; temp3 = temp2[temp2 != '']; temp4 = paste0('not-', temp3, collapse = ' '); temp5 = paste0(temp1[[1]][1],temp4)}, by = sentence]
data_split[ , sentence := paste(sentence, collapse = ''), by = id]
data_split <- unique(data_split)

text_data<- merge(data[ , .(sentiment, id)], data_split, by = 'id')[ , .(sentiment, sentence, id)]
id_train <- createDataPartition(text_data$sentiment, p = 0.7)$Resample1
training <- text_data[id_train]
testing  <- text_data[-id_train, ]

dict <- as.data.table(get_sentiments('bing'))
dict[ , word := {temp1 = tolower(word); temp2 = gsub(pattern = "[^a-z'-]", '', temp1)}]
dict_not <- dict[ , .(word = paste0('not-', word), sentiment)]
dict_not[ , sentiment:=ifelse(sentiment == 'negative', 'positive', 'negative')]
dict <- rbind(dict, dict_not)

positive <- dict[sentiment == 'positive', word]
negative <- dict[sentiment == 'negative', word]

train_word_count <- training[ , .(positive = list(str_count(sentence, positive)), negative = list(str_count(sentence, negative))), by = id]

tf <- matrix(data = unlist(train_word_count$positive), ncol = nrow(train_word_count))
tf_positive_train <- apply(tf, 1, function(x){x * log(length(x) / sum(x != 0))})
positive_log <- apply(tf, 1, function(x){log(length(x) / sum(x != 0))})
positive_log[is.infinite(positive_log)] <- 0
tf_positive_train <- as.data.table(tf_positive_train)
tf_positive_train[is.na(tf_positive_train)] <- 0
colnames(tf_positive_train) <- positive


tf <- matrix(data = unlist(train_word_count$negative), ncol = nrow(train_word_count))
tf_negative_train <- apply(tf, 1, function(x){-x * log(length(x) / sum(x != 0))})
negative_log <- apply(tf, 1, function(x){log(length(x) / sum(x != 0))})
negative_log[is.infinite(negative_log)] <- 0
tf_negative_train <- as.data.table(tf_negative_train)
tf_negative_train[is.na(tf_negative_train)] <- 0
colnames(tf_negative_train) <- negative

tf_train <- cbind(tf_positive_train, tf_negative_train)

forest <- randomForest(tf_train, as.factor(training$sentiment), ntree = 100)

Sent_analysis <- function(text, extra_arguments_list = arg) {
  print('Three packages should be loaded: data.table, stringr and randomForest')
  if(require("data.table")){
    print("data.table is loaded correctly")
  } else {
    print("trying to install datatable")
    install.packages("data.table")
    if(require(data.table)){
      print("datatable installed and loaded")
    } else {
      stop("could not install datatable")
    }
  }
  
  if(require("stringr")){
    print("stringr is loaded correctly")
  } else {
    print("trying to install stringr")
    install.packages("stringr")
    if(require(stringr)){
      print("stringr installed and loaded")
    } else {
      stop("could not install stringr")
    }
  }
  
  if(require("randomForest")){
    print("randomForest is loaded correctly")
  } else {
    print("trying to install randomForest")
    install.packages("randomForest")
    if(require(randomForest)){
      print("randomForest installed and loaded")
    } else {
      stop("could not install randomForest")
    }
  }
  
  expansion <- function(text) {
    # "won't" is a special case as it does not expand to "wo not"
    text <- gsub("won't", "will not", text)
    text <- gsub("can't", "can not", text)
    text <- gsub("n't", " not", text)
    text <- gsub("'ll", " will", text)
    text <- gsub("'re", " are", text)
    text <- gsub("'ve", " have", text)
    text <- gsub("'m", " am", text)
    text <- gsub("'d", " would", text)
    # 's could be 'is' or could be possessive: it has no expansion
    text <- gsub("'s", "", text)
    return(text)
  }
  
  positive <- arg$positive
  negative <- arg$negative
  
  data <- data.table(text = text, id = 1:length(text))
  data[ , text_clean := {temp = gsub('<br />', ' ', text); temp1 = tolower(temp); temp2 = expansion(temp1); temp3 = gsub('[.?!,()]', ',', temp2); temp4 = gsub('[^a-zA-Z0-9 ,]', ' ', temp3)}]
  data_split <- data[ , .(sentence = unlist(strsplit(text_clean, split = ','))), by = id]
  data_split <- data_split[sentence != '']
  data_split[ , sentence := paste(' ', sentence, ' '), by = sentence]
  data_split[ , sentence := gsub(pattern = " no ", replacement = " not ", sentence)]
  data_split[grepl(' not ', sentence) , sentence := {temp1 = strsplit(sentence, split = 'not'); temp2 = strsplit(temp1[[1]][2], ' ')[[1]]; temp3 = temp2[temp2 != '']; temp4 = paste0('not-', temp3, collapse = ' '); temp5 = paste0(temp1[[1]][1],temp4)}, by = sentence]
  data_split[ , sentence := paste(sentence, collapse = ''), by = id]
  testing <- unique(data_split)
  testing[ , words := {temp1 = strsplit(sentence, ' ')[[1]]; temp2 = temp1[!(temp1 %in% c('\t', ' ', ''))]; length(temp2)}, by = id]
  testing <- testing[order(words, decreasing = F)]
  
  testing_short <- testing[words <= 15]
  testing_long  <- testing[words > 15]
  
  if (nrow(testing_short) > 0) {
    test_word_count_short <- testing_short[ , .(positive = sum(str_count(sentence, positive)), negative = sum(str_count(sentence, negative))), by = id]
    result_short <- test_word_count_short[ , .(id, sentiment = ifelse(positive > negative, 'positive', 'negative'))]
  } else {
    result_short <- NULL
  }
  
  if (nrow(testing_long) > 0) {
    test_word_count_long <- testing_long[ , .(positive = list(str_count(sentence, positive)), negative = list(str_count(sentence, negative))), by = id]
    positive_log <- arg$positive_log
    tf <- as.data.table(matrix(data = unlist(test_word_count_long$positive), ncol = nrow(test_word_count_long)))
    tf_positive_test <- as.data.table(t(apply(tf, 2, function(x){x * positive_log})))
    #tf_positive_test <- as.data.table(tf_positive_test)
    #tf_positive_test[is.na(tf_positive_test)] <- 0
    colnames(tf_positive_test) <- positive
    
    negative_log <- arg$negative_log
    tf <- as.data.table(matrix(data = unlist(test_word_count_long$negative), ncol = nrow(test_word_count_long)))
    tf_negative_test <- as.data.table(t(apply(tf, 2, function(x){-x * negative_log})))
    colnames(tf_negative_test) <- negative
    
    tf_test <- cbind(tf_positive_test, tf_negative_test)
    
    prediction = predict(arg$model, newdata = tf_test, type = 'class')
    
    result_long <- ifelse(prediction == 1, 'positive', 'negative')
    result_long <- data.table(id = testing_long$id, sentiment = result_long)
    
  } else {
    result_long <- NULL
  }
  
  
  result <- rbind(result_long, result_short)
  result <- result[order(id, decreasing = F)]
  return(result$sentiment)
}

save(Sent_analysis, arg ,file = 'sent_analyzer.RData')


