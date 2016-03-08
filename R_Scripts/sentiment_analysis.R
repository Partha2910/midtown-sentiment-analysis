library("rmongodb")
library("dplyr")
library("tm")
library("scatterplot3d")
library("randomForest")
library("dplyr")
library("plyr")
library("class")
library("data.table")
library("e1071")
library("nnet")
library(randomForest)
library(ranger)
mongo <- mongo.create()
mongo.is.connected(mongo)


########################
# Loading and cleaning #
########################

## If mongodb is connected, query for all entries with no NaN values, stick them in a DF,
# and name the columns appropriately, additionally get rid of all values in the DF
# with a text column that is less than 200 characters.

if(mongo.is.connected(mongo) == TRUE) {
  db <- "TweetSentiment"
  collection <- paste(db, "pos_emotes", sep=".")
  fields <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "text", 1L)
  mongo.bson.buffer.append(fields, "id_str", 1L)
  mongo.bson.buffer.append(fields, "created_at", 1L)
  mongo.bson.buffer.append(fields, "_id", 0L)
  fields <- mongo.bson.from.buffer(fields)
  tweets <- mongo.find.all(mongo, collection, fields=fields)
  pos_tweets <- data.frame(matrix(unlist(tweets), nrow=length(tweets), byrow=T))
  pos_tweets$sentiment <- "P"
  
  collection <- paste(db, "neg_emotes", sep=".")
  tweets <- mongo.find.all(mongo, collection, fields=fields)
  neg_tweets <- data.frame(matrix(unlist(tweets), nrow=length(tweets), byrow=T))
  neg_tweets$sentiment <- "N"
}

pos_neg_tweets <- rbind(pos_tweets, neg_tweets)
names(pos_neg_tweets) <- c("id_str", "text", "created_at", "sentiment")
rm(pos_tweets)
rm(neg_tweets)



####################
## Pre-Processing ## ################################
####################

tokenize <- function(documents){
  doc <- tolower(documents)
  doc <- gsub("(?:#|@)[a-zA-Z0-9_]+ ?", "", doc)
  doc <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", doc)
  doc <- gsub("[[:punct:]]", "", doc)
  doc <- gsub("[\r\n]", "", doc)
  doc_words <- strsplit(doc, split=" ")
  return(doc_words)
}

corpus_freq <- function(tokens, corpus_size=2000){
  all_words <- do.call(c, tokens)
  all_words <- all_words[-which(all_words == "")]
  all_words <- all_words[-which(all_words %in% stopwords("en"))]
  corpus_freq <- data.frame(unlist(table(all_words)))
  names(corpus_freq) <- c("Word", "Freq")
  corpus_freq$Word <- as.character(corpus_freq$Word)
  corpus_freq$Freq <- as.numeric(corpus_freq$Freq)
  corpus_freq <- corpus_freq[order(-corpus_freq$Freq), ]
  corpus_freq <- corpus_freq[1:corpus_size, ]
  corpus_freq <- corpus_freq[order(corpus_freq$Word), ]
}

add_doc_freq <- function(corpus_freq, tokens){
  corpusfreq <- data.frame(Word = corpus_freq$Word)
  corpus_freq$n_docs <- 0
  for(token_list in tokens){
    t <- data.frame(unlist(table(token_list)))
    names(t) <- c("Word", "n_docs")
    t$n_docs <- 1
    t_freq <- merge(x=corpusfreq, y=t, by="Word", all.x=TRUE)
    t_freq$n_docs[is.na(t_freq$n_docs)] <- 0
    corpus_freq$n_docs <- corpus_freq$n_docs + t_freq$n_docs
  }
  return(corpus_freq)
}

tfidf <- function(tokenized, corpus){
  doc_f <- data.frame(unlist(table(tokenized)))
  names(doc_f) <- c("Word", "Freq")
  in_doc <- intersect(doc_f$Word, corpus$Word)
  doc_f <- doc_f[doc_f$Word %in% in_doc, ]
  
  not_in_doc <- data.frame(Word=setdiff(corpus$Word, tokenized))
  not_in_doc$Freq <-0
  
  tf <- rbind(doc_f, not_in_doc)
  tf$Word <- as.character(tf$Word)
  tf$Freq <- as.numeric(tf$Freq)
  tf <- tf[order(tf$Word), ]
  
  log_freq <- log1p(tf$Freq)
  log_doc_freq <- log1p(nrow(corpus)/corpus$n_docs)
  tf$tfidf <- log_freq * log_doc_freq
  tf$tfidf[is.na(tf$tfidf)] <- 0
  return(tf)
}


########################
## Feature Extraction ## ################################
########################



get_feature_vectors <- function(tokens_list, corpus_size){
  corpus <- corpus_freq(tokens_list, corpus_size=corpus_size)
  corpus <- add_doc_freq(corpus, tokens_list)
  feature_matrix <- matrix(0, length(tokens_list), nrow(corpus))
  
  for(i in 1:length(tokens_list)){
    feature_vector <- tfidf(tokens_list[[i]], corpus)$tfidf
    feature_matrix[i, 1:nrow(corpus)] <- feature_vector
  }
  
  
  colnames(feature_matrix) <- corpus$Word
  return(data.frame(feature_matrix))
} 

# add_targets <- function(feature_matrix, df){
#   feature_matrix$Impressions <- df$Impressions
#   feature_matrix$Engagements <- df$Engagements
#   return(feature_matrix)
# }


add_targets <- function(feature_matrix, df){
  feature_matrix$sentiment <- df$sentiment
  return(feature_matrix)
}


###################
## Model Fitting ##
###################
fit_model <- function(features, k=3){
  h_pop <- features[features$Engagements >= quantile(features$Engagements, .66), ]
  m_pop <- features[features$Engagements > quantile(features$Engagements, .33) & features$Engagements < quantile(features$Engagements, .66), ]
  l_pop <- features[features$Engagements <= quantile(features$Engagements, .33), ]
  
  h_pop$Popularity <- "High"
  m_pop$Popularity <- "Med"
  l_pop$Popularity <- "Low"
  
  features <- rbind(h_pop, m_pop, l_pop)
  targets <- c("Engagements", "Impressions", "Popularity")
  
  train <- sample_frac(features, .8)
  test <- setdiff(features, train)
  test <- sample_frac(test, 1)
  
  test_features <- test[, !(names(test) %in% targets)]
  test_targets <- test$Popularity
  
  train_features <- train[, !(names(train) %in% targets)]
  train_targets <- train$Popularity
  
  test_knn <- knn(train=train_features, test=test_features, cl=factor(train_targets), k=k)
  print("Random...")
  print(sum(sample(c("High", "Med", "Low"), length(test_targets), replace=TRUE) == test_targets) / length(test_targets))
  print("My results...")
  return(sum(test_knn == test_targets) / length(test_targets))
}
trials <- function(features, k=5, n=10){
  targets <- c("Engagements", "Impressions", "Popularity", "sentiment")
  
  acc <- c()
  for(i in 1:n){
    train <- sample_frac(features, .8)
    test <- setdiff(features, train)
    test <- sample_frac(test, 1)
    
    test_features <- test[, !(names(test) %in% targets)]
    test_targets <- test$sentiment
    
    train_features <- train[, !(names(train) %in% targets)]
    train_targets <- train$sentiment
    
    test_knn <- knn(train=train_features, test=test_features, cl=factor(train_targets), k=k)
    print("Random...")
    print(sum(sample(c("P", "N"), length(test_targets), replace=TRUE) == test_targets) / length(test_targets))
    print("My results...")
    acc <- c(acc, sum(test_knn == test_targets) / length(test_targets))
  }
  return(mean(acc))
}

tokens <- tokenize(pos_neg_tweets$text)
system.time(my_features <- get_feature_vectors(tokens, corpus_size=1500))
my_features <- add_targets(my_features, pos_neg_tweets)
my_features$sentiment <- as.factor(my_features$sentiment)


train <- sample_frac(my_features, .8)
test <- setdiff(my_features, train)
test <- sample_frac(test, 1)


train <- as.data.frame(train)
test <- as.data.frame(test)


m_nnet <- nnet(sentiment~., data=train, size=15, MaxNWts=100000)
pred_nnet <- predict(m_nnet, test, type="class")
conf_nnet <- table(pred_nnet, test$sentiment)
sens_nnet <- sensitivity(conf_nnet)
sens_nnet


#5-nearest neighbors
m_knn <- knn(train[,-which(names(train)=="sentiment")], test[,-which(names(train)=="sentiment")], cl=train$sentiment, k=5)
conf_knn <- table(m_knn, test$sentiment)
sens_knn <- sensitivity(conf_knn)
sens_knn

# Naive Bayes Classifier
m_nbayes <- naiveBayes(sentiment~., data=train, laplace=1000, threshold=.5)
pred_nbayes <- predict(m_nbayes, test, threshold=.5, laplace=1000)
conf_nbayes <- table(pred_nbayes, test$sentiment)
sens_nbayes <- sensitivity(conf_nbayes)
sens_nbayes


# Random Forest
m_randomforest <- ranger(dependent.variable.name="sentiment", data=train, write.forest=TRUE)
pred_rf <- predict(m_randomforest, data=test)
pred_rf <- pred_rf$predictions
conf_rf <- table(test$sentiment, pred_rf)
sens_rf <- sensitivity(conf)
#Logistic Regression
m_logit <- glm(sentiment~., data=train, family=binomial(link='logit'))
pred_log <- predict(m_logit, test, type="response")
pred_fitted <- ifelse(pred_log > .5,"P","N")
conf_log <- table(pred_fitted, test$sentiment)
sens_log <- sensitivity(conf_log)


#SVM
m_svm <- svm(sentiment~., data=train, type="C")
pred_svm <- predict(m_svm, test)
conf_svm <- table(pred_svm, test$sentiment)
sens_svm <- sensitivity(conf_svm)




ensemble <- function(predictions, test){
  votes <- matrix(0, length(predictions), nrow(test))
  for(i in 1:length(predictions)){
    votes[i,] <- ifelse(predictions[[i]] == "P",1,0)
  }
  vote_decision <- colSums(votes)/nrow(votes)
  vote_decision <- ifelse(vote_decision >= .5,"P", "N")
  
  return(vote_decision)
}

ens <- ensemble(list(pred_log, pred_rf, pred_svm, m_knn, pred_nbayes, pred_nnet), test)
sens_ens <- sensitivity(table(ens, test$sentiment))


sensitivity <- function(confusion_matrix){
  acc <- (confusion_matrix[1]+confusion_matrix[4])/sum(confusion_matrix)
  tn <- (confusion_matrix[1]) / (confusion_matrix[3]+confusion_matrix[1])
  ppv <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3])
  tp <- (confusion_matrix[4]) / (confusion_matrix[4]+confusion_matrix[2])
  return(list(accuracy=acc, specificity=tn, precision=ppv, sensitivity=tp))
}

