library("rmongodb")
library("dplyr")
library("randomForest")
library("dplyr")
library("plyr")
library("class")
library("e1071")
library("nnet")
library("neuralnet")
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
  # mongo.bson.buffer.append(fields, "geo", 0L)
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
  stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse="|"), ")\\b")
  doc <- gsub(stop_pattern, "", doc)
  doc <- gsub(" {2,}", " ", doc)
  doc_words <- strsplit(doc, " ")
  return(doc_words)
}

corpus_freq <- function(tokens, corpus_size=NULL, word_list = NULL){
  all_words <- do.call(c, tokens)
  all_words <- all_words[-which(all_words == "")]
  
  if(is.null(word_list) & !is.null(corpus_size)){
    corpusfreq <- data.frame(table(all_words))
    names(corpusfreq) <- c("Word", "Freq")
    corpusfreq$Word <- as.character(corpusfreq$Word)
    corpusfreq$Freq <- as.numeric(corpusfreq$Freq)
    corpusfreq <- corpusfreq[order(-corpusfreq$Freq), ]
    corpusfreq <- corpusfreq[1:corpus_size, ]
    corpusfreq <- corpusfreq[order(corpusfreq$Word), ]
  }
  
  corpusfreq <- data.frame(word_list)
  names(corpusfreq) <- c("Word")
  corpusfreq$n_docs <- 0
  for(token_list in tokens){
    t <- data.frame(table(token_list))
    names(t) <- c("Word", "n_docs")
    t$n_docs <- 1
    t_freq <- merge(x=corpusfreq, y=t, by="Word", all.x=TRUE)
    t_freq$n_docs.y[is.na(t_freq$n_docs.y)] <- 0
    corpusfreq$n_docs <- corpusfreq$n_docs + t_freq$n_docs.y
  }
  return(corpusfreq)
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


get_feature_vectors <- function(tokens_list, corpus_size=1500, corpus=NULL){
  if(is.null(corpus)){
    corpus <- corpus_freq(tokens_list, corpus_size=corpus_size)
  }
  
  feature_matrix <- matrix(0, length(tokens_list), nrow(corpus))
  
  for(i in 1:length(tokens_list)){
    feature_vector <- tfidf(tokens_list[[i]], corpus)$tfidf
    feature_matrix[i, 1:nrow(corpus)] <- feature_vector
  }
  
  colnames(feature_matrix) <- corpus$Word
  return(data.frame(feature_matrix))
}


add_targets <- function(feature_matrix, df){
  feature_matrix$sentiment <- df$sentiment
  return(feature_matrix)
}


ensemble <- function(predictions){
  votes <- matrix(0, length(predictions), length(predictions[[1]]))
  for(i in 1:length(predictions)){
    votes[i,] <- ifelse(predictions[[i]] == "P",1,0)
  }
  vote_decision <- colSums(votes)/nrow(votes)
  vote_decision <- ifelse(vote_decision >= .5,"P", "N")
  
  return(vote_decision)
}


sensitivity <- function(confusion_matrix){
  acc <- (confusion_matrix[1]+confusion_matrix[4])/sum(confusion_matrix)
  tn <- (confusion_matrix[1]) / (confusion_matrix[3]+confusion_matrix[1])
  ppv <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3])
  tp <- (confusion_matrix[4]) / (confusion_matrix[4]+confusion_matrix[2])
  return(list(accuracy=acc, specificity=tn, precision=ppv, sensitivity=tp))
}

############################################


new_corpus <- corpus_freq(tokenize(pos_neg_tweets$text), word_list=in_both)






###################
## Model Fitting ##
###################

sentiment_corpus <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/corpus_3000.rds")
tokens <- tokenize(pos_neg_tweets$text)
my_features <- get_feature_vectors(tokens, corpus=sentiment_corpus)
my_features <- add_targets(my_features, pos_neg_tweets)
my_features$sentiment <- as.factor(my_features$sentiment)
my_features$textblob <- as.factor(my_features$textblob)






train <- sample_frac(my_features, .8)
test <- setdiff(my_features, train)
test <- sample_frac(test, 1)




#Training Models
form <- as.formula(paste("sentiment~", paste(setdiff(names(test), c("sentiment", "textblob")), collapse="+")))
m_nnet <- nnet(form, data=train, size=10, MaxNWts=100000)
m_nbayes <- naiveBayes(form, data=train, laplace=1000, threshold=.5)
m_randomforest <- ranger(dependent.variable.name="sentiment", data=train, write.forest=TRUE)
m_logit <- glm(form, data=train, family=binomial(link='logit'))
m_svm <- svm(form, data=train, type="C")


#Predicting
#nnet

pred_nnet <- predict(m_nnet, test, type="class")
conf_nnet <- table(pred_nnet, test$sentiment)
sens_nnet <- sensitivity(conf_nnet)
sens_nnet

# Naive Bayes Classifier

pred_nbayes <- predict(m_nbayes, test, threshold=.5, laplace=1000)
conf_nbayes <- table(pred_nbayes, test$sentiment)
sens_nbayes <- sensitivity(conf_nbayes)
sens_nbayes

# Random Forest

pred_rf <- predict(m_randomforest, data=test)
pred_rf <- pred_rf$predictions
conf_rf <- table(test$sentiment, pred_rf)
sens_rf <- sensitivity(conf_rf)

#Logistic Regression

pred_log <- predict(m_logit, test, type="response")
pred_log <- ifelse(pred_log > .5,"P","N")
conf_log <- table(pred_fitted, test$sentiment)
sens_log <- sensitivity(conf_log)


#SVM

pred_svm <- predict(m_svm, test)
conf_svm <- table(pred_svm, test$sentiment)
sens_svm <- sensitivity(conf_svm)


ens <- ensemble(list(pred_nnet, pred_nbayes, pred_rf, pred_log, pred_svm))

# saveRDS(m_logit, "~/Documents/LevelEdu/sentiment_analysis/models/logit_sentiment_3000.rds")
# saveRDS(m_nbayes, "~/Documents/LevelEdu/sentiment_analysis/models/naivebayes_sentiment_3000.rds")
# saveRDS(m_nnet,"~/Documents/LevelEdu/sentiment_analysis/models/nnet_sentiment_3000.rds")
# saveRDS(m_randomforest,"~/Documents/LevelEdu/sentiment_analysis/models/randmforest_sentiment_3000.rds")
# saveRDS(m_svm,"~/Documents/LevelEdu/sentiment_analysis/models/svm_sentiment_3000.rds")
# saveRDS(corpus, "~/Documents/LevelEdu/sentiment_analysis/models/corpus_3000.rds")





predict_and_save <- function(search_term, filepath, corpus){
  print(paste("Grabbing", search_term, "from the database"))
  query = list("$text"=list("$search"=paste("\"", search_term, "\" -job -deal -deals", sep="")))
  mongo_search <- mongo.find.all(mongo, "TweetSentiment.timelines", fields=fields, query = query)
  company_frame <- data.frame(matrix(unlist(mongo_search), nrow=length(mongo_search), byrow=T))
  names(company_frame) <- c("id_str", "text", "created_at")
  print("Tokenizing...")
  company_tokens <- tokenize(company_frame$text)
  company_features <- get_feature_vectors(company_tokens, corpus_size=3000, corpus=corpus)
  print("Predicting with logit")
  p_logit <- predict(m_logit, company_features, type="response")
  p_logit <- ifelse(p_logit > .5,"P","N")
  print("Predicting with naive bayes")
  p_nbayes <- predict(m_nbayes, company_features)
  print("predicting with nnet")
  p_nnet <- predict(m_nnet, company_features)
  print("Predicting with random forest")
  p_randomforest <- predict(m_randomforest, company_features)
  print("Predicting with svm")
  p_svm <- predict(m_svm, company_features)
  
  company_prediction <- ensemble(list(p_logit, p_nbayes, p_nnet, p_randomforest$predictions, p_svm))
  company_frame$sentiment <- company_prediction
  print(paste("Writing to file at ", filepath))
  write.csv(company_frame, filepath)
}



#Searches the database for tweets about query, then saves sentiment-labeled file to CSV
# predict_and_save("Verizon", "~/Documents/LevelEdu/sentiment_analysis/R_Scripts/verizon.csv", corpus=sentiment_corpus)
# predict_and_save("AT\&T", "~/Documents/LevelEdu/sentiment_analysis/R_Scripts/att.csv", corpus=sentiment_corpus)
# predict_and_save("Home Depot", "~/Documents/LevelEdu/sentiment_analysis/R_Scripts/home_depot.csv", corpus=sentiment_corpus)
# predict_and_save("Lowes", "~/Documents/LevelEdu/sentiment_analysis/R_Scripts/lowes.csv", corpus=sentiment_corpus)
# predict_and_save("Bank of America", "~/Documents/LevelEdu/sentiment_analysis/R_Scripts/bank_of_america.csv", corpus=sentiment_corpus)
# predict_and_save("Wells Fargo", "~/Documents/LevelEdu/sentiment_analysis/R_Scripts/wells_fargo.csv", corpus=sentiment_corpus)






## Query Tweets about "Bernie Sanders" from the database

mongo <- mongo.create()
fields <- mongo.bson.buffer.create()
mongo.bson.buffer.append(fields, "text", 1L)
mongo.bson.buffer.append(fields, "id_str", 1L)
mongo.bson.buffer.append(fields, "created_at", 1L)
mongo.bson.buffer.append(fields, "_id", 0L)
fields <- mongo.bson.from.buffer(fields)
query = list("$text"=list("$search"="\"Bernie Sanders\""))
mongo_search <- mongo.find.all(mongo, "TweetSentiment.timelines", fields=fields, query=query)
bernie <- data.frame(matrix(unlist(mongo_search), nrow=length(mongo_search), byrow=T))
names(bernie) <- c("id_str", "text", "created_at")


## Load the Corpus from File
sentiment_corpus <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/corpus_3000.rds")

#Load the Pre-trained Models
m_nnet <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/nnet_sentiment_3000.rds")
m_nbayes <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/naivebayes_sentiment_3000.rds")
m_randomforest <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/randmforest_sentiment_3000.rds")
m_svm <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/svm_sentiment_3000.rds")
m_logit <- readRDS("~/Documents/LevelEdu/sentiment_analysis/models/logit_sentiment_3000.rds")

#Tokenize and get feature vectors
tokens <- tokenize(bernie$text)
bernie_features <- get_feature_vectors(tokens, corpus=sentiment_corpus)


#Predict using all the models
bernie_prediction_nnet <- predict(m_nnet, bernie_features, type="class")
bernie_prediction_nbayes <- predict(m_nbayes, bernie_features, threshold=.5, laplace=1000)
bernie_prediction_rf <- predict(m_randomforest, data=bernie_features)
bernie_prediction_rf <- bernie_prediction_rf$predictions
bernie_prediction_log <- predict(m_logit, bernie_features, type="response")
bernie_prediction_log <- ifelse(bernie_prediction_log > .5,"P","N")
bernie_prediction_svm <- predict(m_svm, bernie_features)

#Use the ensemble function to vote
bernie_ensemble <- ensemble(list(bernie_prediction_log,
                                bernie_prediction_rf, 
                                bernie_prediction_svm, 
                                bernie_prediction_nbayes, 
                                bernie_prediction_nnet))

bernie$sentiment <- bernie_ensemble
table(bernie$sentiment)


