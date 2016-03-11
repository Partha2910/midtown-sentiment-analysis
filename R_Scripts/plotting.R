sensitivity <- function(confusion_matrix){
  acc <- (confusion_matrix[1]+confusion_matrix[4])/sum(confusion_matrix)
  tn <- (confusion_matrix[1]) / (confusion_matrix[3]+confusion_matrix[1])
  ppv <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3])
  tp <- (confusion_matrix[4]) / (confusion_matrix[4]+confusion_matrix[2])
  return(list(accuracy=acc, specificity=tn, precision=ppv, sensitivity=tp))
}


wells_fargo <- read.csv("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/wells_fargo.csv", stringsAsFactors = F)
wells_fargo$id_str <- as.character(wells_fargo$id_str)
wells_fargo$sentiment <- as.factor(wells_fargo$sentiment)
names(wells_fargo) <- c("row","id_str","text","created_at","sentiment","Textblob_polarity")


wells_fargo$date <- as.Date(as.POSIXct(wells_fargo$created_at, origin="1970-01-01"))
wells_fargo <- wells_fargo[order(wells_fargo$created_at),]
wells_fargo <- wells_fargo[wells_fargo$date > as.Date(as.POSIXct("2016-02-10")), ]

wells_fargo$Textblob_polarity <- ifelse(wells_fargo$Textblob_polarity >= 0, "P", "N")

sensitivity(table(wells_fargo$sentiment, wells_fargo$Textblob_polarity))
#naive bayes vs. naive bayes in r and python



per_day <- data.frame(t(table(wells_fargo$sentiment, wells_fargo$date)))
names(per_day) <- c("Date", "Sentiment", "Count")
per_day$scaled_sentiment <- 0.0
per_day$scaled_sentiment[per_day$Sentiment=="N"] <- - (per_day[per_day$Sentiment=="N", ]$Count/max(per_day[per_day$Sentiment=="N", ]$Count))
per_day$scaled_sentiment[per_day$Sentiment=="P"] <- per_day[per_day$Sentiment=="P", ]$Count/max(per_day[per_day$Sentiment=="P", ]$Count)

per_day_neg <- per_day[per_day$Sentiment=="N", ]
per_day_pos <- per_day[per_day$Sentiment=="P", ]

p_over_n <- per_day_pos$Count/per_day_neg$Count
library("ggplot2")

ggplot(per_day, aes(x=per_day$Date, y=per_day$scaled_sentiment, fill=per_day$Sentiment)) +
  geom_bar(stat="identity", position="identity")

ggplot(per_day_pos, aes(x=per_day_pos$Date, y=per_day_pos$scaled_sentiment+per_day_neg$scaled_sentiment,
                        fill=per_day_pos$scaled_sentiment+per_day_neg$scaled_sentiment>0)) +
  geom_bar(stat="identity", position="identity")


read_and_plot <- function(filepath, datestring="2016-02-28", title){
  company_frame <- read.csv(filepath, stringsAsFactors = F)
  company_frame$id_str <- as.character(company_frame$id_str)
  company_frame$date <- as.Date(as.POSIXct(company_frame$created_at, origin="1970-01-01"))
  company_frame$sentiment <- as.factor(company_frame$sentiment)
  company_frame <- company_frame[order(company_frame$created_at),]
  company_frame <- company_frame[company_frame$date > as.Date(as.POSIXct(datestring)), ]
  
  per_day <- data.frame(t(table(company_frame$sentiment, company_frame$date)))
  names(per_day) <- c("Date", "Sentiment", "Count")
  
  per_day$scaled_sentiment <- 0
  per_day$scaled_sentiment[per_day$Sentiment=="N"] <- (per_day[per_day$Sentiment=="N", ]$Count/max(per_day[per_day$Sentiment=="N", ]$Count))
  per_day$scaled_sentiment[per_day$Sentiment=="P"] <- per_day[per_day$Sentiment=="P", ]$Count/max(per_day[per_day$Sentiment=="P", ]$Count)
  per_day_neg <- per_day[per_day$Sentiment=="N", ]
  per_day_pos <- per_day[per_day$Sentiment=="P", ]
  
  
  # ggplot(per_day, aes(x=per_day$Date, y=per_day$scaled_sentiment, fill=per_day$Sentiment)) +
  #   geom_bar(stat="identity", position="identity")

  ggplot(per_day_pos, aes(x=per_day_pos$Date, y=per_day_pos$scaled_sentiment/per_day_neg$scaled_sentiment)) +
    geom_bar(stat="identity", position="identity", fill="darkcyan") + xlab("Date") + ylab("Positive / Negative Ratio")

}


read_and_plot("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/wells_fargo.csv", "2016-02-10")
read_and_plot("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/verizon.csv", "2016-02-10")
read_and_plot("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/home_depot.csv", "2016-02-10")
read_and_plot("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/lowes.csv", "2016-02-10")
read_and_plot("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/bank_of_america.csv", "2016-02-10")
read_and_plot("~/Documents/LevelEdu/sentiment_analysis/R_Scripts/att.csv", "2016-02-10")

