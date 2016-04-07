import csv
from textblob import TextBlob

file1 = open('/Users/Lucien/Documents/LevelEdu/sentiment_analysis/R_Scripts/pos_neg_labeled.csv', 'rb')
reader = csv.reader(file1)
new_csv = []
for row in reader:
	text = row[2].decode('utf-8')
	text = TextBlob(text)
	row.append(text.sentiment.polarity)
	new_csv.append(row)
file1.close()


file2 = open('/Users/Lucien/Documents/LevelEdu/sentiment_analysis/R_Scripts/pos_neg_labeled.csv', 'wb')
writer = csv.writer(file2)
writer.writerows(new_csv)
file2.close()





		
			