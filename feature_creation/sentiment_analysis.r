
#load libraries required

library ('RSentiment') #library used for sentiment analysis
library('plyr')
library('dplyr')

#read in csv
train <- read.csv('M:\\Python Scripts\\Kaggle\\shelter-outcomes\\data\\train.csv')

#calculate frequency of unique names (perhaps names could be tidied prior to this step)
names<- count(train["Name"])

#using RSentiment, calculate score (numerical and descriptive) for each unique name
names.sentiment <- cbind(names,calculate_score(names$Name),calculate_sentiment(names$Name))

#subset to keep columns required
names.sentiment<-names.sentiment[,c("Name","calculate_score(names$Name)","sentiment")]

#rename columns to something more sensible
sentiment<- plyr::rename(names.sentiment, c("calculate_score(names$Name)"="sentiment_score","sentiment"="sentiment_desc" ))

#Join to original table to assign sentiment score to each animal id
animal.name.sentiment <- merge(x = train[c(1,2)], y = sentiment, by = "Name", all.x = TRUE  )

#write to new variables folder in GitHub repo
write.csv(animal.name.sentiment, file = "M:\\GitHub\\shelter_outcomes\\new_variables\\sentiment.csv")
