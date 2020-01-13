#Sentiment Analysis Tests
# install.packages("SentimentAnalysis")
library(SentimentAnalysis)

# Import data of news headlines
doc <- read.csv(file="C:/Users/Elliot/Documents/VU Amsterdam/R Projects/R Introduction/WebScrapingR/iraq_2003_headlines.csv", header=TRUE, sep=",")
doc$headline = as.character(doc$headline)
doc$headline

headlines <- doc$headline # Only need the headlines for now
Encoding(headlines) =  " UTF8"
headlines <- iconv(headlines, from = "UTF-8", to = "ASCII", sub = "") # Convert to ASCII, remove strange characters
headlines
# Analyze sentiment
#my_corpus = corpus(headlines)
#senti = dfm(my_corpus, dictionary = data_dictionary_LSD2015)

sentiment <- analyzeSentiment(headlines)
#?analyzeSentiment

# Extract dictionary-based sentiment according to the QDAP dictionary
sentiment$SentimentQDAP

# View sentiment direction (i.e. positive, neutral and negative)
convertToDirection(sentiment$SentimentQDAP)

response <- c(1, 0, -1, 0, -1, -1, 0, -1, -1, 0,
              +1, -1, -1, -1, 1, 1, -1, 1, -1, -1,
              1,-1, 1, -1, -1, -1,-1, -1, -1, -1)
#compareToResponse(sentiment, response)
compareToResponse(sentiment, convertToBinaryResponse(response))

plotSentimentResponse(sentiment$SentimentQDAP, response)

plotSentimentResponse(time,sentiment$SentimentQDAP)

sentiment$SentimentQDAP
date = doc$pub_date
date
doc$pub_date
doc[order(as.Date(doc$pub_date, format="%m/%d/%Y")),]
doc$pub_date
plot(doc$pub_date, sentiment$SentimentQDAP)
plot(doc$pub_date, sentiment$SentimentQDAP, main="Sentiment over time", ylab="Sentiment Response", xlab="Date")
lines(lowess(doc$pub_date, sentiment$SentimentQDAP), col = "blue")
plotSentimentResponse(doc$pub_date,sentiment$SentimentQDAP, ylab="Sentiment Response", xlab="Date")
?plotSentimentResponse()
#plot(doc$pub_date, sentiment$SentimentQDAP)

first30
sentiment$SentimentQDAP[14:20]
response[14:20]

headlines
