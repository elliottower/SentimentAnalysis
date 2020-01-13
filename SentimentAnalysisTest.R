#Sentiment Analysis Tests
# install.packages("SentimentAnalysis")
library(SentimentAnalysis)

# Analyze a single string to obtain a binary response (positive / negative)
sentiment <- analyzeSentiment("Yeah, this was a great soccer game for the German team!")
convertToBinaryResponse(sentiment)$SentimentQDAP

# Import data of news headlines
doc <- read.csv(file="C:/Users/Elliot/Documents/VU Amsterdam/R Projects/R Introduction/WebScrapingR/iran_2019_headlines.csv", header=TRUE, sep=",")
doc$headline.main = as.character(doc$headline.main)

headlines <- doc$headline.main # Only need the headlines for now
Encoding(headlines) =  " UTF8"
headlines <- iconv(headlines, from = "UTF-8", to = "ASCII", sub = "") # Convert to ASCII, remove strange characters

# Analyze sentiment
my_corpus = corpus(headlines)
senti = dfm(my_corpus, dictionary = data_dictionary_LSD2015)

first13 = headlines[0:13] #First 13 lines to test
first30 = headlines[0:30] #First 30 lines to test

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


plotSentimentResponse(doc$pub_date,sentiment$SentimentQDAP, ylab="Sentiment Response", xlab="Date")
plot(doc$pub_date,sentiment$SentimentQDAP, ylab="Sentiment Response", xlab="Date")

sentiment$SentimentQDAP

first30
sentiment$SentimentQDAP[14:20]
response[14:20]

headlines
