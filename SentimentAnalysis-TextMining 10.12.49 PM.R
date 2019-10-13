# Read file

apple <- read.csv(file.choose(), header = T)

#view the original text
#readLines('apple.csv',10)

#structure of the csv file
#str(apple)

#install required packages
#install.packages(c('tm',syuzhet', 'lubridate', 'ggplot2', 'scales', 'reshape2','dplyr'))

###################################3

library(tm)

# Clean text

# Build corpus
corpus <- iconv(apple$text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#change every word into lower cases
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

#remove punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

#remove numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

#remove stopwords
#check stopwords command  ## stopwords()
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#remove URL if necssary 

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

#remove reduntant words
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))

cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

#remove whitespace 
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix
#give frequencies of the particular words 

tdm <- TermDocumentMatrix(cleanset)
tdm

#check frequencies of words for 20 tweets
tdm <- as.matrix(tdm)
tdm[1:20, 1:20]

# Bar plot
w <- rowSums(tdm) #count the words 
w <- subset(w, w>=30)
barplot(w,
        las = 2,
        col = rainbow(50))

# Sentiment analysis

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
#apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = 'utf-8-mac')

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment(c('delay', 'ugly', 'down'))

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')


#################################
