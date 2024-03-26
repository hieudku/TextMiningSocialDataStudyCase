getwd()
dir_path <- "C:\\Users\\hieu.cu_it.weltec\\OneDrive - Whitireia and WelTec\\Social-Data\\Assignment1\\DS6501-Assignment1-TextMiningSocialData\\Social-Data-Mining"
setwd(dir_path)

# Load libraries into R Studio
library(SnowballC)
library(tm)
library(syuzhet)
library(wordcloud)
library(RColorBrewer)

# Install packages into R Studio
install.packages("SnowballC")
install.packages("tm")
install.packages("syuzhet")
install.packages("wordcloud")


####################### Pre-processing tweets ###########################

# Import tweets
tweets_Green.df <- read.csv("./Political Parties/NZGreens_tweets.csv")
tweets_National.df <- read.csv("./Political Parties/NZNationalParty_tweets.csv")
tweets_Labour.df <- read.csv("./Political Parties/nzlabour_tweets.csv")

# Display tweets
head(tweets_Green.df)
head(tweets_National.df)
head(tweets_Labour.df)

# Display 'text' field of data frame
head(tweets_Green.df$text)
head(tweets_National.df$text)
head(tweets_Labour.df$text)

# Convert text to UTF-8 encoding
tweets_Green.df$text <- iconv(tweets_Green.df$text, "UTF-8", "UTF-8", sub = "byte")
tweets_National.df$text <- iconv(tweets_National.df$text, "UTF-8", "UTF-8", sub = "byte")
tweets_Labour.df$text <- iconv(tweets_Labour.df$text, "UTF-8", "UTF-8", sub = "byte")
  
# Remove URLs, Twitter handles, hashtags, and anything inside <>
tweets_Green.df$text <- gsub("http\\S+|www\\.[^\\s]+|@\\S+|#\\S+|<[^>]+>", "", tweets_Green.df$text)
tweets_National.df$text <- gsub("http\\S+|www\\.[^\\s]+|@\\S+|#\\S+|<[^>]+>", "", tweets_National.df$text)
tweets_Labour.df$text <- gsub("http\\S+|www\\.[^\\s]+|@\\S+|#\\S+|<[^>]+>", "", tweets_Labour.df$text)

# Remove control codes, punctuation, and any non-alphabetic characters (except space)
tweets_Green.df$text <- gsub("[[:cntrl:]]|[[:punct:]]|[^a-zA-Z0-9\\s]", " ", tweets_Green.df$text)
tweets_National.df$text <- gsub("[[:cntrl:]]|[[:punct:]]|[^a-zA-Z0-9\\s]", " ", tweets_National.df$text)
tweets_Labour.df$text <- gsub("[[:cntrl:]]|[[:punct:]]|[^a-zA-Z0-9\\s]", " ", tweets_Labour.df$text)

# Display cleaned tweets
head(tweets_Green.df$text)
head(tweets_National.df$text)
head(tweets_Labour.df$text)

tweets_Green.df2 <- tweets_Green.df$text
tweets_National.df2 <- tweets_National.df$text
tweets_Labour.df2 <- tweets_Labour.df$text

head(tweets_Green.df2)
head(tweets_National.df2)
head(tweets_Labour.df2)

####################### Sentiment Analysis ###########################

#### Green Party ####

# Convert data frame into a vector before performing sentiment analysis
word_Green.df <- as.vector(tweets_Green.df2)

# Perform sentiment analysis to score tweets on emotion
emotion_Green.df <- get_nrc_sentiment(word_Green.df)

# Combine tweets to sentiment scores
emotion_Green.df2 <- cbind(tweets_Green.df2, emotion_Green.df) 

head(emotion_Green.df2)

# Score tweets based on positive and negative sentiment
sent.value_Green <- get_sentiment(word_Green.df)

# Filter based on positive, negative and neutral tweets
positive.tweets_Green <- word_Green.df[sent.value_Green > 0]

negative.tweets_Green <- word_Green.df[sent.value_Green < 0]

neutral.tweets_Green <- word_Green.df[sent.value_Green == 0]

# Count number of positive, negative and neutral tweets

pos_Green <- length(positive.tweets_Green)

neut_Green <- length(neutral.tweets_Green)

neg_Green <- length(negative.tweets_Green)

# Using the values in pos, neut and neg, we can plot a pie chart
# First combine the values and assign to a vector
x <- c(pos_Green, neut_Green, neg_Green)

# Now define the labels to be used in pie chart 
labels_Green <- c("Positive", "Negative", "Neutral")

# Finally, plot the chart
pie(x, labels_Green, main = "Green Party's Sentiment Analysis", col = rainbow(length(x)))

# Select the most positive sentiment (highest sent.value)
most.positive_Green <- word_Green.df[sent.value_Green == max(sent.value_Green)]

# Display tweet with most positive sentiment
most.positive_Green

# Select the most negative sentiment (lowest sent.value)
most.negative_Green <- word_Green.df[sent.value_Green <= min(sent.value_Green)]

# Display tweet with most negative sentiment
most.negative_Green

# Create a corpus (collection of words) from our data frame of cleaned tweets
tweet_corpus_Green <- Corpus(VectorSource(word_Green.df))

# create term document matrix applying some transformations
tdm_Green <- TermDocumentMatrix(tweet_corpus_Green,
                          control = list(removeNumbers = TRUE, wordLengths=c(5, 15),
                                         stopwords = c("Green", "green", "greens", "Party", "party", "government", "Government", "tweet", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix so we can calculate word frequencies
tdm.matrix_Green <- as.matrix(tdm_Green)

# get word counts in decreasing order
word_freqs_Green <- sort(rowSums(tdm.matrix_Green), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm_Green <- data.frame(word=names(word_freqs_Green), freq=word_freqs_Green)

# plot wordcloud with maximum of 50 words, excluding stopwords
wordcloud(dm_Green$word, dm_Green$freq, min.freq = 10, max.words = 50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#####################
