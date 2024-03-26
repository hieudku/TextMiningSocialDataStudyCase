getwd()

# Install packages into R Studio
install.packages("SnowballC")
install.packages("tm")
install.packages("syuzhet")
install.packages("wordcloud")

# Load libraries into R Studio
library(SnowballC)
library(tm)
library(syuzhet)
library(wordcloud)

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

tweets_Green.df2 <- tweets_Green.df$text
tweets_National.df2 <- tweets_National.df$text
tweets_Labour.df2 <- tweets_Labour.df$text

