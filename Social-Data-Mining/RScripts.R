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


############################################################
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

clean_tweet <- function(tweet) {
  # Convert text to UTF-8 encoding
  tweet <- iconv(tweet, "UTF-8", "UTF-8", sub = "byte")
  
  # Remove URLs
  tweet <- gsub("http\\S+|www\\.[^\\s]+", "", tweet)
  
  # Remove hashtags
  tweet <- gsub("#\\S+", " ", tweet)
  
  # Remove Twitter handles
  tweet <- gsub("@\\S+", " ", tweet)
  
  # Remove control codes and punctuation
  tweet <- gsub("[[:punct:]]", " ", tweet)
  
  # Remove any non-alphabetic characters (except space)
  tweet <- gsub("[^a-zA-Z\\s]", " ", tweet)
  
  # Remove extra whitespaces
  tweet <- gsub("\\s+", " ", tweet)
  
  return(tweet)
}

# Now you can use clean_tweet to clean your tweets
tweets_Green.df$text <- sapply(tweets_Green.df$text , clean_tweet)
tweets_National.df$text <- sapply(tweets_National.df$text, clean_tweet)
tweets_Labour.df$text <- sapply(tweets_Labour.df$text, clean_tweet)

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

######### Construct words corpus ###########################

