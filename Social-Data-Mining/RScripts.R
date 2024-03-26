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

####################### Construct words corpus ###########################

