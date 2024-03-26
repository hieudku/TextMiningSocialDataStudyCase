getwd()
dir_path <- "C:\\Users\\hieu.cu_it.weltec\\OneDrive - Whitireia and WelTec\\Social-Data\\Assignment1\\DS6501-Assignment1-TextMiningSocialData\\Social-Data-Mining"
setwd(dir_path)
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

#############################################################
# Create Corpus from text files contained in Political folder
docs <- Corpus(DirSource("Political Parties"))

# View summary of the corpus
summary(docs)

# View the structure of the corpus
str(docs)

# Get a list of documents in the corpus
names(docs)

##########################
tweets_Green.df2 <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
tweets_Green.df2 <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})

# Now we can use this content transformer to eliminate colons and hyphens 
docs <- tm_map(docs, tweets_Green.df2, "-")
docs <- tm_map(docs, tweets_Green.df2, "http\\S+")
docs[[1]]$content


###############################

# Remove punctuation - replace punctuation marks with a space
docs <- tm_map(docs, removePunctuation)

#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))

#Strip digits (std transformation - no need for content_transformer)
docs <- tm_map(docs, removeNumbers)

#remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

#Strip whitespace 
docs <- tm_map(docs, stripWhitespace)

#######################
#Info about the docs
# View summary of the corpus
summary(docs)

# View the structure of the corpus
str(docs)

# Get a list of documents in the corpus
names(docs)

# Access the content of a specific document
docs[[1]]$content

#######################

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

tweets_Green.df2 <- tweets_Green.df$text
tweets_National.df2 <- tweets_National.df$text
tweets_Labour.df2 <- tweets_Labour.df$text

head(tweets_Green.df2)
head(tweets_National.df2)
head(tweets_Labour.df2)
# Use a 'find and replace' function to remove garbage from tweets
tweets_Green.df2 <- gsub("http.*","",tweets_Green.df2)
tweets_National.df2 <- gsub("http.*","",tweets_National.df2)
tweets_Labour.df2 <- gsub("http.*","",tweets_Labour.df2)

# The '.* means it will remove all text to the right of the pattern
# found - http in the example above. This would clear the whole tweet
# if it begin with this pattern, so be careful.

tweets_Green.df2 <- gsub("https.*","",tweets_Green.df2)
tweets_National.df2 <- gsub("https.*","",tweets_National.df2)
tweets_Labour.df2 <- gsub("https.*","",tweets_Labour.df2)

# Remove everything shown between the square brackets - very useful
# Avoids striping the whole tweet, unlike '.*' option
tweets_Green.df2 <- gsub("[\\]+","",tweets_Green.df2)
tweets_National.df2 <- gsub("[\\]+","",tweets_National.df2)
tweets_Labour.df2 <- gsub("[\\]+","",tweets_Labour.df2)

tweets_Green.df2 <- gsub("#.*","",tweets_Green.df2)
# Display clean text
head(tweets.df2)

tweets_Green.df2 <- gsub("@.*","",tweets_Green.df2)
tweets.df2 <- gsub("…","",tweets.df2)
#tweets.df2 <- gsub("@","",tweets.df2) # Airline tweets

# tweets.df2 <- gsub(".","",tweets.df2)

# Display clean text
head(tweets.df2)