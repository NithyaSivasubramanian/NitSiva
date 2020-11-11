setwd("C://ExcelR//Assignments//Text Mining")
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library(devtools)
library("twitteR")
install.packages("ROAuth")
library("ROAuth")
# developer.twitter.com, when u create ur own account u will get all the API keys
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("yi7hT1dPt3sSVtk8lCTIZC6eg", # Consumer Key (API Key)
                    "vXCIb8zC8fqFczB657tpfL7yi7iWiy4fyhh33g618SReSOyQjh", #Consumer Secret (API Secret)
                    "396521281-BsCEGBJJGGFs8rmummLbmDa0lRXI1Jtevv4VYDD4",  # Access Token
                    "hVlx2TvOFwfs7o8xu3oOeMTL81XCiMd0nbrcBHP20k9pJ")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('msdhoni', n = 15,includeRts = T)
TweetsDF_Dhoni <- twListToDF(Tweets)
dim(TweetsDF_Dhoni)
View(TweetsDF_Dhoni)
write.csv(TweetsDF_Dhoni, "Tweets_N_Dhoni.csv",row.names = F)
Tweets <- userTimeline('imVkohli', n = 15,includeRts = T)
TweetsDF_Kholi <- twListToDF(Tweets)
dim(TweetsDF_Kholi)
View(TweetsDF_Kholi)
write.csv(TweetsDF_Kholi, "Tweets_N_Kholi.csv",row.names = F)

getwd()

#sentiment analysis 


setwd("C://Users//ExcelR//Assignments//Text Mining")
getwd()
library(readr)
require(graphics)
library(tm)
twitter.sub <- read.csv("Tweets_N_Dhoni.csv")
twitter.sub[1]
corpus <- Corpus(VectorSource(twitter.sub[1]))


#sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub='byte'))

# clean up the corpus using tm_map()
corpus <- tm_map(corpus, (tolower))

inspect(corpus)
inspect(corpus[10])

# remove numbers from the comments 
corpus <- tm_map(corpus, removeNumbers)

inspect(corpus[1])


stopwords("english")
# removing stopwords , punctuation, whitespaces
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus)

# create a document-term sparse matrix
dtm <- TermDocumentMatrix(corpus, 
                              control = list(minWordLength=c(1,Inf)))
dtm
findFreqTerms(dtm, lowfreq = 2)
# create a document-term sparse matrix

#Barplot
# rowsums of all words
termFrequency <- rowSums(as.matrix(dtm))
termFrequency
#Barplot

# selecting the which are repeated more than 10 times 
termFrequency <- subset(termFrequency, termFrequency>=2)
termFrequency

library(ggplot2)
barplot(termFrequency,las=2, col = rainbow(20))

install.packages("wordcloud")
library(wordcloud)
m <- as.matrix(dtm)

wordFreq <- sort(rowSums(m), decreasing=TRUE)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 20, random.order = F, col=gray.colors(1))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 20, random.order = F, colors=rainbow(20))

install.packages("wordcloud2")
library(wordcloud2)
W_data <- data.frame(names(termFrequency),termFrequency)
colnames(W_data) <- c('word','freq')
window()
wordcloud2(W_data, size = 0.5,shape = 'triangle')



# user review on IMDB
# https://www.imdb.com/title/tt0418279/reviews


setwd("C://Users//ExcelR//Assignments//Text Mining")
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library(devtools)
library("twitteR")
install.packages("ROAuth")
library("ROAuth")
# developer.twitter.com, when u create ur own account u will get all the API keys
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("yi7hT1dPt3sSVtk8lCTIZC6eg", # Consumer Key (API Key)
                    "vXCIb8zC8fqFczB657tpfL7yi7iWiy4fyhh33g618SReSOyQjh", #Consumer Secret (API Secret)
                    "396521281-BsCEGBJJGGFs8rmummLbmDa0lRXI1Jtevv4VYDD4",  # Access Token
                    "hVlx2TvOFwfs7o8xu3oOeMTL81XCiMd0nbrcBHP20k9pJ")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('msdhoni', n = 15,includeRts = T)
TweetsDF_Dhoni <- twListToDF(Tweets)
dim(TweetsDF_Dhoni)
View(TweetsDF_Dhoni)
write.csv(TweetsDF_Dhoni, "Tweets_N_Dhoni.csv",row.names = F)
Tweets <- userTimeline('imVkohli', n = 15,includeRts = T)
TweetsDF_Kholi <- twListToDF(Tweets)
dim(TweetsDF_Kholi)
View(TweetsDF_Kholi)
write.csv(TweetsDF_Kholi, "Tweets_N_Kholi.csv",row.names = F)

getwd()

#sentiment analysis 


setwd("C://Users//data Science//ExcelR//Assignments//Text Mining")
getwd()
library(readr)
require(graphics)
library(tm)
twitter.sub <- read.csv("Tweets_N_Dhoni.csv")
twitter.sub[1]
corpus <- Corpus(VectorSource(twitter.sub[1]))


#sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub='byte'))

# clean up the corpus using tm_map()
corpus <- tm_map(corpus, (tolower))

inspect(corpus)
inspect(corpus[10])

# remove numbers from the comments 
corpus <- tm_map(corpus, removeNumbers)

inspect(corpus[1])


stopwords("english")
# removing stopwords , punctuation, whitespaces
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus)

# create a document-term sparse matrix
dtm <- TermDocumentMatrix(corpus, 
                          control = list(minWordLength=c(1,Inf)))
dtm
findFreqTerms(dtm, lowfreq = 2)
# create a document-term sparse matrix

#Barplot
# rowsums of all words
termFrequency <- rowSums(as.matrix(dtm))
termFrequency
#Barplot

# selecting the which are repeated more than 10 times 
termFrequency <- subset(termFrequency, termFrequency>=2)
termFrequency

library(ggplot2)
barplot(termFrequency,las=2, col = rainbow(20))

install.packages("wordcloud")
library(wordcloud)
m <- as.matrix(dtm)

wordFreq <- sort(rowSums(m), decreasing=TRUE)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 20, random.order = F, col=gray.colors(1))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 20, random.order = F, colors=rainbow(20))

install.packages("wordcloud2")
library(wordcloud2)
W_data <- data.frame(names(termFrequency),termFrequency)
colnames(W_data) <- c('word','freq')
window()
wordcloud2(W_data, size = 0.5,shape = 'triangle')

