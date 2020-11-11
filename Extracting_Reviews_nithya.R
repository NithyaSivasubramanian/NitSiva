setwd("C://Users//Users//Assignments//Text Mining")
getwd()
library(rvest)
install.packages("XML")
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Redmi-8A-Dual-White-Storage/product-reviews/B086977J48?reviewerType=all_reviews"
aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
#  "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"amazon_reviews_RedmiN.txt",row.names = F)
getwd()


# Snapdeal reviews #############################
surl_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
surl_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"samsung.txt",row.names = FALSE)
getwd()
# Sample urls 
# url  = http://www.amazon.in/product-reviews/B01LXMHNMQ/ref=cm_cr_getr_d_paging_btm_4?ie=UTF8&reviewerType=all_reviews&showViewpoints=1&sortBy=recent&pageNumber=1
# url = http://www.amazon.in/Moto-G5-GB-Fine-Gold/product-reviews/B01N7JUH7P/ref=cm_cr_getr_d_paging_btm_3?showViewpoints=1&pageNumber=1
# url = http://www.amazon.in/Honor-6X-Grey-32GB/product-reviews/B01FM7JGT6/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber=1

# sentiment analysis on amazon_reviews_RedmiN.txt

setwd("C://Users//Users//Assignments//Text Mining")
getwd()
library(readr)
require(graphics)
library(tm)

redmi_rev <- readLines("amazon_reviews_RedmiN.txt")


corpus <- Corpus(VectorSource(redmi_rev))


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
termFrequency <- subset(termFrequency, termFrequency>=5)
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
############################################################################
################## aSSIGNMENT extracting movie reviews from IMDB
############ and sentiment analysis


setwd("C://Users//Assignments//Text Mining")
getwd()
library(rvest)
install.packages("XML")
library(XML)
library(magrittr)

# IMDB url for transformers movie
aurl <- "https://www.imdb.com/title/tt3371366/reviews?ref_=tt_ql_3"
# IMDB url for aqua man movie
aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"


imdb_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  imdb_reviews <- c(imdb_reviews,rev)
}
View(imdb_reviews)
write.table(imdb_reviews,"imdb_reviews_aqua.txt",row.names = F)
getwd()
# sentiment analysis on transformers reviews

setwd("C://Users//Assignments//Text Mining")
getwd()
library(readr)
require(graphics)
library(tm)

trans_rev <- readLines("imdb_reviews_transformers.txt")
View(trans_rev)

corpus <- Corpus(VectorSource(trans_rev))

# clean up the corpus using tm_map()
corpus <- tm_map(corpus, (tolower))

inspect(corpus)

# remove numbers from the comments 
corpus <- tm_map(corpus, removeNumbers)

inspect(corpus[10])

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
findFreqTerms(dtm, lowfreq = 20)
# create a document-term sparse matrix

#Barplot
# rowsums of all words
termFrequency <- rowSums(as.matrix(dtm))
termFrequency
#Barplot

# selecting the which are repeated more than 10 times 
termFrequency <- subset(termFrequency, termFrequency>=30)
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

#################### assignment question 3
############# choose anything from internet and read reviews and perform sentiment analysis
## choosing review on practo
# https://www.trustpilot.com/review/practo.com#:~:text=though%20outside%20they%20charge%20high,recommend%20practo%20to%20any%20one.


setwd("C://Users//Assignments//Text Mining")
getwd()
library(rvest)
install.packages("XML")
library(XML)
library(magrittr)

#Extracted the Customer reviews from Amazon on
#"MSI GT63 TITAN-052 15.6" 120Hz 3ms G-Sync Extreme Gaming Laptop"
# amazon 
aurl <- "https://www.amazon.com/MSI-GT63-TITAN-052-Extreme-i7-8750H/product-reviews/B07CSFW5Y1/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"


Gaming_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  Gaming_reviews <- c(Gaming_reviews,rev)
}
View(Gaming_reviews)
write.table(Gaming_reviews,"Gaming_reviews_amazon.txt",row.names = F)
getwd()
# sentiment analysis on transformers reviews

setwd("C://Users//Assignments//Text Mining")
getwd()
library(readr)
require(graphics)
library(tm)

trans_rev <- readLines("Gaming_reviews_amazon.txt")
View(trans_rev)

corpus <- Corpus(VectorSource(trans_rev))


# clean up the corpus using tm_map()
corpus <- tm_map(corpus, (tolower))

inspect(corpus[10])

# remove numbers from the comments 
corpus <- tm_map(corpus, removeNumbers)

inspect(corpus[10])

stopwords("english")
# removing stopwords , punctuation, whitespaces
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus)

# create a document-term sparse matrix
dtm <- TermDocumentMatrix(corpus, control = list(minWordLength=c(1,Inf)))
dtm
findFreqTerms(dtm, lowfreq = 20)
# create a document-term sparse matrix

#Barplot
# rowsums of all words
termFrequency <- rowSums(as.matrix(dtm))
termFrequency
#Barplot

# selecting the which are repeated more than 10 times 
termFrequency <- subset(termFrequency, termFrequency>=30)
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
