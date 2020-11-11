setwd("C://Users////Assignments//Text Mining")
getwd()
library(readr)
require(graphics)
library(tm)
text_pos <- readLines("positive-words.txt")
text_neg <- readLines("negative-words.txt")
text_pos
text_neg
corpus_pos <- Corpus(VectorSource(text_pos))
corpus_neg <- Corpus(VectorSource(text_neg))

#sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub='byte'))

# clean up the corpus using tm_map()
corpus_clean_pos <- tm_map(corpus_pos, (tolower))
corpus_clean_neg <- tm_map(corpus_neg, (tolower))

inspect(corpus_clean_pos)
inspect(corpus_clean_pos)

inspect(corpus_clean_pos[10])

inspect(corpus_clean_neg[10])

# remove numbers from the comments 
corpus_clean_pos <- tm_map(corpus_clean_pos, removeNumbers)
corpus_clean_neg <- tm_map(corpus_clean_neg, removeNumbers)

inspect(corpus_clean_pos[1])
inspect(corpus_clean_neg[3076])

stopwords("english")
# removing stopwords , punctuation, whitespaces
corpus_clean_pos <- tm_map(corpus_clean_pos, removeWords, stopwords())
corpus_clean_pos <- tm_map(corpus_clean_pos, removePunctuation)
corpus_clean_pos <- tm_map(corpus_clean_pos, stripWhitespace)
inspect(corpus_clean_pos)

corpus_clean_neg <- tm_map(corpus_clean_neg, removeWords, stopwords())
corpus_clean_neg <- tm_map(corpus_clean_neg, removePunctuation)
corpus_clean_neg <- tm_map(corpus_clean_neg, stripWhitespace)
inspect(corpus_clean_neg)

# create a document-term sparse matrix
dtm_pos <- TermDocumentMatrix(corpus_clean_pos, 
                          control = list(minWordLength=c(1,Inf)))
dtm_pos
findFreqTerms(dtm_pos, lowfreq = 2)
# create a document-term sparse matrix
dtm_neg <- TermDocumentMatrix(corpus_clean_neg, 
                              control = list(minWordLength=c(1,Inf)))
dtm_neg
findFreqTerms(dtm_neg, lowfreq = 2)

#Barplot
# rowsums of all words
termFrequency_pos <- rowSums(as.matrix(dtm_pos))
termFrequency_pos
#Barplot
# rowsums of all words
termFrequency_neg <- rowSums(as.matrix(dtm_neg))
termFrequency_neg

# selecting the which are repeated more than 10 times 
termFrequency_pos <- subset(termFrequency_pos, termFrequency_pos>=2)
termFrequency_pos

# selecting the which are repeated more than 10 times 
termFrequency_neg <- subset(termFrequency_pos, termFrequency_pos>=10)
termFrequency_neg

library(ggplot2)
barplot(termFrequency_pos,las=2, col = rainbow(20))
barplot(termFrequency_neg,las=2, col = rainbow(20))

install.packages("wordcloud")
library(wordcloud)
m_pos <- as.matrix(dtm_pos)
m_neg <- as.matrix(dtm_neg)

wordFreq_pos <- sort(rowSums(m_pos), decreasing=TRUE)
wordcloud(words=names(wordFreq_pos), freq=wordFreq_pos, min.freq = 20, random.order = F, col=gray.colors(1))
wordcloud(words=names(wordFreq_pos), freq=wordFreq_pos, min.freq = 20, random.order = F, colors=rainbow(20))

wordFreq_neg <- sort(rowSums(m_neg), decreasing=TRUE)
wordcloud(words=names(wordFreq_neg), freq=wordFreq_neg, min.freq = 20, random.order = F, col=gray.colors(1))
wordcloud(words=names(wordFreq_neg), freq=wordFreq_neg, min.freq = 20, random.order = F, colors=rainbow(20))


install.packages("wordcloud2")
library(wordcloud2)
W_data_pos <- data.frame(names(termFrequency_pos),termFrequency_pos)
colnames(W_data_pos) <- c('word','freq')
window()
wordcloud2(W_data_pos, size = 0.5,shape = 'triangle')

W_data_neg <- data.frame(names(termFrequency_neg),termFrequency_neg)
colnames(W_data_neg) <- c('word','freq')
window()
wordcloud2(W_data_neg, size = 0.5,shape = 'triangle')
