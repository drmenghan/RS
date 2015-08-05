# install.packages("tm")  # for text mining
# install.package("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# install.packages("RWeka") # for tokenization algorithms more complicated than single-word
# install.packages("RTextTools")
# install.packages("ggplot2")
# install.packages("https://cran.r-project.org/src/contrib/SnowballC_0.5.1.tar.gz")
# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")


# Load
library("tm")
library("ggplot2")
library("RWeka")
library("RTextTools")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("lattice")
setwd("C:\\Users\\mhan0\\Documents\\TMProject\\TM-R")

filePath = "OnlyQueryOtherListContent.txt"
text <- readLines(filePath)


# Load the data as a corpus
docs <- Corpus(VectorSource(text))

toSpace <- function (x , pattern ) gsub(pattern, " ", x)
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# docs
# View(docs)
# Convert the text to lower case
#length(docs)

#When content_transformer doesn't work, use the one below it.
# docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, tolower, mc.cores=1)
docs <- tm_map(docs, PlainTextDocument)

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
docs <- tm_map(docs, stemDocument)
# Remove your own stop word
# specify your stopwords as a character vector


OwnUselessWords = c("discover", "baccount","service","message","email","trademarks","name","information","financial")
CustomerWords = c("addit","comment","suggest","discov","interact","survey","interactionfieldvaluesurvey","feedback")
CustomerWords = c(OwnUselessWords,CustomerWords,"secur","messag","thank","please", "discov","pleas")
# length(CustomerWords)
docs <- tm_map(docs, removeWords, CustomerWords) 
# length(docs)
backup <- docs
docs <- backup

# length(docs1)
docs = backup[(0:2000)]
# docs = backup[(0:500)]
# docs = backup[(5001:10000)]
# length(docs)

# matrix <- create_matrix(dtm,ngramLength=3)

#To build Gram Tokenizer for specific size
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

# length(docs)
# docs <- docs[(0:1000)]
#Build the basic DTM
# dtm <- TermDocumentMatrix(docs,control = list(tokenize = BigramTokenizer))
dtm <- TermDocumentMatrix(docs,control = list(tokenize = BigramTokenizer))


#Basic matrix view
# inspect(dtm)
# dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 1000)
# View(head(d, 100))

# dhead = head(d, 200)
# plot(dhead)
# set.seed(1234)


#
#Cluster Dendrogram
#
tdm2 <- removeSparseTerms(dtm, sparse = 0.92)

tdm2

m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit)
rect.hclust(fit, k = 10) 



#
#Frequency Terms and Associcate Relationship
#



freq.terms <- findFreqTerms(dtm, lowfreq = 50)#, highfreq = 60)


findAssocs(dtm, "fico", 0.1)
findAssocs(dtm, "cashback", 0.1)
findAssocs(dtm, "late", 0.1)

library(graph)
library(Rgraphviz)

plot(dtm, term = freq.terms, corThreshold = 0.8, weighting = T, cex=0.5)


require(cluster)
plot(agnes(dtm, metric = "manhattan", stand = TRUE), which.plots=2,cex =5)



#Cluster Message Words by kmeans

m3<-m2
View(m3)
m3 <-t(m3)
set.seed(1234)
k<- 50

kmeansResult <-kmeans(m3,k)
round(kmeansResult$centers[i,], digits =3)


for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the key of every cluster
  # print(key[which(kmeansResult£cluster==i)])
}


# freqr <- colSums(as.matrix(dtm))
# length(freqr)                
# #create sort order (asc)
# ordr <- order(freqr,decreasing=TRUE)
# #inspect most frequently occurring terms
# freqr[head(ordr)]
# freqr[tail(ordr)]
# findFreqTerms(dtm,lowfreq=20)



#
# Topic Modeling
#

install.packages("topicmodels")
library("topicmodels")

dtmt <- t(dtm)

lda <- LDA(dtmt, k = 20)
(term <- terms(lda,10))

topic <- topics(lda, 1)

install.packages("qplot")

install.packages("ggplot2",type = "source")
require("ggplot2")

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

#topics <- data.frame(date=dtmt$nrow, topic)
#plot(dtmt, data=topics, geom="density",fill=term[topic], position="stack")




#Circle Style
# wordcloud(words = dhead$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))
# 
# #Square Style
# pal2 <- brewer.pal(8,"Dark2")
# wordcloud(dhead$word,dhead$freq, scale=c(8,.2),min.freq=3,
#           max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

require(ggplot2)

# length(d)
dhead = head(d,50)
# names(dhead)[names(dhead)=="freq"]  <- "Experimental Condition"
# 
# OtherLikeContent = head(dhead,10)
# 
pg <-ggplot(OtherLikeContent, aes(x = OtherLikeContent$word, y = OtherLikeContent$freq,color=OtherLikeContent$freq)) + 
  geom_bar(stat="identity")+ labs(x = "Frequency",
  y = "Terms",title = "Terms Frequency in Other-like Messages")+theme(title = element_text(colour="chocolate", size=16, face="bold"),legend.title = element_blank())
rpg = pg+coord_flip()
rpg

findAssocs()


source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

install.packages("graph")
install.packages("Rgraphviz")

library("graph")
library("Rgraphviz")

findAssocs(dtm,"code",0.2)
findAssocs(dtm,"card",0.25)
freq.terms <- findFreqTerms(dtm, lowfreq = 50)
plot(dtm, term = freq.terms, corThreshold = 0.1, weighting = T)

plot(dtm, term = freq.terms, corThreshold = 0.1, weighting = T)


# histogram(OtherLikeContent$word)

