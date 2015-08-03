# 
# TextMining of Shakespeare Study
# 
#Data Source: "http://www.gutenberg.org/cache/epub/100/pg100.txt"

#Reference: "http://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/"

TEXTFILE = "./data/pg100.txt"


if(!file.exists(TEXTFILE)){
  dir.create("data")
  download.file("http://www.gutenberg.org/cache/epub/100/pg100.txt", destfile = TEXTFILE)
}
#R could not identify the direction but could creat the file

TEXTFILE = "OtherListContent.txt"
shakespeare = readLines(TEXTFILE)
length(shakespeare)
backup = shakespeare
# shakespeare
#To change the default option:
# getOption("max.print")
#options(max.print = 5.5E5)

#Output the result to a file
sink(file = "./data/output.txt")
summary(shakespeare)
sink(NULL)  #Remove the sink

head(shakespeare)
tail(shakespeare)
#Delete the unusefully information
shakespeare = shakespeare[-(1:173)]
shakespeare = shakespeare[-(124195:length(shakespeare))]
shakespeare = paste(shakespeare, collapse = " ")
nchar(shakespeare)
#

shakespeare2 = strsplit(shakespeare, "\n")[[1]]
length(shakespeare)
# sink("./data/seg1.txt")
# shakespeare[1]
# sink(NULL)

#Test the difference of [1] VS [[1]]
# shakespeare[1]
# x = 1:6
# x
# x[1]
# x[[1]]
# y = rep(x, times = 5)
# y
# y = as.matrix(y,5:6)
# x = c("abc", "cde", "def", "fgh")
# mode(x[1])
# mode(x[[1]])

# ??letters
# mylist = list(lower = letters[1:4], upper = LETTERS[-1:-20])
# mylist
# mylist[1]
# mylist[[1]]
# mode(mylist[1])
# mode(mylist[[1]])

(dramatis.personae <- grep("Dramatis Personae", shakespeare, ignore.case = TRUE))
length(shakespeare)
shakespeare = shakespeare[-dramatis.personae]
length(shakespeare)

library(tm)

doc.vec = VectorSource(shakespeare)
doc.corpus = Corpus(doc.vec)
summary(doc.corpus)

doc.corpus <- tm_map(doc.corpus, tolower)
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
# stopwords("english")

library(SnowballC)

dtm <- TermDocumentMatrix(doc.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Dark2"))





doc.corpus = tm_map(doc.corpus, stemDocument)
doc.corpus = tm_map(doc.corpus, stripWhitespace)
#inspect(doc.corpus[8])

TDM = TermDocumentMatrix(doc.corpus)
TDM
inspect(TDM[1:10, 1:10])

findFreqTerms(TDM, 1000)
findAssocs(TDM, "love", 0.7)

library(wordcloud)
m = as.matrix(doc.corpus)
v = sort(colSums(m),decreasing = TRUE)
myNames = names(v)
d = data.frame(word=myNames, freq=v)
d
wordcloud(d$word, colors=c(3,4), random.color=TRUE, d$freq, min.freq=20);

?wordcloud
TDM.common = removeSparseTerms(TDM, 0.1)
dim(TDM)
dim(TDM.common)
inspect(TDM.common[1:10,1:10])

library(slam)
TDM.dense = as.matrix(TDM.common)
TDM.dense
object.size(TDM.dense)
object.size(TDM.common)

library(reshape2)
TDM.dense = melt(TDM.dense, value.name = "count")
head(TDM.dense)

library(ggplot2)

ggplot(TDM.dense, aes(x = Docs, y = Terms, fill = log10(count)))+
  geom_tile(colour = "white")+
  scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
  ylab("")+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

inspect(TDM.common[1:10,1:10])

TDM.scaled = dim(TDM.dense)
TDM.scaled
TDM.dist = dist(TDM.scaled)

library(cluster)
agnes(x = TDM.dist, method = "complete")
Height(summary)

plot(hclusters, which.plots = 2, main = "", sub = "", xlab = "")









# install.packages("tm")  # for text mining
# install.package("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes

# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

text <- readLines("OtherListContent.txt")

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

toSpace <- function (x , pattern ) gsub(pattern, " ", x)
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs
# View(docs)
# Convert the text to lower case
#length(docs)

#When content_transformer doesn't work, use the one below it.
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, tolower, mc.cores=1)

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
# docs <- tm_map(docs, stemDocument)
docs[(1:5000)]
docs1 = docs
docs = docs1
docs = docs[(1:10000)]
docs = docs[(1:000)]
length(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))





library(plyr)
library(stringr)
library(e1071)
posText <- read.delim(file='Y:/Studied/R/rt-polaritydata/rt-polarity.pos', header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim(file='Y:/Studied/R/rt-polaritydata/rt-polarity.neg', header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))

x <- c(0,10,11,12,13)
y <- c(1,NA,NA,NA,NA)
z <- c(2,20,21,22,23)    
a <- c(0,6,5,4,3)
b <- c(1,7,8,9,10)
c <- c(2,NA,NA,NA,NA)
df1 <- data.frame(x,y,z)
df2 <- data.frame(a,b,c)

df1




#Study R Introduction



help.start()
?solve
??solve
source("TestTextMining.R")
sink("record.lis")
?sink
sink.number()
objects()
ls()
?Quotes

x = 5
y = x[!is.na(x)]
z = (x+1)[!is.na(x) & x>0]
y
z

z = 1:1500
z
dim(z) = c(3,5,100)
View(z)
i = array(c(1:3, 4:6), dim = c(3:2))
View(i)

attach(faithful)
summary(eruptions)
View(eruptions)
View(faithful)
?attach
?stem
stem(eruptions)
?rug
hist(eruptions, seq(1.6, 5.2, 0.2), prob = TRUE)
rug(eruptions)
lines(density(eruptions, bw = 0.1))




lst = list(m0 = formula(mpg~wt), m2 = formula(mpg~cyl), m3 = formula(mpg~hp))
output = lapply(lst, lm, data = mtcars)
lapply(output,summary)

ls()
rm(list = ls())
ls()

mm = 1:6
as.matrix(mm)
mm
aa = "1,2,3,4,5"
as.matrix(aa)
is.matrix(aa)


mod = lm(mpg~wt,mtcars)
plot(mod)




