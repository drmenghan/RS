library(tm)
library(SnowballC)
library(qdap)
library(qdapDictionaries)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(Rgraphviz)
#system("for f in *.pdf; do pdftotext -enc ASCII7 $f; done")
#getReaders()
cname <- file.path("Y:/Studied/R/rt-polaritydata")
# cname
# length(dir(cname))
# dir(cname)
docs<- Corpus(DirSource(cname))
# class(docs)
# class(docs[[1]])
# summary(docs)
inspect(docs[3])
#getTransformations()
#toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
# docwithoutnumber<-tm_map(docs[3], removePunctuation)
#docwithoutnumber<-tm_map(docs[3], removeWords, c("type","data"))

dtm<- DocumentTermMatrix(docs)
inspect(dtm)
freq<- colSums(as.matrix(dtm))
length(freq)
ord<-order(freq)
freq[head(ord)]

head(table(freq),15)
tail(table(freq),15)
m<- as.matrix(dtm)
dim(m)
write.csv(m, file = "dtm.csv")

dtms <- removeSparseTerms(dtm,0.1)
dim(dtms)
dim(dtm)
inspect(dtms)

freq<- colSums(as.matrix(dtms))
table(freq)

findFreqTerms(dtm, lowfreq = 1000)

findAssocs(dtm, "data", corlimit=0.6)

plot(dtm,
     term = findFreqTerms(dtm, lowfreq = 500)[1:50],
     corThreshold = 0.5)
#Very slow to draw the graph
freq<- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq,14)

wf<- data.frame(word = names(freq), freq = freq)
head(wf)
word = names(freq)


ggplot(wf,aes(word,freq))
d<-ggplot(wf,aes(word,freq))
d<-d+geom_line()
# Still questionable
subset(wf, freq>500)
ggplot(wf,aes(word,freq))
geom_bar(stat = "identity")
theme(axis.text.x = element_text(angle = 45, hjust = 1))

set.seed(123)
wordcloud(names(freq),freq, min.freq = 400)
subset(wf, freq>5)
freq<- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
freq
set.seed(142)
wordcloud::wordcloud(names(freq),freq, min.freq = 10, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))

words<- dtms
length(words)


#
# Rlecture Study by Yaqing Si
#

plot(cumsum(rnorm(100)), type = "l", col = "red", lwd = 2)

library(ggplot2)
View(diamonds)
summary(diamonds)
avgSize <- round(mean(diamonds$carat), 4)
clarity <- levels(diamonds$clarity)
q<- qplot(carat, price, 
          data = diamonds, color = clarity,
          xlab = "Carat",
          ylab = "Price", 
          main = "Diamond Pricing")
q

getwd()
setwd("Y:/Studied/R")
getwd()
?getwd()

x = 1
v1 = c(1, 2, 3, 4, 5, 6)
v1
v2 = 1:50
v2
seq(-2,2,0.1)
seq(-2,2,length = 11)
?seq

s = rep(3,10)
s
?rep
x = 1:5
x
rep(x, each =3)
rep(x, times =2)
rep(x, each =3, times =2)

#Append value
append(x,9)

#Vector Computing
x = 1:3
y = x + 1
x + y
sin(x)
sum(x)
max(x)
which.max(x)
?which.max
?c
rev(x)
x = c(x,x,x)
x
order(x)
sort(x)
rev(x)
x = 1:20

#Matrix
z = matrix(x, 4, 5)
z
z = matrix(x, 4, 5, byrow = TRUE)
z

rbind(z, z)
cbind(z, z)

z[2,]
z[, 4]
rownames(z) = c("a", "b", "c", "d")
colnames(z) = c("e", "f", "g", "h", "i")
length(z)
z["a",]
z^2

z[c("b", "c"),]

qr(z)
svd(z)

#String
s = c("Sunday","Monday", "Tuesday", "Wndesday", "Friday", "Saturday")
s[2:3]
toupper(s)
tolower(s)

nchar(s)
substr(s, 1,3)
substr(s, nchar(s) - 2, nchar(s))
substr(s, nchar(s) - 1, nchar(s))

strsplit(s,"e")

x = c(as = "asfef", qu = "qwerty", "stuff.blah.yech")
strsplit(x, "e")

strsplit(x, "e")


strsplit("a.b.c", ".", fixed = TRUE)
strsplit("a.b.c", ".")

paste(s)
paste(s, collapse = "_")
n = 0:6
paste(s, n, sep=":")
paste(n, s, sep=":")

grade = c("A", "1", 100, "100", "a", 1, "*", TRUE, "Z")
factor(grade)

ordered(grade)

levels(grade)

levels(ordered(grade))

k = as.numeric(grade)

k = as.numeric(ordered(grade))
k


#Test matching

str = "pestars_pb_12	studars_pb_11	sc_thin_wo_fico_pb_14	pepco_pb_s_13	PUBREC_m	PUB100_m	PUB250_m"
splitstr = strsplit(str, "\t", fixed = TRUE )
splitstr
R = grepl("_m", "PUBREC_m")
R = grepl("_m", "studars_pb_11")
R
length(splitstr)
for(i in 1:length(splitstr)){
  # if(grepl("_m", i)){
  print(i)
  # }
}

#Time and date
today = Sys.Date()
today
now = Sys.time()
now
zone = Sys.timezone()
zone

format(now, "%a %b %d %X %Y")

quarters(now)

Lst = list(name = "Fred", wife = "Mary", no.children = 3, child.ages = c(4,7,9))
Lst$name
Lst$name = "husband"
Lst
names(Lst)[1] = "husband"
Lst$name = "Mike"
Lst$husband = "Mike"
Lst$name = NULL
Lst


#File Operation
getwd()
dir(getwd())
myData = read.table("mdl_trt3_sample_recs.txt", sep = "\t", header = TRUE)
x = myData
myrownames = rownames(x)
mycolnames = colnames(x)
myrownum = nrow(x)
mycolnum = ncol(x)
myrownum #500  DONE
View(x)
#PUBREC_m
#TL061_m
R = grep("_m", colnames(x))
x[,R]
l = sapply(x[,R], sd)

is.numeric('')
View(l)

plot(l,col = colors(distinct = TRUE))


is.dummy = function(y){
  sum(y %in% c(0,1)) == length(y)
}

is.dummy(c(0,0,1,1))


for(i in 1:length(colnames)){
  
  grep
  
}



mydata_m = subset(x, subset = (x1 = "PUBREC_m" & x2 = "TL061_m"), select = c(x1:x2))
mydata_m = myData[,c('PUBREC_m':'TL061_m')]
mydata_m
x[2,3]
x[1,2]

x.



data()$results
data = read.table("clipboard")
write.table("clipboard")

####################################################
#Function
myTri = function(a,b){
  z = sqrt(a^2+b^2)
  e = c(a,b,z)
  m = "This is a right triangle"
  s = list(Message = m, Area = a*b, Edge = e)
  return(s)
}
myTri(3,4)
myTri(3,4)$Message



#Print Function
sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%5.3f", pi)
sprintf("%05.1f", pi)


exp.taylor = function(x,k){
  n = 1:k
  z = x^n/factorial(n)
  y = 1 + sum(z)
  return(y)
}


sd()
exp.taylor(0.5,3)


#Plot
a = 1:10
plot(a)
b = 2:11
plot(a,b)

par(mfrow = c(2,3))
par(mar = c(3,3,3,.1))
for(i in 1:6){
  x = rgamma(100,i,i^2)
  hist(x, col = "grey80")
}
par(mfrow = c(1,1))



#Additional Study from Team
ls()
rm()
options()

xx = savenumber = 0
?save()

example("table")
apropos("table")
data()
library(RODBC)
