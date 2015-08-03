setwd("Y:/Studying/R")
library("mboost")
str(iris)
#data("bodyfat", package = "mboost")
data("bodyfat", package = "TH.data")
str(bodyfat)
View(bodyfat)

#File Operation
a = 1:10
save(a, file = "dumData.Rdata")
a = 10
load("dumData.Rdata")
a

var1 = 1:5
var2 = (1:5)/10
var3 = c("R", "and", "Data Mining", "Examples", "Case Studies")
#var3 = "R and Data Mining Examples Case Studies"
#var3 = strsplit(var3, " ", fixed = TRUE)
var3
# var3[3] = c(var3[3], var3[4])
# var3[4] = NULL
# var3[1]
##?? How to split a string and assign them to a list??

df1 = data.frame(var1, var2, var3)
names(df1) = c("VariableInt", "VariableReal", "VariableChar")
write.csv(df1, "dummyData.csv", row.names = FALSE)

df2 = read.csv("dummyData.csv")
print(df2)


#Communication with SAS
library(foreign)
#Create empty SAS table

#originalHome = "C:/Program Files/SAS/9.3"
# newHome = chartr(old = "\\", new = "/", originalHome)
##?? Transfer path to R accepted version??
sashome = "C:/Program Files/SAS/9.3"
#egain_jun2013.sas7bdat
fileName = "egain_jun2013.sas7bdat"
a = read.ssd(file.path("."), fileName, sascmd = file.path(sashome, "sas.exe"))


install.packages("sas7bdat")
library(sas7bdat)
# mydata = read.sas7bdat("egain_jun2013.sas7bdat")
library(sas7bdat)
helpfromSAS = read.sas7bdat("http://www.math.smith.edu/sasr/datasets/help.sas7bdat")
is.data.frame(helpfromSAS)
View(helpfromSAS)

Mydata = read.csv("egain_apr2014.csv")

View(Mydata)


getwd()
#Have a look at data
dim(iris)
?dim
names(iris)
str(iris)
attributes(iris)

iris[1:10,]

head(iris)
tail(iris)

iris[1:10,"Sepal.Length"]
iris$Sepal.Length[1:10]
summary(iris)
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(.1, .3, .65))

var(iris$Sepal.Length)
hist(iris$Sepal.Length)

plot(iris$Sepal.Length)
plot(density(iris$Sepal.Length))
iris$Species
table(iris$Species)
pie(table(iris$Species))
barplot(table(iris$Species),col = colors(distinct = TRUE))

#Explore Multiple variables
cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])

aggregate(Sepal.Length ~ Species, summary, data = iris)

boxplot(Sepal.Length~Species, data = iris)
attributes(iris)

with(iris, plot(Sepal.Length, Sepal.Width, col = Species, pch = as.numeric(Species)))
# col set the classification by Species 
?pch

plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Length))
pairs(iris)
?pairs

#3D Plot
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)
library(rgl)
plot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

distMatrix = as.matrix(dist(iris[,1:5]))
heatmap(distMatrix)

library(lattice)
levelplot(Petal.Width~Sepal.Length*Sepal.Width, iris, cuts = 9, col.regions = grey.colors(10)[10:1])

data("volcano")
filled.contour(volcano, color = terrain.colors, asp = 1, plot.axes = contour(volcano, add = T))
persp(volcano, theta = 25, phi = 30, expand = 0.5, col = "lightblue")
attributes(volcano)
View(volcano)
head(volcano)
volcano[10,]
volcano[10,1:3]
volcano[1:10,1:3]

library(MASS)
parcoord(iris[1:4], col = iris$Species)


library(lattice)
parallelplot(~iris[1:4]|Species, data = iris)

library(ggplot2)

qplot(Sepal.Length, Sepal.Width, data = iris, facets = Species ~.)

getwd()
#Save Charts
pdf ("myPlot1.pdf")
x = 1:50
plot(x,log(x))
graphics.off()

postscript("myPlot2.ps")
x = -20:20
plot(x,x^2)
graphics.off()


#Ch4 

#4.1 Predict iris
str(iris)
set.seed(123)
ind = sample(2,nrow(iris), replace = TRUE, prob = c(0.7,0.3))
ind
?sample
trainData = iris[ind ==1,]
testData = iris[ind ==2,]

#install.packages("party")
library(party)
#install.packages(party)
?ctree

myFormula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree = ctree(myFormula, data = trainData)
table(predict(iris_ctree), trainData$Species)

print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type = "simple")


testPred = predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

#4.2 Predict bodyfat
install.packages("rpart")
# pkgTest("rpart")
#Test a package
is.installed = function(mypkg) is.element(mypkg, installed.packages()[,1]) 
is.installed("rpart")
installed.packages()
length(installed.packages())
installed.packages()[1]
#
#
library(rpart)
data("bodyfat", package = "TH.data")
dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]
head(bodyfat)

names(bodyfat)

set.seed(123)
ind = sample(2,nrow(bodyfat), replace = TRUE, prob = c(0.7,0.3))
bodyfat.train = bodyfat[ind == 1,]
bodyfat.test = bodyfat[ind == 2,]
myFormula = DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart = rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)

print(bodyfat_rpart$cptable)
print(bodyfat_rpart)
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n = T)

opt = which.min(bodyfat_rpart$cptable[,"xerror"])
cp = bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune = prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)

plot(bodyfat_prune)
text(bodyfat_prune, use.n = T)
?text
DEXfat_pred = predict(bodyfat_prune, newdata = bodyfat.test)
xlim = range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data = bodyfat.test, xlab = "Observed", ylab = "Predicted", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)
abline(a = 1, b = 1)
?abline
# plot(DEXfat_pred ~)
