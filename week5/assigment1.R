setwd('C:/Users/wahyuros/Downloads')
wiki = read.csv('wiki.csv', stringsAsFactors = FALSE)

library(tm)

table(wiki$Vandal)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords('english'))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAddded = removeSparseTerms(dtmAdded, 0.997)
sparseAddded

wordsAdded = as.data.frame(as.matrix(sparseAddded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)


corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords('english'))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

set.seed(123)
library(caTools)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, split == TRUE)
wikiTest = subset(wikiWords, split == FALSE)

table(wikiTest$Vandal)
618/nrow(wikiTest)

library(rpart)
library(rpart.plot)
wikiRpart = rpart(Vandal~., data = wikiTrain, method = "class")
prediction = predict(wikiRpart, newdata = wikiTest, type = "class")
table(wikiTest$Vandal, prediction)
(618+12)/nrow(wikiTest)

prp(wikiRpart)
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split == TRUE)
wikiTest2 = subset(wikiWords2, split == FALSE)
wikiRpart = rpart(Vandal~., data = wikiTrain2, method = "class")
prediction = predict(wikiRpart, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, prediction)
(609+57)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3 = subset(wikiWords2, split == TRUE)
wikiTest3 = subset(wikiWords2, split == FALSE)
wikiRpart = rpart(Vandal~., data = wikiTrain3, method = "class")
prediction = predict(wikiRpart, newdata = wikiTest3, type = "class")
table(wikiTest2$Vandal, prediction)
(514+248)/nrow(wikiTest2)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, split == TRUE)
wikiTest4 = subset(wikiWords3, split == FALSE)
wikiRpart = rpart(Vandal~., data = wikiTrain4, method = "class")
prediction = predict(wikiRpart, newdata = wikiTest4, type = "class")
table(wikiTest4$Vandal, prediction)
(595+248)/nrow(wikiTest4)
prp(wikiRpart)
