setwd('C:/Users/wahyuros/Downloads')
emails = read.csv('emails.csv', stringsAsFactors = FALSE)

table(emails$spam)
str(emails$text)

max(nchar(emails$text))
which.min(nchar(emails$text))

library(tm)
corpusEmail = Corpus(VectorSource(emails$text))
corpusEmail = tm_map(corpusEmail, tolower)
corpusEmail = tm_map(corpusEmail, PlainTextDocument)
corpusEmail = tm_map(corpusEmail, removePunctuation)
corpusEmail = tm_map(corpusEmail, removeWords, stopwords("en"))
corpusEmail = tm_map(corpusEmail, stemDocument)
dtm = DocumentTermMatrix(corpusEmail)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
colnames(spdtm) = make.names(colnames(spdtm))
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
emailsSparse$spam = emails$spam
which.max(colSums(emailsSparse))

sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))


emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

spamLog = glm(spam~., data = train, family = binomial)
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data = train, method = "class")
library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data = train)

predTrainLog = predict(spamLog, type = "response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type = "prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

summary(spamLog)
prp(spamCART)

table(train$spam, predTrainLog >0.5)
(3052+954)/nrow(train)
ROCRPredTrain = prediction(predTrainLog, train$spam)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)

table(train$spam, predTrainCART > 0.5)
(2885+894)/nrow(train)
ROCRPredTrain = prediction(predTrainCART, train$spam)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)

table(train$spam, predTrainRF >= 0.5)
(3013+918)/nrow(train)
ROCRPredTrain = prediction(predTrainRF, train$spam)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)

predTestLog = predict(spamLog, newdata = test, type = "response")
table(test$spam, predTestLog > 0.5)
(1257+376)/nrow(test)
ROCRPredTest = prediction(predTestLog, test$spam)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)

predTestCART = predict(spamCART, newdata = test)[,2]
table(test$spam, predTestCART >= 0.5)
(1228+386)/nrow(test)
ROCRPredTest = prediction(predTestCART, test$spam)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)

predTestRF = predict(spamRF, newdata = test, type = "prob")[,2]
table(test$spam, predTestRF>0.5)
(1290+384)/nrow(test)
ROCRPredTest = prediction(predTestRF, test$spam)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)