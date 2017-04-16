setwd('C:/Users/wahyuros/Downloads')
clinical_trial = read.csv('clinical_trial.csv', stringsAsFactors = FALSE)

summary(clinical_trial)
str(clinical_trial)

max(nchar(clinical_trial$abstract))
sum(nchar(clinical_trial$abstract) == 0)
clinical_trial$title[which(nchar(clinical_trial$title)==min(nchar(clinical_trial$title)))]

library(tm)
library(SnowballC)

corpusTitle = Corpus(VectorSource(clinical_trial$title))
corpusAbstract = Corpus(VectorSource(clinical_trial$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords('english'))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords('english'))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
which.max(colSums(dtmAbstract))
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = clinical_trial$trial

set.seed(144)
library(caTools)
split = sample.split(dtm$trial, SplitRatio = 0.7)
dtm_train = subset(dtm, split == TRUE)
dtm_test = subset(dtm, split == FALSE)
table(dtm_train$trial)
730/nrow(dtm_train)

library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data = dtm_train, method = "class")
prp(trialCART)
trialCART2 = rpart(trial~., data = dtm_train)
predTrain = predict(trialCART)[,2]
max(predTrain)

table(dtm_train$trial, predTrain > 0.5)
(631+441)/nrow(dtm_train)
441/(441+131)
631/(631+99)


predTest = predict(trialCART, newdata = dtm_test)[,2]
table(dtm_test$trial, predTest > 0.5)
(261+162)/nrow(dtm_test)

library(ROCR)
ROCRPredTest = prediction(predTest, dtm_test$trial)
auc = as.numeric(performance(ROCRPredTest, "auc")@y.values)