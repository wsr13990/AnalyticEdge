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
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
which.max(colSums(emailsSparse))

sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))

