setwd('C:/Users/wahyuros/Downloads')
train = read.csv('pisa2009train.csv')
test = read.csv('pisa2009test.csv')

str(train)
summary(train)

train = na.omit(train)
test = na.omit(test)

table(train$raceeth)
train$raceeth = relevel(train$raceeth, 'White')
test$raceeth = relevel(test$raceeth, 'White')

