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

model = lm(readingScore ~ ., data = train)
summary(model)
prediction = predict(model, newdata = test)
SSE = sum((test$readingScore - prediction)^2)
RMSE = sqrt(SSE/nrow(test))
RMSE

SST = sum((test$readingScore - mean(train$readingScore))^2)
R2 = 1- SSE/SST
