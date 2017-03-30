library(zoo)
setwd('C:/Users/User/Downloads')
train = read.csv('fluTrain.csv')

train$Week[which.max(train$ILI)]
train$Week[which.max(train$Queries)]

hist(train$ILI)
plot(log(train$ILI), train$Queries)
model = lm(log(ILI) ~ Queries, data = train)
summary(model)
cor(log(train$ILI), train$Queries)

test = read.csv('FluTest.csv')
predTest = exp(predict(model, newdata = test))
predTest[which(test$Week == '2012-03-11 - 2012-03-17')]/

(test$ILI[which(test$Week == '2012-03-11 - 2012-03-17')] - predTest[which(test$Week == '2012-03-11 - 2012-03-17')])/test$ILI[which(test$Week == '2012-03-11 - 2012-03-17')]
SSE = sum((test$ILI - predTest)^2)
RMSE = sqrt(SSE/nrow(test))

test$Week

ILILag2 = lag(zoo(train$ILI), -2, na.pad=TRUE)
train$ILILag2 = coredata(ILILag2)
plot(log(train$ILILag2), log(train$ILI))

model2 = lm(log(ILI) ~ Queries + log(ILILag2), data = train)
summary(model2)

ILILag2 = lag(zoo(test$ILI), -2, na.pad=TRUE)
test$ILILag2 = coredata(ILILag2)
summary(test)

test$ILILag2[1] = train$ILI[nrow(train)-1]
test$ILILag2[2] = train$ILI[nrow(train)]

predTest2 = exp(predict(model2, newdata = test))
SSE2 = sum((test$ILI - predTest2)^2)
RMSE2 = sqrt(SSE2/nrow(test))