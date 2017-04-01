setwd('C:/Users/User/Downloads')
loans = read.csv('loans.csv')
loans_imputed = read.csv('loans_imputed.csv')

str(loans_imputed)
summary(loans_imputed)

set.seed(144)
library(caTools)

split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_imputed, split == TRUE)
test = subset(loans_imputed, split == FALSE)

model1 = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(model1)

predicted.risk = predict(model1, newdata = test, type = "response")
test$predicted.risk = predicted.risk

table(test$not.fully.paid, test$predicted.risk > 0.5)
accuracy = (3+2400)/nrow(test)
table(test$not.fully.paid)
baseline_accuracy = 2413/nrow(test)

library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

model2 = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
predicted.risk = predict(model2, newdata = test, type = "response")
test$predicted.risk = predicted.risk
summary(test$predicted.risk)

pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)

highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
summary(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoan = subset(highInterest, predicted.risk <= cutoff)
selectedLoan = selectedLoan[1:100,]
sum(selectedLoan$profit)
table(selectedLoan$not.fully.paid)