setwd('C:/Users/wahyuros/Downloads')
parole = read.csv('parole.csv')

table(parole$violator)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1 = glm(violator ~ ., data = train, family = binomial)
summary(model1)

prediction = predict(model1, newdata = test, type = "response")
summary(prediction)

table(test$violator, prediction > 0.5)
(167+12)/nrow(test)

table(test$violator)
179/(179+23)

library(ROCR)
predictTrain = predict(model1, type = "response")
ROCRPred = prediction(predictTrain, train$violator)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize= TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj= c(-0.2, 1.7))

pred = prediction(prediction, test$violator)
as.numeric(performance(pred, "auc")@y.values)