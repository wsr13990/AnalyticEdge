setwd('C:/Users/User/Downloads')
gerber = read.csv('Gerber.csv')
str(gerber)
table(gerber$voting)
108696/nrow(gerber)

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

library(caTools)
logReg = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(logReg)

prediction = as.matrix(predict(logReg, type = 'response'))
table(gerber$voting, prediction >= 0.3)
(51966+134513)/nrow(gerber)

table(gerber$voting, prediction >= 0.5)
(235388)/nrow(gerber)

library(ROCR)
ROCRPred = prediction(prediction, gerber$voting)
auc = as.numeric(performance(ROCRPred, 'auc')@y.values)