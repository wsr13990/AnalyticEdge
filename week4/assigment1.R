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

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
rpart.plot(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2, digits = 6)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3, digits = 6)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)

CARTmodel5 = rpart(voting ~ sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

CARTmodel6 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel6, digits = 6)

logReg2 = glm(voting ~ sex + control, data = gerber, family = binomial)
summary(logReg2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logReg2, newdata=Possibilities, type="response")
0.2908065-0.290456

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
0.2904558 - 0.290456