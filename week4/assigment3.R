setwd('C:/Users/User/Downloads')
census = read.csv('Census.csv')

str(census)
set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)
censusglm  = glm(over50k ~., data = train, family = "binomial")
summary(censusglm )

prediction = predict(censusglm, newdata = test, type = "response")
table(test$over50k, prediction >= 0.5)
(9051+1888)/nrow(test)
table(test$over50k)
baseline = 9713/nrow(test)

library(ROCR)
ROCRPred = prediction(prediction, test$over50k)
ROCRPerf = performance(ROCRPred, 'tpr', 'fpr')
plot(ROCRPerf)
auc = as.numeric(performance(ROCRPred, "auc")@y.values)

library(rpart)
censusTree = rpart(over50k ~., data = train, method = "class")
prp(censusTree)

prediction = predict(censusTree, newdata = test, type="class")
table(test$over50k, prediction)
(1596+9243)/nrow(test)

prediction = predict(censusTree, newdata = test)
prediction = prediction[,2]
ROCRPred2 = prediction(prediction, test$over50k)
ROCRPerf2 = performance(ROCRPred2, 'tpr', 'fpr')
plot(ROCRPerf)
auc = as.numeric(performance(ROCRPred2, "auc")@y.values)


set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
library(randomForest)
censusForest = randomForest(over50k~., data = trainSmall)
prediction = predict(censusForest, newdata = test)
table(test$over50k, prediction)
(9614+1050)/nrow(test)

vu = varUsed(censusForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

varImpPlot(censusForest)

library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

censusTree = rpart(over50k ~., data = train, method = "class", cp = 0.002)
prediction = predict(censusTree, newdata = test, type="class")
table(test$over50k, prediction)
(9178+1838)/nrow(test)
prp(censusTree)