setwd("C:/Users/wahyuros/Downloads")
stocks = read.csv("StocksCluster.csv")
str(stock)
nrow(stock)

table(stock$PositiveDec)
6324/(6324+5256)

correlation = cor(stock, stock)
summary(stock)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec~., data = stocksTrain, family = "binomial")
predictTrain = predict(StocksModel, type = "response")
table(stocksTrain$PositiveDec, predictTrain >= 0.5)
(990+3460)/nrow(stocksTrain)
predictTest = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, predictTest >= 0.5)
(417+1553)/nrow(stocksTest)

table(stocksTest$PositiveDec)
1897/nrow(stocksTest)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)

set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

stocksModel1 = glm(PositiveDec~., data = stocksTrain1, family = "binomial")
stocksModel2 = glm(PositiveDec~., data = stocksTrain2, family = "binomial")
stocksModel3 = glm(PositiveDec~., data = stocksTrain3, family = "binomial")
summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel3)

predictTest1 = predict(stocksModel1, newdata = stocksTest1, type = "response")
predictTest2 = predict(stocksModel2, newdata = stocksTest2, type = "response")
predictTest3 = predict(stocksModel3, newdata = stocksTest3, type = "response")
table(stocksTest1$PositiveDec, predictTest1 >= 0.5)
(30+774)/nrow(stocksTest1)
table(stocksTest2$PositiveDec, predictTest2 >= 0.5)
(388+757)/nrow(stocksTest2)
table(stocksTest3$PositiveDec, predictTest3 >= 0.5)
(49+13)/nrow(stocksTest3)

AllPredictions = c(predictTest1, predictTest2, predictTest3)
AllOutcome = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcome, AllPredictions >= 0.5)
(467+1544)/(467+1110+353+1544)