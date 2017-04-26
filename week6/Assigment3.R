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

