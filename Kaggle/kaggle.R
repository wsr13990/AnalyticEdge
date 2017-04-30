setwd("C:/Users/wahyuros/Downloads")
data2016 = read.csv("train2016.csv")
summary(data2016)

library(caTools)
split = sample.split(data2016$Party, SplitRatio = 0.7)
train2016 = subset(data2016, split == TRUE)
test2016 = subset(data2016, split == FALSE)

#CART Model
library(rpart)
library(rpart.plot)
cart = rpart(Party~., data = train2016, method = "class")
predTrainCart = predict(cart, type = "class")
table(train2016$Party)
bottomLine = 2066/nrow(train2016)
table(train2016$Party, predTrainCart)
cartTrainAccuracy = (1387+1092)/nrow(train2016)
predTestCart = predict(cart, newdata = test2016, type = "class")
table(test2016$Party, predTestCart)
cartTestAccuracy = (543+448)/nrow(test2016)

#Random Forest
library(randomForest)
rf = randomForest(Party~., data = train2016)
predTrainRF = predict(rf, newda)

