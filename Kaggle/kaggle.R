library(mice)
library(caTools)
library(caret)

setwd("C:/Users/User/Downloads")
data2016 = read.csv("train2016.csv")
summary(data2016)

#Imputing Data
imputedData = mice(data2016)
summary(imputedData)
completeData = complete(imputedData)
completeData = completeData[,-1]
summary(completeData)
completeData$Age <- 2016 - completeData$YOB
completeData <- completeData[,-1]

set.seed(1)
split = sample.split(completeData$Party, SplitRatio = 0.7)
train2016 = subset(completeData, split == TRUE)
test2016 = subset(completeData, split == FALSE)
summary(train2016)
summary(test2016)


#Remove Outliers
train2016 = train2016[!train2016$YOB %in% boxplot.stats(train2016$YOB)$out,]

#Preprocess
preprocess <- preProcess(train2016, method=c("center", "scale"))
train2016 <- predict(preprocess, train2016)
test2016 <- predict(preprocess, test2016)

#CART Model
library(rpart)
library(rpart.plot)
cart = rpart(Party~., data = train2016, method = "class")
predTrainCart = predict(cart, type = "class")
table(train2016$Party)
bottomLine = 2066/nrow(train2016)
table(train2016$Party, predTrainCart)
cartTrainAccuracy = (1452+1016)/nrow(train2016)
predTestCart = predict(cart, newdata = test2016, type = "class")
table(test2016$Party, predTestCart)
cartTestAccuracy = (616+393)/nrow(test2016)

#Random Forest
library(randomForest)
rf = randomForest(Party~., data = train2016)
predTrainRF = predict(rf)
table(train2016$Party,predTrainRF)
rfTrainAccuracy = (1443+993)/nrow(train2016)
predTestRf = predict(rf, newdata = test2016)
table(test2016$Party, predTestRf)
rfTestAccuracy = (633+403)/nrow(test2016)