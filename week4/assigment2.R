setwd('C:/Users/User/Downloads')
letter = read.csv('letters_ABPR.csv')
str(letterAPBR)

letter$isB = as.factor(letter$letter == 'B')
table(letter$isB)
baseline = 1-766/nrow(letter)

set.seed(1000)
library(caTools)
split = sample.split(letter$isB, SplitRatio = 0.5)
train = subset(letter, split == TRUE)
test = subset(letter, split == FALSE)
table(test$isB)
baseline = 1175/nrow(test)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
prediction = predict(CARTb, newdata = test, type = "class")
table(test$isB, prediction)
accuracy1 = (340+1118)/nrow(test)

set.seed(1000)
library(randomForest)
forest = randomForest(isB ~ . - letter, data=train)
prediction2 = predict(forest, newdata = test)
table(test$isB, prediction2)
accuracy2 = (1165+374)/nrow(test)

letter$letter = as.factor( letter$letter )
set.seed(2000)
split = sample.split(letter$letter, SplitRatio = 0.5)
train = subset(letter, split == TRUE)
test = subset(letter, split == FALSE)

table(test$letter)
baseline = 401/nrow(test)


CARTb = CARTb = rpart(letter ~ ., data=train, method="class")
prediction = predict(CARTb, newdata = test, type = "class")
table(test$letter, prediction)
accuracy = (348+318+363+340)/nrow(test)

set.seed(1000)
forest = randomForest(letter ~., data = train)
prediction = predict(forest, newdata = test)
table(test$letter, prediction)
accuracy = (390+380+393+369)/nrow(test)