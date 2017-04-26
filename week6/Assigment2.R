setwd("C:/Users/wahyuros/Downloads")
airline = read.csv("AirlinesCluster.csv")
summary(airline)

library(caret)
preproc = preProcess(airline)
airlinesNorm = predict(preproc, airline)
summary(airlinesNorm)

distance = dist(airlinesNorm)
airlineHier = hclust(distance, method = "ward.D")
plot(airlineHier)
clusterGroup = cutree(airlineHier, k=5)
table(clusterGroup)

