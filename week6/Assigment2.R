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

tapply(airline$Balance, clusterGroup, mean)
tapply(airline$QualMiles, clusterGroup, mean)
tapply(airline$BonusMiles, clusterGroup, mean)
tapply(airline$BonusTrans, clusterGroup, mean)
tapply(airline$FlightMiles, clusterGroup, mean)
tapply(airline$FlightTrans, clusterGroup, mean)
tapply(airline$DaysSinceEnroll, clusterGroup, mean)

set.seed(88)
airlineKMean = kmeans(airlinesNorm, centers = 5)
table(airlineKMean$cluster)


table(clusterGroup, airlineKMean$cluster)