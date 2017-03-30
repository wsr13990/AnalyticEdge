setwd('C:/Users/wahyuros/Downloads')
flu = read.csv('FluTrain.csv')

flu$Week[which.max(flu$ILI)]
flu$Week[which.max(flu$Queries)]

hist(flu$ILI)
plot(log(flu$ILI), flu$Queries)