setwd("C:/Users/User/Downloads")
dailykos = read.csv("dailykos.csv", stringsAsFactors = FALSE)

distance = dist(dailykos, method = "euclidean")
kosHier = hclust(distance, method = "ward")
plot(kosHier)