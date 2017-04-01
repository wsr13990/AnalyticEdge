setwd('C:/Users/wahyuros/Downloads')
songs = read.csv('songs.csv')
MichaelJackson = subset(songs, songs$artistname == "Michael Jackson")
MichaelJackson$songtitle[which(MichaelJackson$Top10 == 1)]

table(songs$timesignature)
songs$songtitle[which(songs$tempo == max(songs$tempo))]

songsTrain = subset(songs, songs$year <= 2009)
songsTest = subset(songs, songs$year == 2010)
