setwd('C:/Users/wahyuros/Downloads')
songs = read.csv('songs.csv')
MichaelJackson = subset(songs, songs$artistname == "Michael Jackson")
MichaelJackson$songtitle[which(MichaelJackson$Top10 == 1)]

table(songs$timesignature)
songs$songtitle[which(songs$tempo == max(songs$tempo))]

SongsTrain = subset(songs, songs$year <= 2009)
SongsTest = subset(songs, songs$year == 2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~., data = SongsTrain, family = binomial)
summary(SongsLog1)

cor(SongsTrain$energy, SongsTrain$loudness)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

SongPredict = predict(SongsLog3, newdata = SongsTest, type ='response')
table(SongsTest$Top10, SongPredict > 0.45)
accuracy = (309+19)/nrow(SongsTest)


