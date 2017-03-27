setwd('D:/Analytic Edge')
mvt = read.csv('mvtWeek1.csv')
DateConvert = as.Date(mvt$Date, "%m%d%y%H:%M")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
hist(mvt$Date, breaks=100)
boxplot(Date ~ Arrest, data = mvt)
Top5 = subset(mvt,
              LocationDescription == 'STREET'|
              LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)'|
              LocationDescription == 'STREET'|
              LocationDescription == 'ALLEY'|
              LocationDescription == 'GAS STATION'|
              LocationDescription == 'DRIVEWAY - RESIDENTIAL'
              )