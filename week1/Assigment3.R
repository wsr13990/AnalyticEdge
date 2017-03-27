setwd('C:/Users/User/Downloads')
CPS = read.csv('CPSData.csv')
str(CPS)
summary(CPS)

sort(table(CPS$State))
nrow(subset(CPS, CPS$Citizenship == 'Citizen, Native'|CPS$Citizenship == 'Citizen, Naturalized'))/nrow(CPS)
table(CPS$Race, CPS$Hispanic)

table(CPS$Age, is.na(CPS$Married))
table = table(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = read.csv('MetroAreaCodes.csv')
countryMap = read.csv('CountryCodes.csv')
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sort(table(CPS$MetroArea))