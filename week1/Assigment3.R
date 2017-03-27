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
sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

CPS = merge(CPS, countryMap, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x = TRUE)
tapply(CPS$Country != 'United States', CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" , mean, na.rm = TRUE)
sort(tapply(CPS$Country == 'Somalia', CPS$MetroArea, sum, na.rm = TRUE))