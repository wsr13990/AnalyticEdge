setwd('C:/Users/User/Downloads')
ClimateChange = read.csv('climate_change.csv')
str(ClimateChange)

climate_train = subset(ClimateChange, Year <= 2006)
climate_test = subset(ClimateChange, Year > 2006)

model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train)
summary(model1)
cor(climate_train)
model2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climate_train)
summary(model2)

model3 = step(model1)
summary(model3)

prediction = predict(model3, newdata = climate_test)

SSE = sum((prediction - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2