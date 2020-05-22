setwd("C:\\Users\\Devam N Shah\\Desktop\\Rutgers\\Sem-2\\Applied Regression Analysis\\Project\\Final Code,Deck & Data")

library(xlsx)
library(tseries) 
library(forecast)
library(ggplot2)

#reading data
data_covid = read.xlsx("Data.xlsx",1)
data_covid

tsdata_confirmed= ts(data_covid[,2])
tsdata_confirmed

tsdata_death= ts(data_covid[,3])
tsdata_death

tsdata_recovered= ts(data_covid[,4])
tsdata_recovered

tsdata_active= ts(data_covid[,5])
tsdata_active

fit_confirmed <- tslm(log(tsdata_confirmed) ~ trend)
fit_confirmed
summary(fit_confirmed) # Adjusted R-squared - 83.76%

fc_confirmed <- forecast(fit_confirmed)
plot(fc_confirmed,main="Confirmed Cases in US")

fit_death <- tslm(log(tsdata_death) ~ trend)
fit_death
summary(fit_death) # Adjusted R-squared - 88.32%

fc_death <- forecast(fit_death)
plot(fc_death,main="Deaths in US")

fit_recovered <- tslm(log(tsdata_recovered) ~ trend)
fit_recovered
summary(fit_recovered) # Adjusted R-squared - 87.76%

fc_recovered <- forecast(fit_recovered)
plot(fc_recovered,main="Recovered Cases in US")

fit_active <- tslm(log(tsdata_active) ~ trend)
fit_active
summary(fit_active) # Adjusted R-squared - 82.13%

fc_active <- forecast(fit_active)
plot(fc_active,main="Active Cases in US")