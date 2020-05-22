setwd("C:\\Users\\Devam N Shah\\Desktop\\Rutgers\\Sem-2\\Applied Regression Analysis\\Project\\Final Code,Deck & Data")
library(xlsx)
library(tseries) 
library(forecast)
library(ggplot2)

#reading data

data_covid=read.xlsx("Data.xlsx",1)
data_covid
#Plot of no of Confirmed Cases from March to May

ggplot(data = data_covid, aes(x = Date, y = Confirmed)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Confirmed  Cases in US ",
       x = "Date", y = "No of Confirmed Cases")

#Plot for confirmed cases
ts.plot(data_covid$Confirmed,main="Confirmed Cases by Day from 15th March-2nd May")

# dataframe for confirmed
data_confirmed = data_covid[2]
data_confirmed

tsdata_confirmed = ts(data_confirmed$Confirmed)
tsdata_confirmed

# Dickey-fuller test to check if the data is stationary

adf.test(tsdata_confirmed) 

tsdata_confirmed_1 <- diff(tsdata_confirmed, differences = 2)
adf.test(tsdata_confirmed_1) # Since P-value is less than 0.01, data is stationary and d=2

#Finding ACF
a = acf(tsdata_confirmed_1)
a  # lag order q = 0

# Finding PACF
p = pacf(tsdata_confirmed_1)
p   # lag order p = 1

fit1 <- arima(data_confirmed$Confirmed, order = c(1,2,0))
fit1
accuracy(fit1)

#Forecasting
forecast1 = forecast(fit1,h=10)
forecast1 #Number of Confirmed Cases will reach 14,12,368 in next ten days
plot(forecast1)


fit2 <- auto.arima(data_confirmed$Confirmed)
fit2
accuracy(fit2)



#Plot of no of Active Cases from March to May
ggplot(data = data_covid, aes(x = Date, y = Active)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Active Cases in US ",
       x = "Date", y = "No of Active Cases")

#Plot for Active cases
ts.plot(data_covid$Active,main="Active Cases by Day from 15th March-2nd May")

# dataframe for Active
data_active = data_covid[5]
data_active

tsdata_active= ts(data_active$Active)
tsdata_active

# Dickey-fuller test to check if the data is stationary

adf.test(tsdata_active) 

tsdata_active_1 <- diff(tsdata_active, differences = 4)
adf.test(tsdata_active_1) # Since P-value is less than 0.01, data is stationary and d=4

#Finding ACF
a = acf(tsdata_active_1)
a  # lag order q = 1

# Finding PACF
p = pacf(tsdata_active_1)
p   # lag order p = 3

fit3 <- arima(data_active$Active, order = c(3,4,1))
fit3

accuracy(fit3)

#Forecasting
forecast2 = forecast(fit3,h=10)
forecast2 #Number of Active Cases will reach 10,85,521 in next ten days
plot(forecast2)

fit4 <- auto.arima(data_active$Active)
fit4
accuracy(fit4)


#Plot of no of Deaths from March to May
ggplot(data = data_covid, aes(x = Date, y = Death)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Deaths in US ",
       x = "Date", y = "No of Deaths")
#Plot for Deaths
ts.plot(data_covid$Death,main="Number of Deaths by Day from 15th March-2nd May")

# dataframe for Deaths
data_death = data_covid[3]
data_death
tsdata_death= ts(data_death$Death)
tsdata_death

# Dickey-fuller test to check if the data is stationary

adf.test(tsdata_death) 

tsdata_death_1 <- diff(tsdata_death, differences = 2)
adf.test(tsdata_death_1) 
#p value=0.01, therefore stationary , d=2

#Finding ACF
a = acf(tsdata_death_1)
a  # lag order q = 3

# Finding PACF
p = pacf(tsdata_death_1)
p   # lag order p = 5

fit5 <- arima(data_death$Death, order = c(5,2,3))
fit5
accuracy(fit5)

#Forecasting
forecast3 = forecast(fit5,h=10)
forecast3 #Number of Deaths will reach 82765 in next ten days
plot(forecast3)

fit6 <- auto.arima(data_death$Death)
fit6
accuracy(fit6)


#Plot of no of Recovered cases from March to May
ggplot(data = data_covid, aes(x = Date, y = Recovered)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Recovered cases in US ",
       x = "Date", y = "No of Recovered Cases")

#Plot for Recovered cases
ts.plot(data_covid$Recovered,main="Recovered Cases by Day from 15th March-2nd May")

# dataframe for Recovered
data_recovered = data_covid[4]
data_recovered

tsdata_recovered= ts(data_recovered$Recovered)
tsdata_recovered

# Dickey-fuller test to check if the data is stationary

adf.test(tsdata_recovered) 

tsdata_recovered_1 <- diff(tsdata_recovered, differences = 2)
adf.test(tsdata_recovered_1) # Since P-value is less than 0.01, data is stationary and d=2

#Finding ACF
a = acf(tsdata_recovered_1)
a  # lag order q = 1

# Finding PACF
p = pacf(tsdata_recovered_1)
p   # lag order p = 0

fit7 <- arima(data_recovered$Recover, order = c(0,2,1))
fit7

accuracy(fit7)

#Forecasting
forecast4 = forecast(fit7,h=10)
forecast4 #Number of recovered Cases will reach 283674 in next ten days
plot(forecast4)

fit8 <- auto.arima(data_recovered$Recovered)
fit8
accuracy(fit8)

