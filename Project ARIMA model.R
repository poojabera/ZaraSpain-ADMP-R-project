install.packages("forecast")
library(forecast)
data<- read.csv("C:\\Users\\Prajakta\\OneDrive\\Desktop\\homework\\time series\\ADMP5years data.csv")
tsdata<-ts(data$Volume,frequency =356,start=c(2016, 1))
# plot the time-series formatted data 
plot(tsdata)
# use auto.arima() function to get the optimal auto arima model 
autoarimal<-auto.arima(tsdata)
# get forcast data for a period of 27 week i.e 6 month
forecast1<-forecast(autoarimal, h=27)
forecast1
# plot the forecasted data from the auto arima model
plot(forecast1)
#plot residuals over time to see variance
plot(forecast1$residuals)
#plot the residuals (sample vs theoretical)
qqnorm(forecast1$residuals)
#
acf(forecast1$residuals)
pacf(forecast1$residuals)
# get the accuracy by MAPE and other leading factors (method 1)
summary(autoarimal)
#method 2
accuracy(autoarimal)
