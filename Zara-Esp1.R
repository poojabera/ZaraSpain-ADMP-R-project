
library("readxl")
read <- read_excel("C:\\Users\\Pooja Bera\\OneDrive\\Documents\\rennes school\\time series\\Project on Time Series-Zara SPain and ADMP\\ZARA_ESP.xls")

zara=read$SALES

head(zara)

# info data
str(zara)
class(zara)



# to transforme data to ts data 
zaraseries <- ts(zara, frequency=12, start=c(2010,2))
class(zaraseries)
plot.ts(zaraseries)
zaraseries

zaratimeseriecomponent= decompose(zaraseries)
zaratimeseriecomponent$seasonal
plot(zaratimeseriecomponent)

zaratimeseriesseasonallyadjusted = zaraseries - zaratimeseriecomponent$seasonal
plot(zaratimeseriesseasonallyadjusted)

acf(zaratimeseriesseasonallyadjusted, lag.max=20, na.action = na.omit)


# plot data

plot.ts(zaraseries)
zaraseries

# use my SMA forecast model. 
#We decided to use exponencial smoothing eventhough we now there is seasonality to show the forecast that we may obtain using not an acurate model

zaraseriesforecasts <- HoltWinters(zaraseries, beta=FALSE, gamma=FALSE)

zaraseriesforecasts

plot(zaraseriesforecasts)

zaraseriesforecasts$fitted
plot(zaraseriesforecasts)

# acuracy rate
zaraseriesforecasts$SSE

### bns
new_forecast = HoltWinters(zaraseries, beta=FALSE, gamma=FALSE, l.start=15214.59)

# forecast model with Forecast library 

library("forecast")


# to forecast n future period  ( h = 12)

zaraseriesforecasts2 <- forecast:::forecast.HoltWinters(zaraseriesforecasts, h=12)

zaraseriesforecasts2

plot(zaraseriesforecasts2)


# testing my model# 

#test 1 Coorrelogram-forecast errors test

acf(zaraseriesforecasts2$residuals, lag.max=20, na.action = na.omit)

#test 2 Ljung Box

Box.test(zaraseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# p-value = 0.6268    ( if p < 0,05)     (1-p  )


#test 3 forecast errors are normally distributed with mean zero
# and constant variance

plot.ts(zaraseriesforecasts2$residuals)




##define an R function "plotForecastErrors()"

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors, na.rm = TRUE)/4
  mysd   <- sd(forecasterrors, na.rm = TRUE)
  mymin  <- min(forecasterrors, na.rm = TRUE) - mysd*5
  mymax  <- max(forecasterrors, na.rm = TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm, na.rm = TRUE)
  mymax2 <- max(mynorm, na.rm = TRUE)
  if (mymin2 < mymin ) { mymin <- mymin2}
  if (mymax2 > mymax) { mymax <- mymax2}
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#plot a histogram (with overlaid normal curve) of the forecast errors for the rainfall predictions
plotForecastErrors(zaraseriesforecasts2$residuals)

#ARIMA model
#Plot  data as time series

require(forecast)
ARIMAfit = auto.arima(zaraseries)
summary(ARIMAfit)

zaratimeseriesforecasts <- forecast(ARIMAfit, h=12)
zaratimeseriesforecasts

plot(zaratimeseriesforecasts)

#Evaluation of the forecast errors of an ARIMA model 

# it is a good idea to investigate whether the
# forecast errors of an ARIMA model are normally
# distributed with mean zero and constant variance


acf(zaratimeseriesforecasts$residuals, lag.max=20)

Box.test(zaratimeseriesforecasts$residuals, lag=20, type="Ljung-Box")

zaratimeseriesforecasts$residuals


shapiro.test(zaratimeseriesforecasts$residuals)

plot.ts(zaratimeseriesforecasts$residuals)            # make time plot of forecast errors
plotForecastErrors(zaratimeseriesforecasts$residuals)
accuracy(zaratimeseriesforecasts)
accuracy(zaraseriesforecasts2)
