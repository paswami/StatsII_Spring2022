library('ggplot2')
library('forecast')
library('tseries')

###Google trends searches for 'depression'
depressionUS <- c(87,96,99,100,87,64,61,60,80,85,90,78,78,91,90,95,91,63,61,56,69,82,86,75,80,83,87,81,76,54,52,50,62,69,72,61,65,71,73,73,69,52,46,46,58,66,66,57,63,68,69,71,63,49,47,43,73,86,77,69,66,80,79,71,60,50,49,47,55,60,67,59,60,63,68,66,59,47,45,43,51,55,57,50,60,65,66,67,63,51,49,50,57,59,61,54,59,65,64,65,60,48,46,48,53,58,59,54,59,63,64,67,59,48,46,45,53,60,60,55,58,65,65,67,60,50,46,63,60,64,65,59,63,66,72,71,65,54,55,55,64,70,70,66,65,75,75,78,69,55,50,56,63,69,69,66,72,75,78,76,72,56,57,61,64,73,74,68,71,78,77,79,72,68,60,59,68,76,78,72,77,82,84,79,73,60,59,58,64)

# changing the dataset into a timeseries

depressionUS.timeseries <- ts(depressionUS,start = c(2004,1),frequency = 12)
depressionUS.timeseries

## 80/20 split data train test 
h2US <- 38L
trainUS <- head(depressionUS.timeseries, round(length(depressionUS.timeseries) - h2US))
testUS <- tail(depressionUS.timeseries, h2US)

trainDataUS <- trainUS
testDataUS <- testUS
arimaModUS <- auto.arima(trainDataUS, stepwise=FALSE, approximation=FALSE)
arimaMod.FrUS <-forecast(arimaModUS,h=38)
plot(arimaMod.FrUS, main ="US Forecast")
lines(testDataUS, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

accuracy(arimaMod.FrUS, testDataUS)


### auto.arima forecast for future based on training set only
autoarimaTRAINUS <- auto.arima(trainUS)
forecastTRAINUS <- forecast(autoarimaTRAINUS, h=62)

### point forecasts for training set based forecast
forecastTRAINUS

#### plot training set, test set, and forecast based on training set into future

h2US <- 38L
trainUS <- head(depressionUS.timeseries, round(length(depressionUS.timeseries) - h2US))
testUS <- tail(depressionUS.timeseries, h2US)

trainDataUS <- trainUS
testDataUS <- testUS
arimaModUS <- auto.arima(trainDataUS, stepwise=FALSE, approximation=FALSE)
arimaMod.FrUS <-forecast(arimaModUS,h=62)
plot(arimaMod.FrUS, main= "US Forecast")
lines(testDataUS, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

### auto.arima forecast based on full dataset
autoarimaFULLUS <- auto.arima(depressionUS.timeseries)
forecastFULLUS <- forecast(autoarimaFULLUS, h = 24)
forecastFULLUS
plot(forecastFULLUS, main = "non-trained US Forecast")
summary(autoarimaFULLUS)
accuracy(autoarimaFULLUS)

# decompose into three components 
us.decom = ts(depressionUS.timeseries, frequency = 12)
stl_us = stl(us.decom, "periodic")
seasonal_stl_us   <- stl_us$time.series[,1]
trend_stl_us     <- stl_us$time.series[,2]
random_stl_us  <- stl_us$time.series[,3]

# display main time series and the individual components 
plot(us.decom)
plot(as.ts(seasonal_stl_us)) 
plot(trend_stl_us) 
plot(random_stl_us) 
plot(stl_us, main = "Indiviual Components of US Time Series")

