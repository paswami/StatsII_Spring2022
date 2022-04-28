###Google trends searches for 'depression'
depressionAustralia <- c(51,75,95,79,96,79,83,92,92,91,83,56,47,71,84,76,100,78,66,83,76,87,68,39,61,71,78,69,85,56,56,70,63,61,61,38,37,52,61,53,72,55,47,62,59,55,51,34,49,49,56,52,62,51,41,53,63,71,58,37,43,52,60,57,62,53,48,60,59,56,54,36,39,49,57,54,64,49,44,60,55,54,52,35,35,44,53,48,61,48,42,56,54,54,49,39,41,45,52,50,56,49,49,56,56,55,48,36,36,50,53,54,61,52,48,57,54,53,49,38,40,55,57,53,58,53,47,67,57,57,53,40,42,49,61,54,61,54,50,58,58,57,51,40,43,53,54,53,60,49,45,52,54,53,49,37,39,49,57,53,57,50,47,57,51,52,46,39,39,49,53,48,54,53,45,51,52,51,48,37,37,46,52,46,50,46,43,49,56)

depressionAustralia.timeseries <- ts(depressionAustralia,start = c(2004,1),frequency = 12)
depressionAustralia.timeseries

## 80/20 split data train test 
h2 <- 38L
train <- head(depressionAustralia.timeseries, round(length(depressionAustralia.timeseries) - h2))
test <- tail(depressionAustralia.timeseries, h2)

trainData <- train
testData <- test
arimaMod <- auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
arimaMod.Fr <-forecast(arimaMod,h=38)
plot(arimaMod.Fr, main = "Australian Forecast")
lines(testData, col="red")
legend("bottomleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

accuracy(arimaMod.Fr, testData)


### auto.arima forecast for future based on training set only
autoarimaTRAIN <- auto.arima(train)
forecastTRAIN <- forecast(autoarimaTRAIN, h=62)

### point forecasts for training set based forecast
forecastTRAIN

#### plot training set, test set, and forecast based on training set into future

h2 <- 38L
train <- head(depressionAustralia.timeseries, round(length(depressionAustralia.timeseries) - h2))
test <- tail(depressionAustralia.timeseries, h2)

trainData <- train
testData <- test
arimaMod <- auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
arimaMod.Fr <-forecast(arimaMod,h=62)
plot(arimaMod.Fr, main = "Australian Forecast")
lines(testData, col="red")
legend("bottomleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

### auto.arima forecast based on full dataset
autoarimaFULL <- auto.arima(depressionAustralia.timeseries)
forecastFULL <- forecast(autoarimaFULL, h = 24)
forecastFULL
plot(forecastFULL)
summary(autoarimaFULL)
accuracy(autoarimaFULL)

# decompose into three components 
aus.decom = ts(depressionAustralia.timeseries, frequency = 12)
stl_aus = stl(aus.decom, "periodic")
seasonal_stl_aus   <- stl_aus$time.series[,1]
trend_stl_aus     <- stl_aus$time.series[,2]
random_stl_aus  <- stl_aus$time.series[,3]

# display main time series and the individual components 
plot(aus.decom)
plot(as.ts(seasonal_stl_aus)) 
plot(trend_stl_aus) 
plot(random_stl_aus) 
plot(stl_aus, main = "Indiviual Components of Australian Time Series")
