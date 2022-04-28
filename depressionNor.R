norway <- read.csv("norway.csv")

# changing the dataset into a timeseries

norway.ts <- ts(norway, start = c(2004,1),frequency = 12)
norway.ts

## 80/20 split data train test 
h2N <- 38L
trainNorway <- head(norway.ts, round(length(norway.ts) - h2N))
testNorway <- tail(norway.ts, h2N)

trainDataN <- trainNorway
testDataN <- testNorway
arimaModN <- auto.arima(trainDataN, stepwise=FALSE, approximation=FALSE)
arimaMod.FN <-forecast(arimaModN,h=38)
plot(arimaMod.FN)
lines(testDataN, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

accuracy(arimaMod.FN, testDataN)


### auto.arima forecast for future based on training set only
autoarimaTRAINn <- auto.arima(trainNorway)
forecastTRAINn <- forecast(autoarimaTRAINn, h=62)

### point forecasts for training set based forecast
forecastTRAINn

#### plot training set, test set, and forecast based on training set into future

h2US <- 38L
trainNorway <- head(norway.ts, round(length(norway.ts) - h2N))
testNorway <- tail(norway.ts, h2US)

trainDataN <- trainNorway
testDataN <- testNorway
arimaModN <- auto.arima(trainDataN, stepwise=FALSE, approximation=FALSE)
arimaMod.FN <-forecast(arimaModN,h=62)
plot(arimaMod.FN, main = "Norway Forecast")
lines(testDataN, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

### auto.arima forecast based on full dataset
autoarimaFULLN <- auto.arima(norway.ts)
forecastFULLN <- forecast(autoarimaFULLN, h = 24)
forecastFULLN
plot(forecastFULLN)
summary(autoarimaFULLN)
accuracy(autoarimaFULLN)

# decompose into three components 
nor.decom = ts(norway.ts, frequency = 12)
stl_nor = stl(nor.decom, "periodic")
seasonal_stl_nor   <- stl_nor$time.series[,1]
trend_stl_nor     <- stl_nor$time.series[,2]
random_stl_nor  <- stl_nor$time.series[,3]

# display main time series and the individual components 
plot(nor.decom)
plot(as.ts(seasonal_stl_nor)) 
plot(trend_stl_nor) 
plot(random_stl_nor) 
plot(stl_nor)
