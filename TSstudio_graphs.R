set.seed(1234)
library(TSstudio)
NN5_32<-read.csv("NN5_32_NA.csv", header=F,na.strings=("NA"))
NN5_12<-read.csv("NN5_12_replacedNA.csv", header=F,na.strings=("NA"))

NN5_12<-na.locf(NN5_12, option = "locf")
NN5_12
# Create the training set
train_NN5_12 <- ts(NN5_12[1:721,],frequency=7) # converting into time series as well
train_NN5_12
# Create the test set
test_NN5_12 <- ts(NN5_12[722:735,],frequency=7, start = c(104,1))
test_NN5_12
NN5_12<-ts(NN5_12, frequency = 7)
NN5_32<-ts(NN5_32, frequency = 7)
NN5_12

NN5_32<-na.locf(NN5_32, option = "locf")
NN5_32
# Create the training set
train_NN5_32 <- ts(NN5_32[1:721,],frequency=7) # converting into time series as well
train_NN5_32
# Create the test set
test_NN5_32 <- ts(NN5_32[722:735,],frequency=7, start = c(104,1))
test_NN5_32
NN5_32<-ts(NN5_32, frequency = 7)

ts_plot(NN5_12)
x11()
ts_seasonal(NN5_12, type = "all")
ts_lags(NN5_12, lags = 1:7)

# Plotting actual vs. fitted and forecasted
test_forecast(actual = NN5_12, forecast.obj = forecast_es, test = test_NN5_12)

# Forecasting with backtesting 
NN5_12_backtesting <- ts_backtesting(NN5_12, 
                                    models = "abehntw", 
                                    periods = 6, 
                                    error = "sMAPE", 
                                    window_size = 7, 
                                    h = 14)
NN5_12_backtesting$MAPE_score
NN5_12_backtesting$leaderboard
NN5_12_backtesting$

NN5_32_backtesting <- ts_backtesting(NN5_32, 
                                     models = "abehntw", 
                                     periods = 6, 
                                     error = "sMAPE", 
                                     window_size = 7, 
                                     h = 14)

NN5_32_backtesting
NN5_32_backtesting$MAPE_score
NN5_32_backtesting$leaderboard
