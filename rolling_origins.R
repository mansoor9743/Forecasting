NN5_12<-read.csv("NN5_12_replacedNA.csv", header=F,na.strings=("NA")) # data load

NROW(NN5_12)
NN5_12
is.na(NN5_12)

# using imputeTS library to impute last observed carry forward value to replace NA
NN5_12<-na.locf(NN5_12, option = "locf")
NN5_12<-ts(NN5_12, frequency = 7)
NN5_12
trendtest(NN5_12, extract = "TRUE")

# Parameter setting
origins <- 6    # We would like to predict for 6 rolling origins
ourHorizon <- 14
NumberofModel <- 1    # You can generalize this code for more models

#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(NumberofModel, origins, ourHorizon, ncol(NN5_12)))
ourHoldouts  <- array  (NA,c(origins, ourHorizon, ncol(NN5_12)))
# Double loop
for (i in 1:ncol(NN5_12)) {
  for (j in 1:origins )     {
    ourData <- NN5_12 [,i]
    ourData <- ts(ourData,frequency=7)
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    #ourModel<-Arima(ourtrainset,order=c(1,0,0), seasonal=list(order=c(0,1,2)))
    ourModel <- es(ourtrainset,"ANA", h= ourHorizon, silent=TRUE)
    #ourForecasts [NumberofModel,j,,i] <- ourModel$forecast }}
    ourForecasts [NumberofModel,j,,i] <- forecast(ourModel, h=ourHorizon)$mean }}

ourHoldouts
ourForecasts

#plot of all the foreacst and the test data with different origins
par(mfrow=c(2,3))
plot(ourForecasts[NumberofModel,1,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[1,,], col="blue", lwd=2)
legend("topleft",c("Rolling origin holdout test data","Rolling origin forecast from exponential smoothing (ANA) model -NN5_12"),fill=c("blue","red" ))
plot(ourForecasts[NumberofModel,2,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[2,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,3,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[3,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,4,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[4,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,5,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[5,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,6,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[6,,], col="blue", lwd=2)


#For improving the execution time of your coding, you can use sapply() function in R
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[NumberofModel,,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[NumberofModel,,,]))/2
zzznn <- colMeans(qq/bb)
(zzznn)
mean(zzznn)
#smape of each forecast of the 6 origins
SMAPE(ourHoldouts[1,,],ourForecasts[NumberofModel,1,,])#0.345, 0.384,0.388, 0.358, 0.418, 0.405
SMAPE(ourHoldouts[2,,],ourForecasts[NumberofModel,2,,])
SMAPE(ourHoldouts[3,,],ourForecasts[NumberofModel,3,,])
SMAPE(ourHoldouts[4,,],ourForecasts[NumberofModel,4,,])
SMAPE(ourHoldouts[5,,],ourForecasts[NumberofModel,5,,])
SMAPE(ourHoldouts[6,,],ourForecasts[NumberofModel,6,,])

# mean of smape errors of the foreacsts. its same as zzznn
SMAPE(ourHoldouts[,,],ourForecasts[NumberofModel,,,])

write.csv (zzznn,"zzznn.csv")
#find the zzznn in your working directory and analyze it

# arima rolling origin NN5_12
NN5_12<-read.csv("NN5_12_replacedNA.csv", header=F,na.strings=("NA")) # data load

NROW(NN5_12)
NN5_12
is.na(NN5_12)

# using imputeTS library to impute last observed carry forward value to replace NA
NN5_12<-na.locf(NN5_12, option = "locf")
NN5_12<-ts(NN5_12, frequency = 7)
NN5_12
trendtest(NN5_12, extract = "TRUE")

# Parameter setting
origins <- 6    # We would like to predict for 6 rolling origins
ourHorizon <- 14
NumberofModel <- 1    # You can generalize this code for more models

#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(NumberofModel, origins, ourHorizon, ncol(NN5_12)))
ourHoldouts  <- array  (NA,c(origins, ourHorizon, ncol(NN5_12)))
# Double loop
for (i in 1:ncol(NN5_12)) {
  for (j in 1:origins )     {
    ourData <- NN5_12 [,i]
    ourData <- ts(ourData,frequency=7)
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    ourModel<-Arima(ourtrainset,order=c(1,0,0), seasonal=list(order=c(0,1,2)))
    #ourModel <- es(ourtrainset,"ZZZ", h= ourHorizon, silent=TRUE)
    #ourForecasts [NumberofModel,j,,i] <- ourModel$forecast }}
    ourForecasts [NumberofModel,j,,i] <- forecast(ourModel, h=ourHorizon)$mean }}

ourHoldouts
ourForecasts

#plot of all the foreacst and the test data with different origins
par(mfrow=c(2,3))
plot(ourForecasts[NumberofModel,1,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[1,,], col="blue", lwd=2)
legend("topleft",c("Rolling origin holdout test data","Rolling origin forecast from ARIMA model"),fill=c("blue","red" ))
plot(ourForecasts[NumberofModel,2,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[2,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,3,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[3,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,4,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[4,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,5,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[5,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,6,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[6,,], col="blue", lwd=2)

#For improving the execution time of your coding, you can use sapply() function in R
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[NumberofModel,,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[NumberofModel,,,]))/2
zzznn <- colMeans(qq/bb)
(zzznn)
mean(zzznn)
#smape of each forecast of the 6 origins
SMAPE(ourHoldouts[1,,],ourForecasts[NumberofModel,1,,])
SMAPE(ourHoldouts[2,,],ourForecasts[NumberofModel,2,,])
SMAPE(ourHoldouts[3,,],ourForecasts[NumberofModel,3,,])
SMAPE(ourHoldouts[4,,],ourForecasts[NumberofModel,4,,])
SMAPE(ourHoldouts[5,,],ourForecasts[NumberofModel,5,,])
SMAPE(ourHoldouts[6,,],ourForecasts[NumberofModel,6,,])

# mean of smape errors of the foreacsts. its same as zzznn
SMAPE(ourHoldouts[,,],ourForecasts[NumberofModel,,,])

write.csv (zzznn,"zzznn.csv")
#find the zzznn in your working directory and analyze it

# regression rolling origin foreacast NN5_12

NN5_12<-read.csv("NN5_12_replacedNA.csv", header=F,na.strings=("NA")) # data load

NROW(NN5_12)
NN5_12
is.na(NN5_12)

# using imputeTS library to impute last observed carry forward value to replace NA
NN5_12<-na.locf(NN5_12, option = "locf")
NN5_12<-ts(NN5_12, frequency = 7)
NN5_12
trendtest(NN5_12, extract = "TRUE")

# Parameter setting
origins <- 6    # We would like to predict for 6 rolling origins
ourHorizon <- 14
NumberofModel <- 1    # You can generalize this code for more models

#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(NumberofModel, origins, ourHorizon, ncol(NN5_12)))
ourHoldouts  <- array  (NA,c(origins, ourHorizon, ncol(NN5_12)))
# Double loop
for (i in 1:ncol(NN5_12)) {
  for (j in 1:origins )     {
    ourData <- NN5_12 [,i]
    ourData <- ts(ourData,frequency=7)
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    ourModel<-fit1
    #ourModel <- es(ourtrainset,"ZZZ", h= ourHorizon, silent=TRUE)
    #ourForecasts [NumberofModel,j,,i] <- ourModel$forecast }}
    ourForecasts [NumberofModel,j,,i] <- predict(fit1, test_NN5_12) }}

ourHoldouts
ourForecasts

#plot of all the foreacst and the test data with different origins
par(mfrow=c(2,3))
plot(ourForecasts[NumberofModel,1,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[1,,], col="blue", lwd=2)
legend("topleft",c("Rolling origin holdout test data","Rolling origin forecast from Regression model for NN5_12"),fill=c("blue","red" ))
plot(ourForecasts[NumberofModel,2,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[2,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,3,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[3,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,4,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[4,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,5,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[5,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,6,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[6,,], col="blue", lwd=2)


#For improving the execution time of your coding, you can use sapply() function in R
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[NumberofModel,,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[NumberofModel,,,]))/2
zzznn <- colMeans(qq/bb)
(zzznn)
mean(zzznn)
#smape of each forecast of the 6 origins
SMAPE(ourHoldouts[1,,],ourForecasts[NumberofModel,1,,])
SMAPE(ourHoldouts[2,,],ourForecasts[NumberofModel,2,,])
SMAPE(ourHoldouts[3,,],ourForecasts[NumberofModel,3,,])
SMAPE(ourHoldouts[4,,],ourForecasts[NumberofModel,4,,])
SMAPE(ourHoldouts[5,,],ourForecasts[NumberofModel,5,,])
SMAPE(ourHoldouts[6,,],ourForecasts[NumberofModel,6,,])

# mean of smape errors of the foreacsts. its same as zzznn
SMAPE(ourHoldouts[,,],ourForecasts[NumberofModel,,,])

write.csv (zzznn,"zzznn.csv")
#find the zzznn in your working directory and analyze it



########################  NN5_32 #################################
NN5_32<-read.csv("NN5_32_NA.csv", header=F,na.strings=("NA")) # 



# using imputeTS library to impute last observed carry forward value to replace NA
NN5_32<-na.locf(NN5_32, option = "locf")
NN5_32<-ts(NN5_32, frequency = 7)


# Parameter setting
origins <- 6    # We would like to predict for 6 rolling origins
ourHorizon <- 14
NumberofModel <- 1    # You can generalize this code for more models

#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(NumberofModel, origins, ourHorizon, ncol(NN5_32)))
ourHoldouts  <- array  (NA,c(origins, ourHorizon, ncol(NN5_32)))
# Double loop
for (i in 1:ncol(NN5_32)) {
  for (j in 1:origins )     {
    ourData <- NN5_32 [,i]
    ourData <- ts(ourData,frequency=7)
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    #ourModel<-Arima(ourtrainset,order=c(1,0,0), seasonal=list(order=c(0,1,2)))
    ourModel <- es(ourtrainset,"AAdA", h= ourHorizon, silent=TRUE)
    #ourForecasts [NumberofModel,j,,i] <- ourModel$forecast }}
    ourForecasts [NumberofModel,j,,i] <- forecast(ourModel, h=ourHorizon)$mean }}

ourHoldouts
ourForecasts

#plot of all the foreacst and the test data with different origins
par(mfrow=c(2,3))
plot(ourForecasts[NumberofModel,1,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[1,,], col="blue", lwd=2)
legend("topleft",c("Rolling origin holdout test data","Rolling origin forecast from Holt - winters exponential smoothing (AAdA) model -NN5_32"),fill=c("blue","red" ))
plot(ourForecasts[NumberofModel,2,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[2,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,3,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[3,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,4,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[4,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,5,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[5,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,6,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[6,,], col="blue", lwd=2)

#For improving the execution time of your coding, you can use sapply() function in R
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[NumberofModel,,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[NumberofModel,,,]))/2
zzznn <- colMeans(qq/bb)
(zzznn)
mean(zzznn)
#smape of each forecast of the 6 origins
SMAPE(ourHoldouts[1,,],ourForecasts[NumberofModel,1,,])#0.345, 0.384,0.388, 0.358, 0.418, 0.405
SMAPE(ourHoldouts[2,,],ourForecasts[NumberofModel,2,,])
SMAPE(ourHoldouts[3,,],ourForecasts[NumberofModel,3,,])
SMAPE(ourHoldouts[4,,],ourForecasts[NumberofModel,4,,])
SMAPE(ourHoldouts[5,,],ourForecasts[NumberofModel,5,,])
SMAPE(ourHoldouts[6,,],ourForecasts[NumberofModel,6,,])
#0.139,0.132,0.142,0.129,0.128,0.126

# mean of smape errors of the foreacsts. its same as zzznn
SMAPE(ourHoldouts[,,],ourForecasts[NumberofModel,,,])

write.csv (zzznn,"zzznn.csv")
#find the zzznn in your working directory and analyze it
# arima nn5_32

NN5_32<-read.csv("NN5_32_NA.csv", header=F,na.strings=("NA")) # data load# data load

NROW(NN5_12)
NN5_12
is.na(NN5_12)

# using imputeTS library to impute last observed carry forward value to replace NA
NN5_32<-na.locf(NN5_32, option = "locf")
NN5_32<-ts(NN5_32, frequency = 7)
NN5_32
trendtest(NN5_32, extract = "TRUE")

# Parameter setting
origins <- 6    # We would like to predict for 6 rolling origins
ourHorizon <- 14
NumberofModel <- 1    # You can generalize this code for more models

#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(NumberofModel, origins, ourHorizon, ncol(NN5_32)))
ourHoldouts  <- array  (NA,c(origins, ourHorizon, ncol(NN5_32)))
# Double loop
for (i in 1:ncol(NN5_32)) {
  for (j in 1:origins )     {
    ourData <- NN5_32 [,i]
    ourData <- ts(ourData,frequency=7)
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    ourModel<-Arima(ourtrainset,order=c(1,0,0), seasonal=list(order=c(0,1,2)))
    #ourModel <- es(ourtrainset,"ZZZ", h= ourHorizon, silent=TRUE)
    #ourForecasts [NumberofModel,j,,i] <- ourModel$forecast }}
    ourForecasts [NumberofModel,j,,i] <- forecast(ourModel, h=ourHorizon)$mean }}

ourHoldouts
ourForecasts

#plot of all the foreacst and the test data with different origins
par(mfrow=c(2,3))
plot(ourForecasts[NumberofModel,1,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[1,,], col="blue", lwd=2)
legend("topleft",c("Rolling origin holdout test data","Rolling origin forecast from ARIMA model"),fill=c("blue","red" ))
plot(ourForecasts[NumberofModel,2,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[2,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,3,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[3,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,4,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[4,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,5,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[5,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,6,,], type = "l", col="red",lwd=2, xlab = "Time in days")
lines(ourHoldouts[6,,], col="blue", lwd=2)

#For improving the execution time of your coding, you can use sapply() function in R
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[NumberofModel,,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[NumberofModel,,,]))/2
zzznn <- colMeans(qq/bb)
(zzznn)
mean(zzznn)
#smape of each forecast of the 6 origins
SMAPE(ourHoldouts[1,,],ourForecasts[NumberofModel,1,,])
SMAPE(ourHoldouts[2,,],ourForecasts[NumberofModel,2,,])
SMAPE(ourHoldouts[3,,],ourForecasts[NumberofModel,3,,])
SMAPE(ourHoldouts[4,,],ourForecasts[NumberofModel,4,,])
SMAPE(ourHoldouts[5,,],ourForecasts[NumberofModel,5,,])
SMAPE(ourHoldouts[6,,],ourForecasts[NumberofModel,6,,])

# mean of smape errors of the foreacsts. its same as zzznn
SMAPE(ourHoldouts[,,],ourForecasts[NumberofModel,,,])

write.csv (zzznn,"zzznn.csv")
#find the zzznn in your working directory and analyze it
# regression rolling origin foreacast NN5_32

NN5_32<-read.csv("NN5_32_NA.csv", header=F,na.strings=("NA")) # data load# data load

NROW(NN5_32)
NN5_12
is.na(NN5_12)

# using imputeTS library to impute last observed carry forward value to replace NA
NN5_32<-na.locf(NN5_32, option = "locf")
NN5_32<-ts(NN5_32, frequency = 7)
NN5_32
trendtest(NN5_32, extract = "TRUE")

# Parameter setting
origins <- 6    # We would like to predict for 6 rolling origins
ourHorizon <- 14
NumberofModel <- 1    # You can generalize this code for more models

#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(NumberofModel, origins, ourHorizon, ncol(NN5_32)))
ourHoldouts  <- array  (NA,c(origins, ourHorizon, ncol(NN5_32)))
# Double loop
for (i in 1:ncol(NN5_32)) {
  for (j in 1:origins )     {
    ourData <- NN5_32 [,i]
    ourData <- ts(ourData,frequency=7)
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    ourModel<-fit1
    #ourModel <- es(ourtrainset,"ZZZ", h= ourHorizon, silent=TRUE)
    #ourForecasts [NumberofModel,j,,i] <- ourModel$forecast }}
    ourForecasts [NumberofModel,j,,i] <- predict(fit1, test_NN5_32) }}

ourHoldouts
ourForecasts

#plot of all the foreacst and the test data with different origins
par(mfrow=c(2,3))
plot(ourForecasts[NumberofModel,1,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[1,,], col="blue", lwd=2)
legend("topleft",c("Rolling origin holdout test data","Rolling origin forecast from Regression model for NN5_32"),fill=c("blue","red" ))
plot(ourForecasts[NumberofModel,2,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[2,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,3,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[3,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,4,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[4,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,5,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[5,,], col="blue", lwd=2)
plot(ourForecasts[NumberofModel,6,,], type = "l", col="red",lwd=2,xlab = "Time in days")
lines(ourHoldouts[6,,], col="blue", lwd=2)

#For improving the execution time of your coding, you can use sapply() function in R
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[NumberofModel,,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[NumberofModel,,,]))/2
zzznn <- colMeans(qq/bb)
(zzznn)
mean(zzznn)
#smape of each forecast of the 6 origins
SMAPE(ourHoldouts[1,,],ourForecasts[NumberofModel,1,,])
SMAPE(ourHoldouts[2,,],ourForecasts[NumberofModel,2,,])
SMAPE(ourHoldouts[3,,],ourForecasts[NumberofModel,3,,])
SMAPE(ourHoldouts[4,,],ourForecasts[NumberofModel,4,,])
SMAPE(ourHoldouts[5,,],ourForecasts[NumberofModel,5,,])
SMAPE(ourHoldouts[6,,],ourForecasts[NumberofModel,6,,])

# mean of smape errors of the foreacsts. its same as zzznn
SMAPE(ourHoldouts[,,],ourForecasts[NumberofModel,,,])

write.csv (zzznn,"zzznn.csv")
#find the zzznn in your working directory and analyze it






















