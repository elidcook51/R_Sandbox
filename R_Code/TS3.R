#************************************************************
#
#      	Session 19
#			Time Series Analysis 3
#
#************************************************************


#*****************************
#
# Load the data & source files
#
sourcedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"
datadir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/Spam"

library(mtsdi)
library(forecast)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggfortify)
library(ggpubr)
library(tseries)

##Load the Virginia weather data
setwd(datadir)
VAweather <- read.table('VirginiaWeatherData.csv',header=T,sep=',')
setwd(sourcedir)

#Replace -999s with NAs
VAweather[VAweather == -999.0] <- NA
VAweather <- VAweather[1:(dim(VAweather)[1]-7),]

# create date column for plotting
date <- VAweather %>% select(Year, Month) %>% mutate(date = make_datetime(Year,Month))
VAweather$date <- date$date

# Impute NAs
f <- ~ C_Precip + C_Tmax + C_Tmin + R_Precip + R_Tmax + R_Tmin
t <- c(seq(1,dim(VAweather)[1]))
imputedData <- mnimput(f, VAweather[,3:8], eps=1e-3, ts=TRUE, method="gam", 
                       ga.control=list(formula=paste(names(VAweather[,3:8]),'~ns(t,6)')))
VAweather[,3:8] <- imputedData$filled.dataset

# Create time series of monthly Richmond precipitation and minimum temperature
Richmond.P <- VAweather$R_Precip
Richmond.Tmin <- VAweather$R_Tmin

##Use the ts() command to get a time series of Richmond.P
precip.ts<-ts(Richmond.P)

##Use the ts() command to get a time series of Richmond.Tmin
temp.ts<-ts(Richmond.Tmin)

#*****************************
# 
# Temp Linear Models
# Trend and Seasonality  
# 
#*****************************
# Create a new variable time.precip which contains the index of the months 
# minus the last 6 which we will use as a test set

time.temp <- c(1:(length(temp.ts)-6))

# Get a linear model of precip vs. the index of the month and the seasonality

temp.trend.seasonal<-lm(temp.ts[time.temp]~time.temp + sin(2*pi*time.temp/12) + cos(2*pi*time.temp/12))

# Use the summary() command on temp.trend.seasonal,
# Is time significant in predicting temperature? How about seasonality?

summary(temp.trend.seasonal)

# Plot the true vs. fitted time series
ggplot(VAweather[1:(length(temp.ts)-6),], aes(x=date,y=R_Tmin)) + geom_line() + 
  geom_line(aes(x=date,y=temp.trend.seasonal$fitted.values),color="red") +
  xlab("") + ylab("Richmond Tmin")


#*****************************
# 
# AR, MA & ARIMA Models  
# 
#***************************** 

#Get the residuals from the temp.trend.seasonal model above and store in e.ts:

e.ts.temp<-ts(temp.trend.seasonal$residuals)

##Plot the residuals for the temp.trend.seasonal model

autoplot(e.ts.temp, ylab = "Residuals from Temperature Model")



# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals of temp.trend.seasonal
temp.acf <- ggAcf(e.ts.temp)
temp.pacf <- ggPacf(e.ts.temp)
ggarrange(temp.acf,temp.pacf,nrow=2,ncol=1)

# Does this time series still have structure to be modeled?

##Yes


# Do we need to consider a first order difference of our residuals?
temp.diff.acf <- ggAcf(diff(e.ts.temp))
temp.diff.pacf <- ggPacf(diff(e.ts.temp))
ggarrange(temp.diff.acf,temp.diff.pacf,nrow=2,ncol=1)

# No!


#*****************************
# 
# AR, MA & ARIMA Models  
# 
#*****************************

# Let us choose p and q terms for e.ts.temp based on the acf and pacf 

# Choose p and q terms for e.ts.temp based on the acf and pacf 

# ar(1) p=1
temp.ar1 <- arima(e.ts.temp, order=c(1,0,0), include.mean=FALSE)
summary(temp.ar1)

# ma(2) p=0, q=2
temp.ma2 <- arima(e.ts.temp, order=c(0,0,2), include.mean=FALSE)
summary(temp.ma2)

# arma(1,2) p=1, q=2
temp.arma12 <- arima(e.ts.temp, order=c(1,0,2), include.mean=FALSE)
summary(temp.arma12)

# Based on AIC, which one do you choose?
temp.auto <- auto.arima(e.ts.temp,approximation=FALSE)
summary(temp.auto)

#What if we use BIC and diagnostics?

# BIC
BIC(temp.ar1)
BIC(temp.ma2)
BIC(temp.arma12)
BIC(temp.auto)

# assess residuals vs. fitted
model1 = ggplot() + geom_point(aes(x=fitted(temp.ar1), y=temp.ar1$residuals)) + ggtitle("AR1")
model2 = ggplot() + geom_point(aes(x=fitted(temp.ma2), y=temp.ma2$residuals)) + ggtitle("MA2")
model3 = ggplot() + geom_point(aes(x=fitted(temp.arma12), y=temp.arma12$residuals)) + ggtitle("ARMA12")
model4 = ggplot() + geom_point(aes(x=fitted(temp.auto), y=temp.auto$residuals)) + ggtitle("Auto")

ggarrange(model1, model2, model3, model4, ncol=2, nrow=2)

# assess normality of residuals
model1 = qplot(sample=temp.ar1$residuals) + stat_qq_line(color="red") + ggtitle("AR1")
model2 = qplot(sample=temp.ma2$residuals) + stat_qq_line(color="red") + ggtitle("MA2")
model3 = qplot(sample=temp.arma12$residuals) + stat_qq_line(color="red") + ggtitle("ARMA12")
model4 = qplot(sample=temp.auto$residuals) + stat_qq_line(color="red") + ggtitle("Auto")

ggarrange(model1, model2, model3, model4, ncol=2, nrow=2)

# Plot diagnostics for independence of residuals using tsdiag()
tsdiag(temp.ar1,gof.lag=20)

# with ggplot
ggtsdiag(temp.ar1,gof.lag=20)

ggtsdiag(temp.ma2,gof.lag=20)

ggtsdiag(temp.arma12,gof.lag=20)

ggtsdiag(temp.auto,gof.lag=20)


# Which diagnostics plots are adequate?  Which are not?


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals of temp.auto
temp.auto.resid.acf <- ggAcf(temp.auto$residuals)
temp.auto.resid.pacf <- ggPacf(temp.auto$residuals)
ggarrange(temp.auto.resid.acf,temp.auto.resid.pacf,nrow=2,ncol=1)

# Does the temp.auto model account for correlation in the residuals?

# Yes, mostly


# Plot the best model's fitted values vs. true values
# Since the best AR model has p=1, we start predicting at t=2
Tmin.fitted <- temp.trend.seasonal$fitted.values + fitted(temp.auto)

ggplot() + geom_line(aes(x=time.temp,y=temp.ts[1:length(time.temp)],color="True")) +
  geom_line(aes(x=time.temp,y=Tmin.fitted,color="Fitted")) + xlab("Time") + 
  ylab("Richmond Tmin")


# Forecast with the best model:

# Use the forecast option to forecast the next 6 months of temperature residuals - temp.auto.forecast
temp.auto.forecast <- forecast(temp.auto, h=6)

plot(temp.auto.forecast)

# with ggplot
autoplot(temp.auto.forecast,main="Forecasts from ARIMA(2,0,1) with zero mean")

# Prediction performance
# Create test set from temp data set with last 6 months

# The test period in months

next.6mo.time <- c((length(temp.ts)-5):(length(temp.ts)))

# The test data frame

next.6mo <- data.frame(time.temp = next.6mo.time, temp = temp.ts[next.6mo.time])

# Prediction for the next 6 months by temp.auto:

E_Y.pred <- predict(temp.trend.seasonal, newdata=next.6mo)
e_t.pred <- forecast(temp.auto, h=6)
next.6mo.prediction <- E_Y.pred + e_t.pred$mean

# MSE:

mean((next.6mo.prediction-next.6mo$temp)^2)

# Plot actual values and predicted values
plot(ts(next.6mo$temp),type='o',ylim=c(25,60))
lines(ts(next.6mo.prediction),col='red',type='o')
lines(1:6, E_Y.pred + e_t.pred$lower[,2], col = "red", lty = "dashed")
lines(1:6, E_Y.pred + e_t.pred$upper[,2], col = "red", lty = "dashed")
legend(1,60, legend = c("Actual", "Predicted"), lwd = 2, col = c("black", "red")) 

# with ggplot
time.predictions <- VAweather$date[(length(time.temp)+1):(length(time.temp)+6)]
model1.predictions <- ggplot() + 
  geom_line(aes(x=time.predictions,y=next.6mo$temp),color="black") + 
  geom_line(aes(x=time.predictions,y=next.6mo.prediction),color="red") + 
  geom_line(aes(x=time.predictions,y=E_Y.pred + e_t.pred$lower[,2]),
            color="red",linetype="dashed") + 
  geom_line(aes(x=time.predictions,y=E_Y.pred + e_t.pred$upper[,2]),
            color="red",linetype="dashed") +
  xlab("") + ylab("Richmond Tmin") + 
  ggtitle("Temp Trend + Seasonal Model + ARIMA of Residuals")
model1.predictions


# Simulate 50 years of monthly minimum temperatures with the best model
# auto.arima builds ARMA(2,1) model
set.seed(1)
temp.auto.sim <- arima.sim(n=12*50, list(ar=c(temp.auto$coef[1],temp.auto$coef[2]),
                                      ma=c(temp.auto$coef[3])),
                        sd=sqrt(temp.auto$sigma2))

# Add mean predictions and plot simulation of Tmin
time.50yr <- c(1:(50*12))
time.50yr.df <- data.frame(time.temp = time.50yr)

temp.50.yr.predictions <- predict(temp.trend.seasonal, newdata=time.50yr.df)

# plot simulated temperatures
autoplot(ts(temp.50.yr.predictions + temp.auto.sim),xlab="Time",ylab="Simulated Richmond Tmin")

#************************************************
#
# Repeat the process for precip
#
#************************************************

#*****************************
# 
# Model Trend and Seasonality  
# 
#*****************************

#Plot precip.ts and acf and pacf of precip.ts


# Create a new variable for the index of months in the precip series and hold out the last 6 months


# Model the trend (if applicable) and seasonality of precip
# Call the model precip.lm


# do we need to transform precip?
# if so, modify precip.lm accordingly


#*****************************
# 
# AR, MA & ARIMA Models  
# 
#***************************** 

#get the residuals from the precip.lm model above and store in e.ts:


##plot the residuals for the precip.lm model


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals of precip.lm


# Does this time series still have structure to be modeled?


# Do we need to consider a first order difference of our residuals?


# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms


# Diagnostics



# Which diagnostics plots are adequate?  Which are not?


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals of precip.auto


# What if we used the residuals from temp.trend.seasonal to predict the residuals from precip.lm? 
# Build this model and call it precip.temp.lm



# Forecasting:

# Use the forecast option to forecast the next 6 months of precip data- precip.auto.forecast



# Prediction performance
# Create test set from precip data set with last 6 months


# Prediction for the next 6 months by precip.lm and precip.auto:


# MSE:


# Plot actual values and predicted values and confidence intervals

# Repeat with precip.temp.lm
# First forecast temperature residuals, then use those forecasts to forecast precipitation residuals
# and add the forecast precipitation residuals to the forecast precipitation mean from precip.lm
