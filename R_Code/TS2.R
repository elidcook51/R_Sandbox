#************************************************************
#
#      	Session 18 
#			Time Series Analysis 2
#
#************************************************************


#*****************************
#
# Load the data & source files
#
#*****************************
sourcedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"
datadir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/VAweather"

##Load the Virginia weather data
setwd(datadir)
VAweather <- read.table('VirginiaWeatherData.csv',header=T,sep=',')
setwd(sourcedir)

# Create time series of monthly Richmond precipitation and minimum temperature
Richmond.P <- VAweather$R_Precip
Richmond.Tmin <- VAweather$R_Tmin

##Use the ts() command to get a time series of Richmond.P
precip.ts<-ts(Richmond.P)

##Use the ts() command to get a time series of Richmond.Tmin
temp.ts<-ts(Richmond.Tmin)

##Load libraries (install if necessary)
library(forecast)
library(ggplot2)
library(mtsdi)
library(tidyverse)
library(lubridate)
library("car")
library(ggfortify)
library(ggpubr)
library(tseries)

date <- VAweather %>% select(Year, Month) %>% mutate(date = make_datetime(Year,Month))
VAweather$date <- date$date

#*****************************
# Observations
#*****************************

# Plot the time series you created for Richmond precip, precip.ts.
autoplot(precip.ts)

# Plot the time series you created for Richmond Tmin, temp.ts.
autoplot(temp.ts)


# What's wrong?

# Replace -999s with NA, remove last 7 rows, and re-make time series
VAweather[VAweather == -999.0] <- NA
VAweather <- VAweather[1:(dim(VAweather)[1]-7),]

Richmond.P <- VAweather$R_Precip
Richmond.Tmin <- VAweather$R_Tmin

precip.ts<-ts(Richmond.P)
temp.ts<-ts(Richmond.Tmin)

# Re-plot the time series
autoplot(precip.ts)
autoplot(temp.ts)


# Impute missing values
f <- ~ C_Precip + C_Tmax + C_Tmin + R_Precip + R_Tmax + R_Tmin
t <- c(seq(1,dim(VAweather)[1]))
imputedData <- mnimput(f, VAweather[,3:8], eps=1e-3, ts=TRUE, method="gam", 
                       ga.control=list(formula=paste(names(VAweather[,3:8]),'~ns(t,6)')))

# Replace VAweather with imputed data
VAweather[,3:8] <- imputedData$filled.dataset

Richmond.P <- VAweather$R_Precip
Richmond.Tmin <- VAweather$R_Tmin

precip.ts<-ts(Richmond.P)
temp.ts<-ts(Richmond.Tmin)



Boxplot(precip.ts)

ggplot() + geom_boxplot(aes(y=Richmond.P))


#*****************************
# Seasonality
#*****************************

# Get the periodogram for precip.ts
pg.precip <- spec.pgram(precip.ts,spans=9,demean=T,log='no')

spec.precip <- data.frame(freq=pg.precip$freq, spec=pg.precip$spec)
ggplot(spec.precip) + geom_line(aes(x=freq,y=spec)) + 
  ggtitle("Smooth Periodogram of Richmond Precip")

# Find the peak, max.omega.precip
max.omega.precip<-pg.precip$freq[which(pg.precip$spec==max(pg.precip$spec))]

# Where is the peak?
max.omega.precip

# What is the period?
1/max.omega.precip

# What are the periods of the next biggest peaks?
# sort spectrum from largest to smallest and find index
sorted.spec <- sort(pg.precip$spec, decreasing=T, index.return=T)
names(sorted.spec)

# corresponding periods (omegas = frequencies, Ts = periods)
sorted.omegas <- pg.precip$freq[sorted.spec$ix]
sorted.Ts <- 1/pg.precip$freq[sorted.spec$ix]

# look at first 20
sorted.omegas[1:20]
sorted.Ts[1:20]

# Repeat for temp.ts
pg.temp <- spec.pgram(temp.ts,spans=9,demean=T,log='no')

max.omega.temp<-pg.temp$freq[which(pg.temp$spec==max(pg.temp$spec))]

# Where is the peak?
max.omega.temp

# What is the period?
1/max.omega.temp

#*****************************
# 
# Model Trend and Seasonality  
# 
#*****************************

# Build a new model, temp.trend which predicts temp.ts based on the time variable
temp.trend<-lm(temp.ts ~ t)

# Use the summary() command on temp.trend,
# Is time significant in predicting Richmond minimum temperature?
summary(temp.trend)

# Plot temp.trend model
ggplot(VAweather, aes(x=date,y=R_Tmin)) + geom_line() +
  stat_smooth(method="lm",col="red") + xlab("") + ylab("Richmond Tmin")

# Model diagnostics for temp.trend
autoplot(temp.trend, labels.id = NULL)
    
# Are there any issue with the diagnostic plots for the temp.trend model?

# Model seasonality
temp.trend.seasonal <- lm(NO2.ts ~ t + sin(2*pi*t/12) + cos(2*pi*t/12))
summary(temp.trend.seasonal)
    
# Plot temp.trend.seasonal model
ggplot(VAweather, aes(x=date,y=R_Tmin)) + geom_line() + 
  geom_line(aes(x=date,y=temp.trend.seasonal$fitted.values),color="red") +
  xlab("") + ylab("Richmond Tmin")

# Model diagnostics for temp.trend.seasonal
autoplot(temp.trend.seasonal, labels.id = NULL)

# Are there any issue with the diagnostic plots for the temp.trend.seasonal model?
    
#*****************************
# 
# AR, MA & ARIMA Models  
# 
#*****************************
    
# Get the residuals from the temp.trend.seasonal model above and store in e.ts.temp:
e.ts.temp <- ts(temp.trend.seasonal$residuals)
    
# Plot the residuals for the temp.trend model
autoplot(e.ts.temp)
    
# Plot the autocorrelation (ACF) of the residuals of temp.trend.seasonal
temp.acf <- ggAcf(e.ts.temp)
temp.acf

# Plot the partial autocorrelation (PACF) of the residuals temp.trend.seasonal
temp.pacf <- ggPacf(e.ts.temp)
temp.pacf  

# Plot acf and pacf side by side for easier examination
ggarrange(temp.acf,temp.pacf,nrow=2,ncol=1)


# Does this time series still have structure to be modeled?

# Choose p and q terms for e.ts.temp based on the acf and pacf 

# ar(1) p=1
temp.ar1 <- arma(e.ts.temp, order=c(1,0))
summary(temp.ar1)

temp.ar1 <- arma(e.ts.temp, order=c(1,0), include.intercept=FALSE)
summary(temp.ar1)

    
# ma(2) p=0, q=2
temp.ma2 <- arma(e.ts.temp, order=c(0,2))
summary(temp.ma2)

temp.ma2 <- arma(e.ts.temp, order=c(0,2), include.intercept=FALSE)
summary(temp.ma2)


# arma(1,2) p=1, q=2
temp.arma12 <- arma(e.ts.temp, order=c(1,2))
summary(temp.arma12)

temp.arma12 <- arma(e.ts.temp, order=c(1,2), include.intercept=FALSE)
summary(temp.arma12)

Tmin.pred <- temp.trend.seasonal$fitted.values + temp.arma12$fitted.values

Tmin.pred.ts <- ts(Tmin.pred)

autoplot(temp.ts)
lines(Tmin.pred.ts, col = 'red')
# To automatically select models based on AIC,
# we will use the auto.arima() function, 
# which we will learn about next class

    
