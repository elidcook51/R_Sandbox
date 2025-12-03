#*****************************
sourcedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"
datadir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/Spam"

##load the 'ham_ts.csv' and 'spam_ts.csv' into ham and spam variables respectively
setwd(datadir)
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')
setwd(sourcedir)

##summarize the new ham data set, what do you notice?  what are the variables, ranges?
summary(ham)

##summarize the new spam data set, what do you notice?  what are the variables, ranges?
summary(spam)

##use the ts() command to get a time series of ham amount using ham data from all years
ham.ts<-ts(ham$count)

##use the ts() command to get a time series of spam amount using spam data from all years
spam.ts<-ts(spam$count)

##load libraries (install them if necessary)
library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)

ham.ts.short <-ts(ham$count[1:464])
time.ham<-c(1:length(ham.ts.short))
ham.trend<-lm(ham.ts.short~time.ham)
summary(ham.trend)

acf(ham.ts.short)

pg.precip <- spec.pgram(ham.ts.short,spans=9,demean=T,log='no')
spec.precip <- data.frame(freq=pg.precip$freq, spec=pg.precip$spec)

max.omega.precip<-pg.precip$freq[which(pg.precip$spec==max(pg.precip$spec))]
1/max.omega.precip

ham.day <- time.ham %% 7
ham.day <-as.factor(time.ham %% 7) 

Day <- rep(NA, length(ham.ts.short))
Day[which((time.ham %% 7)    == 1)] <- "Th"  
Day[which((time.ham %% 7)    == 2)] <- "F"
Day[which((time.ham %% 7)    == 3)] <- "Sa"
Day[which((time.ham %% 7)    == 4)] <- "S"
Day[which((time.ham %% 7)    == 5)] <- "M"
Day[which((time.ham %% 7)    == 6)] <- "T"
Day[which((time.ham %% 7)    == 0)] <- "W"
Day <- as.factor(Day)

contrasts(Day)

ham.trendseason<-lm(ham.ts.short~time.ham+Day)

summary(ham.trendseason)

ham.season <- lm(ham.ts.short~Day)
summary(ham.season)

anova(ham.season, ham.trendseason)

spam.ts.short <- ts(spam$count[1:464])
spam.ts <- ts(spam$count)
time.spam <- c(1:length(spam.ts))

spam.trend <- lm(spam.ts~time.spam)
summary(spam.trend)

ggAcf(spam.ts)

pg.precip <- spec.pgram(spam.ts,spans=9,demean=T,log='no')
spec.precip <- data.frame(freq=pg.precip$freq, spec=pg.precip$spec)

max.omega.precip<-pg.precip$freq[which(pg.precip$spec==max(pg.precip$spec))]

max.omega.precip
1/max.omega.precip


e.ts.spam <- ts(spam.trend$residuals)

ggAcf(e.ts.spam)

spam.auto <- auto.arima(e.ts.spam, approximation = FALSE)

summary(spam.auto)

spam.arima111 <- arima(e.ts.spam, order = c(1,1,1), include.mean = FALSE)

e.spam.arima111 <- ts(spam.arima111$residuals)
ggAcf(e.spam.arima111)

spam.auto1 <- auto.arima(spam.ts[1:(length(spam.ts)-7)], approximation = FALSE)
summary(spam.auto1)

e.spam.auto1 <- ts(spam.auto1$residuals)
ggPacf(e.spam.auto1)

library(mtsdi)
library(forecast)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggfortify)
library(ggpubr)
library(tseries)

ggtsdiag(spam.arima111, gof.lag = 20)

