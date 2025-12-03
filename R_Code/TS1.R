#************************************************************
#
#  			SPAM Filter  
#			Time Series Analysis 
#
#
#************************************************************


#*****************************
#
# Load the data & source files
#
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

#*****************************
# Observations
#     &  
# Model trend, seasonality
#*****************************

##Plot the time series you created for ham, ham.ts.
plot(ham.ts)
autoplot(ham.ts,ylab="Number of Ham Emails",xlab="Day")

date <- ham %>% select(year, month, day) %>% mutate(date = make_datetime(year,month,day))
ham$date <- date$date
ggplot(ham, aes(x=date,y=count)) + geom_line() + ylab("Ham Count") + xlab("")

#What do you observe in the ham time series?

##The daily time series for ham.ts shows a change in sampling during the last six weeks. 

#Notched box plots for days 1:464 and days 465:506
boxplot(ham.ts[1:464], ham.ts[465:506], notch = T, ylab = 'Ham E-mails', 
        names = c('Weeks 1/13/00 - 4/20/01', 'Weeks 4/20/01 - 6/1/01'), 
        main = 'Notched Box Plots for Ham E-mails', col = c('blue', 'red'))

ham$set <- rep(0,length(ham$count))
ham$set[465:506] <- rep(1,length(ham$count[465:506]))
ggplot(ham, aes(x=as.factor(set),y=count)) + geom_boxplot() + 
  ylab('Ham E-mails') + xlab('Pre 4/20/01 (0) and post (1)')

##To test for this change we can perform a Wilcoxon test 
# of the means for ham.ts for days 1:464 and days 465:506
wilcox.test(ham.ts[1:464],ham.ts[465:506])

#Remove the last six weeks from ham.ts
ham.ts <-ts(ham$count[1:464])
autoplot(ham.ts)

ham <- ham[1:464,]

#Use the acf() command on your created time series for ham.
acf(ham.ts)
ggAcf(ham.ts)

#Do you notice any seasonality in ham?

##Weekly periodicity.

##Plot the time series you created for spam, spam.ts.
autoplot(spam.ts,ylab="Number of Spam Emails",xlab="Day")

date <- spam %>% select(year, month, day) %>% mutate(date = make_datetime(year,month,day))
spam$date <- date$date
ggplot(spam, aes(x=date,y=count)) + geom_line() + ylab("Spam Count") + xlab("")

#What do you observe in the spam time series?

##Not anything obvious, potentially weekly seasonality


#Use the acf() command on your created time series for spam.
ggAcf(spam.ts)

#Do you notice any seasonality in spam?

##Not as obvious but a potentially weekly periodicity.

#Model trend of spam

#Create a new variable time.spam which is a matrix of (length(spam.ts))
time.spam<-c(1:(length(spam.ts)))

#Build a new model, spam.trend which predicts spam.ts based on the time variable, time.spam
spam.trend<-lm(spam.ts~time.spam)

##use the summary() command on spam.trend. Is time significant in predicting spam frequency?
summary(spam.trend)

#Plot the trend line for spam.ts
plot(spam.ts)
abline(spam.trend,col='red')

ggplot(spam, aes(x=date,y=count)) + geom_line() + 
  stat_smooth(method="lm",col="red") + ylab("Spam Emails")

#Model trend of ham

#Create a new variable time.ham which is a matrix of (length(ham.ts))
time.ham<-c(1:length(ham.ts))

#Build a new model, ham.trend, which predicts ham.ts based on the time variable, time.ham
ham.trend<-lm(ham.ts~time.ham)
summary(ham.trend)

#Plot the trend line for ham.ts
#ggplot(ham, aes(x=date,y=count)) + geom_line() + 
  #stat_smooth(method="lm",col="red") + ylab("Ham Emails")


#Model the seasonality for ham data set using dummy variables. Use day of the week as the interval.

ham.day <- time.ham %% 7
ham.day <-as.factor(time.ham %% 7) 

#January 13, 2000, Thursday is first day of ham e-mail time series
##It's better to change the level values into 'F,Sa,S,M,T,W,Th' for understanding

Day <- rep(NA, length(ham.ts))
Day[which((time.ham %% 7)    == 1)] <- "Th"  
Day[which((time.ham %% 7)    == 2)] <- "F"
Day[which((time.ham %% 7)    == 3)] <- "Sa"
Day[which((time.ham %% 7)    == 4)] <- "S"
Day[which((time.ham %% 7)    == 5)] <- "M"
Day[which((time.ham %% 7)    == 6)] <- "T"
Day[which((time.ham %% 7)    == 0)] <- "W"
Day <- as.factor(Day)

contrasts(Day)

#What is the base case?




#Build a model ham.trendseason to model the trend and seasonality of ham.
ham.trendseason<-lm(ham.ts~time.ham+Day)


#Is ham.trendseason significant?
summary(ham.trendseason) 


# plot ham.trendseason
plot(ham.ts)
lines(ts(ham.trendseason$fitted.values),col="red")

ggplot(ham, aes(x=date,y=count)) + geom_line() + ylab("Ham Emails") +
  geom_line(aes(x=date,y=ham.trendseason$fitted.values), color="red")


# see if Saturday and Sunday are statistically different
# recode base case to Saturday
contrasts(Day)<-matrix(c(1,0,0,0,0,0,0, 0,1,0,0,0,0,0, 0,0,1,0,0,0,0,  0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1),nrow=7)
colnames(contrasts(Day)) <-matrix(c("F","M","S","T","Th","W"),ncol=6)
contrasts(Day)

ham.trendseason<-lm(ham.ts~time.ham+Day)
summary(ham.trendseason) 


# create a model with just weekdays and weekends as dummies
WD <- rep(NA, length(ham.ts))
WD[which((time.ham %% 7)    == 1)] <- "WD"  
WD[which((time.ham %% 7)    == 2)] <- "WD"
WD[which((time.ham %% 7)    == 3)] <- "WE"
WD[which((time.ham %% 7)    == 4)] <- "WE"
WD[which((time.ham %% 7)    == 5)] <- "WD"
WD[which((time.ham %% 7)    == 6)] <- "WD"
WD[which((time.ham %% 7)    == 0)] <- "WD"
WD <- as.factor(WD)


ham.trendseason<-lm(ham.ts~time.ham+WD)
summary(ham.trendseason) 

ggplot(ham, aes(x=date,y=count)) + geom_line() + ylab("Ham Emails") +
  geom_line(aes(x=date,y=ham.trendseason$fitted.values), color="red")
