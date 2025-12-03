#
#      	    
#			Multiple Linear Regression
#	 Metrics & Variable Selection
#******************************************************

traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

library(dplyr)
library(ggplot2)
library(ggpubr)

#load data
setwd(sourcedir)
source("AccidentInput.R")

# Load a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# combine into one data frame
totacts <- combine.data(acts)

# setup categorical variables
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", 
                                                "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", 
                                                "GradeX", "Obstruction", "Explosive", "Fire","Other",
                                                "SeeNarrative" ))

totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("NA", "Freight", "Passenger", "Commuter", 
                                                  "Work",  "Single", "CutofCars", "Yard", "Light", "Maint",
                                                  "MaintOfWay", "Passenger", "Commuter", "ElectricMulti", "ElectricMulti"))

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

totacts$Cause <- factor(totacts$Cause)

##Build a data frame with only extreme accidents for ACCDMG

dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
upper <- ggplot_build(dmgbox)$data[[1]]$ymax
xdmg <- totacts[totacts$ACCDMG > upper,]

# remove 9/11
xdmg <- xdmg[-179,]

## Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
rownames(xdmgnd) <- NULL


#***********************************************************
#  	Build linear regression models in R: lm
#***********************************************************

xdmgnd.lm1<-lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1, data=xdmgnd)

xdmgnd.lm2<-lm(ACCDMG ~ TEMP + TRNSPD + CARS, data=xdmgnd)


##Display regression results for each model
summary(xdmgnd.lm1)

summary(xdmgnd.lm2)


##Metrics and Variable Selection      

##Adjusted R^2:

summary(xdmgnd.lm1)$adj.r.squared
summary(xdmgnd.lm2)$adj.r.squared

##AIC:

AIC(xdmgnd.lm1)
AIC(xdmgnd.lm2)

##BIC:

BIC(xdmgnd.lm1)
BIC(xdmgnd.lm2)


##Variable Selection

#Forward selection
intercept.model <- lm(ACCDMG ~ 1, data=xdmgnd)
summary(intercept.model)

full.model.formula <- formula(lm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1, data=xdmgnd))

xdmgnd.lm1.fwd<-step(intercept.model, direction='forward', 
                     scope=full.model.formula, trace=T)

summary(xdmgnd.lm1.fwd)
AIC(xdmgnd.lm1.fwd)

#Backward selection
xdmgnd.lm1.bwd<-step(xdmgnd.lm1, direction='backward', trace=T)

summary(xdmgnd.lm1.bwd)
AIC(xdmgnd.lm1.bwd)

#Stepwise Regression  

##If you have many predictors, it will take some time to get results. 
# To save time, you can set 'trace=F' to get results without showing each step:

xdmgnd.lm1.step<-step(xdmgnd.lm1, direction='both', trace=T)

summary(xdmgnd.lm1.step)
AIC(xdmgnd.lm1.step)


#Partial F Test

##Recall that we can only compare two nested models by partial F test:

anova(xdmgnd.lm1,xdmgnd.lm2)
anova(xdmgnd.lm1,xdmgnd.lm1.fwd)
anova(xdmgnd.lm1,xdmgnd.lm1.bwd)
anova(xdmgnd.lm1,xdmgnd.lm1.step)



# Test Sets 

##Source TestSet.R

source("TestSet.R")

##set test sets size:
set.seed(5)
test.size<-1/3

##generate training sets and test sets from original data:

xdmgnd.data<-test.set(xdmgnd,test.size)
names(xdmgnd.data)

##Check distribution of ACCDMG of test set, training set:

#2 different plotting methods

##method 1
par(mfrow=c(1,3))
hist(xdmgnd.data$train$ACCDMG)
hist(xdmgnd.data$test$ACCDMG)
hist(xdmgnd$ACCDMG)
par(mfrow=c(1,1))

##method 2 with ggplot
a <- ggplot(as.data.frame(xdmgnd.data$train$ACCDMG), aes(xdmgnd.data$train$ACCDMG)) + geom_histogram()
b <- ggplot(as.data.frame(xdmgnd.data$test$ACCDMG), aes(xdmgnd.data$test$ACCDMG)) + geom_histogram()
c <- ggplot(as.data.frame(xdmgnd$ACCDMG), aes(xdmgnd$ACCDMG)) + geom_histogram()
ggarrange(a,b,c, ncol=3, nrow = 1)

##Build models with training set:
xdmgnd.lm1.train<-lm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd.data$train)

xdmgnd.lm2.train<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd.data$train)


##Recall that we need to measure predicted MSE. 
##First, how to predict with lm models:

xdmgnd.lm1.pred<-predict(xdmgnd.lm1.train,newdata=xdmgnd.data$test) 



xdmgnd.lm2.pred<-predict(xdmgnd.lm2.train,newdata=xdmgnd.data$test)


##Next, compute PMSE:

pmse.xdmgnd.lm1<-mse(xdmgnd.lm1.pred,xdmgnd.data$test$ACCDMG)

pmse.xdmgnd.lm1


pmse.xdmgnd.lm2<-mse(xdmgnd.lm2.pred,xdmgnd.data$test$ACCDMG)

pmse.xdmgnd.lm2


##Which model is better based on PMSE?



#Version 2 Test Sets- Run multiple iterations with different testing and training sets

##create vectors to store PMSE

pmse1.result<-NULL;
pmse2.result<-NULL;


for (i in c(1:20)){
  #set test sets size: 
  test.size<-1/3
  # generate training sets and test sets from original data:
  xdmgnd.data<-test.set(xdmgnd,test.size)
  
  # Build model with train set:
  lm1.train<-lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1,data=xdmgnd.data$train)
  lm2.train<-lm(ACCDMG ~ TEMP + TRNSPD + CARS,data=xdmgnd.data$train)
  
  # First, how to predict with lm models:
  lm1.pred<-predict(lm1.train,newdata=xdmgnd.data$test) 
  lm2.pred<-predict(lm2.train,newdata=xdmgnd.data$test) 
  
  # Next, compute PMSE:
  pmse.lm1<-mse(lm1.pred,xdmgnd.data$test$ACCDMG)
  pmse.lm2<-mse(lm2.pred,xdmgnd.data$test$ACCDMG)
  
  # Add the PMSE for this run into your vector to store PMSE
  pmse1.result<-c(pmse1.result,pmse.lm1)
  pmse2.result<-c(pmse2.result,pmse.lm2)
}


##Compare models based over 20 runs of PMSE

#Plot results method 1
plot(pmse1.result,type='b',col='blue',xlab="Index", ylab="PMSE")
lines(pmse2.result,type='b',col='red')
title(main="Model Comparison Based on PMSE")

#Plot results method 2 with ggplot
Index <- 1:length(pmse1.result);
df <- data.frame(Index,pmse1.result,pmse2.result)
ggplot(data=df, aes(x=Index)) +
  geom_line(aes(y = pmse1.result), color = 'blue', size = 1) +
  geom_line(aes(y = pmse2.result), color = 'red', linetype = 'twodash', size = 1) +
  ggtitle("Model Comparison Based on PMSE") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

##Which model is better from visual inspection of graph?



#We can also use statistical tests to compare our models.  


#Paired t test:

t.test(pmse1.result,pmse2.result,paired=T)


##Wilcoxon Test:


wilcox.test(pmse1.result,pmse2.result,paired=T)


##Which model performs better based on the paired t test and paired Wilcoxon test?
mean(pmse1.result)
mean(pmse2.result)

# Model 1 has a statistically lower mean pmse and 
# is therefore the preferred model

# Cross-Validation


##Need the boot library

library(boot)


##You need to use glm (a function to estimate generalized linear model) instead of lm. 
##Don't be confused by generalized linear models. Because lm is a special case of glm, glm 
##function can be used to estimate lm models as long as you set parameters correctly.


xdmgnd.lm1.cv<-glm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)

xdmgnd.lm2.cv<-glm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)


##Cross-validation error

xdmgnd.lm1.err<-cv.glm(xdmgnd,xdmgnd.lm1.cv,K=10)



xdmgnd.lm1.err$delta



xdmgnd.lm2.err<-cv.glm(xdmgnd,xdmgnd.lm2.cv,K=10)



xdmgnd.lm2.err$delta


##There are two components for estimated errors: 
# the first is the raw cross-validation estimate of prediction error; 
# the second is the adjusted cross-validation estimate.

##Compare xdmgnd.lm2 and xdmgnd.lm3 based on adjusted cross-validation estimate.  
# Which model performs better?


