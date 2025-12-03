#
#      	    
#			Multiple Linear Regression
#	 Metrics & Variable Selection
#  Diagnostics & Transformations
#******************************************************

traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(MASS)
library(lindia)
library(olsrr)

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

# Diagnostics Plot      

##Generate diagnostics plot one by one

# Optional: All the diagnostic plots together with lindia package

gg_diagnose(xdmgnd.lm1)

#with autoplot
autoplot(xdmgnd.lm1, which=1:6, label.size = 3) + theme_bw()

autoplot(xdmgnd.lm1, which = c(1,2,4,5), ncol = 2, label.size = 3) + theme_bw()

#Plot graphs individually - example with Cook's distance

autoplot(xdmgnd.lm1, which=4, ncol = 1, label.size = 3) + theme_bw() #Cook's distance


#What happened in each of the accidents noted on the Cook's Distance plot?




##What do you observe in each diagnostic plot for xdmgnd.lm1?  Discuss your observations and any issues.

##a. residuals vs. fitted

##b. qq-plot

##c. Scale-Location?

##d. Cook's distance / Residuals vs. Leverage?

# Are the residuals homoscedastic
ols_test_breusch_pagan(xdmgnd.lm1)


#Transformations

##Let's take a look at the response variable ACCDMG.  

ggplot(as.data.frame(xdmgnd$ACCDMG), aes(xdmgnd$ACCDMG)) + geom_density()

##Do we violate the distributional assumption of our response variable?


#Box-Cox Transformation          

boxcox(xdmgnd.lm1) #box-cox plot

#with ggplot
gg_boxcox(xdmgnd.lm1)

# find likelihood at intervals of 0.5 for lambda
boxcox(xdmgnd.lm1, plotit=T, lambda=seq(-2,2,by=0.5))


##get x and y values without plotting

boxcox(xdmgnd.lm1,plotit=F)


##find max likelihood

max(boxcox(xdmgnd.lm1, plotit = F)$y)


## find best lambda value

boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 


##The best lambda and store in L

L<-boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 
L

##The model with the best lambda transformation

xdmgnd.lm1.boxcox<-lm((ACCDMG^L-1)/L~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)


##Display regression results for boxcox model
summary(xdmgnd.lm1)
summary(xdmgnd.lm1.boxcox)

# Note - performance metrics are no longer comparable!
# The RSS are in different units, so cannot directly
# compare adjusted R^2, AIC, etc.
# Make decision between models based on diagnostics first
# Then use metrics to compare models with the same response


##Let's replot our density function for our response variable ACCDMG

plot(density((xdmgnd$ACCDMG^L-1)/L))

ggplot(as.data.frame((xdmgnd$ACCDMG^L-1)/L), aes((xdmgnd$ACCDMG^L-1)/L)) + geom_density()

##Plot diagnostic for your new model xdmgnd.lm1.boxcox

autoplot(xdmgnd.lm1.boxcox, which = c(1,2,4,5), ncol = 2, label.size = 3) + theme_bw()

##What do you observe in the diagnostic plots for your new model xdmgnd.lm1.boxcox?  Did the transformation help?

# Are the residuals homoscedastic?
ols_test_breusch_pagan(xdmgnd.lm1.boxcox)

# still no, but slightly better

# is there any multicollinearity
library(car)
vif(xdmgnd.lm1.boxcox)

