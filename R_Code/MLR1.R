#    			
#			
#	 Multiple Linear Regression 1
#
#******************************************************

traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

##load data
setwd(sourcedir)
source("AccidentInput.R")

#load libraries
library(ggplot2)
library(GGally)

acts <- file.inputl(traindir)

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
#  	Possible predictors of damage	
#***********************************************************


#Scatter plot matrices for quantitative predictors and single metric.
ggpairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])


# PCA
# Principal components with the correlation matrix for extreme data with 1 metric and quantitative predictors.
setwd(sourcedir)
source("PCAplots.R")

pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd)[,1])


## Which predictors are most correlated with accident damage?


###############################
# Categorical plots

# heatmap
source("http://www.phaget4.org/R/myImagePlot.R")
heatmapTable <- with(totacts, tapply(ACCDMG, list("Cause#"=Cause, "TYPE#"=TYPE), sum))
heatmapTable <- replace(heatmapTable, is.na(heatmapTable), 0)
myImagePlot(table(xdmgnd$Cause, xdmgnd$TYPE), title = "ACCDMG by Cause and Type of Accident")

## Which accident causes and types have the highest numbers of extreme accidents?

# Type & TRNSPD
library(lattice)

xyplot(log(ACCDMG)~TRNSPD | TYPE, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ TYPE, scales = "free")


# Cause & TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))

qplot(TRNSPD, log(ACCDMG), data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause, scales = "free")


##What is notable about the relationship between train speed and accident
##damages for different accident causes and types?

#More complex xyplots

# Cause X Type and TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause * TYPE, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * TYPE, scales = "free")


# Create the Derail variable & 
# then look at interactions with Cause
xdmgnd$Derail <- (xdmgnd$TYPE == "Derailment")
xdmgnd$Derail

# plot xy with interactions of Derail and Cause
xyplot(log(ACCDMG)~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))

qplot(log(ACCDMG), TRNSPD, data = xdmgnd) +  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~ Cause * Derail, scales = "free")


##################################################
# Linear Models with Quantitative Predictors Only
##################################################

# Build linear regression models with different combinations of quantitative predictors to provide evidence for your hypothesis

# Single predictor
xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
summary(xdmgnd.lm1)
#null hypothesis for model utility? 
#Btemp = 0
#fail to reject the null hypothesis because temp is not significant at p<0.05 level

names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$residuals^2)


# Two predictors
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)
#null hypothesis for the model utilty?
#Btemp = Btrnspd = 0


#null hypothesis for the t-test for temperature?
#Btemp = 0

summary(xdmgnd.lm2)
coef(xdmgnd.lm2)



#Linear regression model with 3 predictors
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)
summary(xdmgnd.lm3)

##null hypothesis for model utility?
#Btemp=Btrsnpd=Bcars=0
#reject the null hypothesis because TRNSPD and CARS are significant at p<0.05 level
coef(xdmgnd.lm3)
sum(xdmgnd.lm3$residuals^2)


xdmgnd.lm4<-lm(ACCDMG~TRNSPD+CARS,data=xdmgnd)
summary(xdmgnd.lm4)
sum(xdmgnd.lm4$res^2)
# Interpret your model coefficients.  Do they make sense?



# Interpret your developed models using the model utility test and t-test.



# Write out the null and alternative hypothesis for each of the tests.  



# Do you reject or fail to reject H0 for each test?




####################################
#	Now repeat for y = Casualties = TOTKLD + TOTINJ
####################################
