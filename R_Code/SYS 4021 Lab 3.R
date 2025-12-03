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

accdmg.lm1 = lm(ACCDMG ~ TEMP + TRNSPD + CARS +HEADEND1, data = xdmgnd)
summary(accdmg.lm1)
autoplot(accdmg.lm1, which=2, ncol = 1, label.size = 3) + theme_bw()
xdmgnd[4999,]

max(boxcox(accdmg.lm1, plotit=F)$y)

accdmg.lm1.trans = lm((ACCDMG^(-0.5) - 1)/(-0.5) ~ TEMP + TRNSPD + CARS + HEADEND1, data = xdmgnd)
autoplot(accdmg.lm1.trans, which = 4, label.size = 3) + theme_bw()

tester = lm(ACCDMG ~ as.factor(WEATHER), data = xdmgnd)
xdmgnd$WEATHER
summary(tester)

accdmg.lm2 = lm(ACCDMG ~ TYPE, data = xdmgnd)
summary(accdmg.lm2)

xdmgnd$Derail <- rep(0, nrow(xdmgnd))
xdmgnd$Derail[which(xdmgnd$TYPE == 'Derailment')] <- 1
xdmgnd$Derail <- as.factor(xdmgnd$Derail)
xdmgnd$Derail

accdmg.lm3 <- lm(ACCDMG ~ Derail, data = xdmgnd)
summary(accdmg.lm3)

accdmg.lm4 <- lm(ACCDMG ~ (Derail + TRNSPD + TONS + CARS + HEADEND1)^2, data = xdmgnd)
accdmg.lm4.step <- step(accdmg.lm4)

summary(accdmg.lm4.step)

anova(accdmg.lm4, accdmg.lm4.step)
AIC(accdmg.lm4)
AIC(accdmg.lm4.step)

xdmgnd$Freight <- rep(0, nrow(xdmgnd))
xdmgnd$Freight[which(xdmgnd$TYPEQ == "Freight")] <- 1 
xdmgnd$Freight <- as.factor(xdmgnd$Freight)

Speed = cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD), median(xdmgnd$TRNSPD), max(xdmgnd$TRNSPD)), include.lowest = T, labels = c('low speed', 'high speed'))
Cars <- cut(xdmgnd$CARS, c(min(xdmgnd$CARS),1,max(xdmgnd$CARS)), include.lowest = T, right=F, labels = c("low hzd", "high hzd"))

interaction.plot(Cars, Speed, log(xdmgnd$ACCDMG))
interaction.plot(Speed, xdmgnd$Freight, log(xdmgnd$ACCDMG))
interaction.plot(Speed, xdmgnd$Derail, log(xdmgnd$ACCDMG))
