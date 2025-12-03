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
library(data.table)
library(plyr)
library(scales)
library(grid)
library(psych)
library(lattice)
library(car)
setwd(sourcedir)
source('AccidentInput.R')
source('PCAplots.R')

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

totacts$Casualty = totacts$TOTINJ + totacts$TOTKLD
casualtyActs = filter(totacts, totacts$Casualty != 0)
casualtyActsND = casualtyActs[!(duplicated(casualtyActs[, c('INCDTNO', 'YEAR', 'MONTH', 'DAY', 'TIMEHR', 'TIMEMIN')])),]


newSet = filter(xdmgnd, xdmgnd$TYPTRK %in% c(1,2))
newSet$track = as.factor(newSet$TYPTRK)
testLm = lm(ACCDMG ~ (track + TRNSPD)^2, data = newSet)
summary(testLm)
Speed <- cut(newSet$TRNSPD, c(min(newSet$TRNSPD),median(newSet$TRNSPD),max(newSet$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))
Speed
interaction.plot(Speed, newSet$track, newSet$ACCDMG)
testLm2 = step(testLm)
summary(testLm2)

totacts$Casualty = totacts$TOTINJ + totacts$TOTKLD
casualtyActs = filter(totacts, (totacts$Casualty != 0) & (totacts$Casualty != 1001))
casnd = casualtyActs[!(duplicated(casualtyActs[, c('INCDTNO', 'YEAR', 'MONTH', 'DAY', 'TIMEHR', 'TIMEMIN')])),]

casnd$Collision = rep(0, nrow(casnd))
casnd$Collision[which(casnd$TYPE %in% c('HeadOn', 'Rearend', 'Side', 'Raking', 'BrokenTrain'))] <- 1
casnd$Collision <- as.factor(casnd$Collision)

casnd$Hazard = rep(0, nrow(casnd))
casnd$Hazard[which(casnd$CARSHZD != 0)] <- 1
casnd$Hazard <- as.factor(casnd$Hazard)

col = filter(casnd, casnd$Collision == 1)
nocol = filter(casnd, casnd$Collision == 0)
Speed <- cut(casnd$TRNSPD, c(min(casnd$TRNSPD),median(casnd$TRNSPD),max(casnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))
highspd <- filter(casnd, casnd$TRNSPD > median(casnd$TRNSPD))
lowspd <- filter(casnd, casnd$TRNSPD < median(casnd$TRNSPD))
interaction.plot(Speed, casnd$Collision, log(casnd$Casualty))


ggplot(data = casnd, aes(x = Collision, y = Casualty)) + 
  geom_boxplot()

a = ggplot(data = highspd, aes(x = Collision, y = log(Casualty))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Box Plots of Collision vs log(Casualty) with High Speed")+
  labs(y = "log(Casualty)", x = "Is it a Collision?")
b = ggplot(data = lowspd, aes(x = Collision, y = log(Casualty))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Box Plots of Collision vs log(Casualty) with Low Speed")+
  labs(y = "log(Casualty)", x = "Is it a Collision?")
ggarrange(a, b, ncol=1, nrow=2)

Casualty.firstmodel = lm(Casualty ~ (Hazard + TONS + Collision + TRNSPD)^2 + I(TONS^2) + I(TRNSPD^2), data = casnd)
summary(Casualty.firstmodel)
Casualty.firstmodel.step = step(Casualty.firstmodel, Trace = F)
summary(Casualty.firstmodel.step)

autoplot(Casualty.firstmodel.step, which = c(1,2,4))
boxcox(Casualty.firstmodel.step)
boxcox(Casualty.firstmodel.step, plotit = F)$x[
  which.max(boxcox(Casualty.firstmodel.step, plotit = F)$y)]

Casualty.secondmodel = lm((Casualty^(-1.5) - 1)/(-1.5) ~ (Hazard + TONS + Collision + TRNSPD)^2 + I(TONS^2) + I(TRNSPD^2), data = casnd)
summary(Casualty.secondmodel)
Casualty.secondmodel.step = step(Casualty.secondmodel, Trace = F)
summary(Casualty.secondmodel.step)

autoplot(Casualty.secondmodel.step, which = c(1,2,4))
