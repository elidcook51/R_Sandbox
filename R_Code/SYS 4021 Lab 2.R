# Load ggplot2 and psych libraries
library(ggplot2)
library(psych)

# libraries needed for ggbiplot and loadingsplot in PCAplots.R
library(data.table)
library(plyr)
library(scales)
library(grid)
library(ggpubr)


traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

setwd(sourcedir)

# Source AccidentInput
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2019

# the combined data frame
totacts <- combine.data(acts)


#*************************************************
#		2. More Data Cleaning
#*************************************************

#***********************************************************
#		2.1 Setup Categorical Variables
#***********************************************************

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

totacts$Casualty <- totacts$TOTKLD + totacts$TOTINJ

ggplot(totacts, aes(Casualty)) +
  geom_boxplot(fill = 'steelblue')

totacts_posCas = totacts %>% filter(Casualty >= 1)
nrow(totacts.casualty)/nrow(totacts)
nrow(totacts.casualty)

totacts_posCas_nd = totacts_posCas[!(duplicated(totacts_posCas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
nrow(totacts_posCas)
nrow(totacts_posCas_nd)

selectYears = totacts_posCas_nd %>% filter(YEAR %in% c(2, 5, 8, 12))
ggplot(selectYears, aes(Casualty)) +
  geom_boxplot(fill = 'steelblue') +
  facet_wrap(~YEAR)

selectYear = totacts_posCas_nd %>% filter(YEAR == 12)
sum(selectYear$Casualty)
nrow(selectYear)

selectType = totacts_posCas_nd %>% filter(TYPE == "Fire")
nrow(selectType)

selectCause = totacts_posCas_nd %>% filter(Cause == "T")
nrow(selectCause)

maxCas = totacts_posCas_nd %>% filter(Casualty == max(totacts_posCas_nd$Casualty))
maxCas$Cause

selectTypeQ = totacts_posCas_nd %>% filter(TYPEQ == "CutofCars")
nrow(selectTypeQ)

sum(totacts_posCas_nd$Casualty)

maxCas$YEAR

totacts_posCas_nd_noMax = totacts_posCas_nd %>% filter(Casualty != max(totacts_posCas_nd$Casualty))
max(totacts_posCas_nd_noMax$Casualty)

pairs.panels(totacts_posCas_nd_noMax[,c("Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

totacts_posCas_pca = princomp(totacts_posCas_nd_noMax[,c("Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T)

source("PCAplots.R")
ggbiplot(totacts_posCas_pca, labels = row(totacts_posCas_nd_noMax)[,1], plot.obs = FALSE)

totacts_posCas_pca$loadings[,2]

cumplot(totacts_posCas_pca)
