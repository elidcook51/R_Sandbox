
#******************************************************
#
#  				             Session 3
#				Duplicates, Categorial Variable Relationships &
#				            Extreme values
#
#******************************************************

#***************************
# 0.1 installing and loading the library for this session
#***************************
# tidyverse libraries
library(ggplot2)
library(dplyr)
#install.packages("stringr")
library(stringr)
#install.packages("GGally")
library(GGally)
#install.packages("psych")
library(psych)
#install.packages("lattice")
library(lattice)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("gplots", dependencies = T)
library(gplots)



#**********************************************************
# 1. loading data
#**********************************************************


# Set working directory (change to your path)

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

#***********************************************************
#		2.2 Extreme data points
#***********************************************************
ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_histogram()

#Look also at TOTKLD and TOTINJ
#what is the more frequent values?

ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
# Get the values in the box plot for ACCDMG
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

#takes the plot object, and performs all steps necessary to produce an object that can be rendered. 
#This function outputs two pieces: a list of data frames (one for each layer), 
#and a panel object, which contain all information about axis limits, breaks etc.
dmgbox.built <- ggplot_build(dmgbox)

names(dmgbox.built) # what variables are associated with the boxplot?
names(dmgbox.built$data[[1]]) # what variables are associated with the boxplot data?


#***********************************************************
#		2.2.1 find only the extremes - the points above the upper whisker
#***********************************************************

# ymax is the upper whisker - anything above that is an outlier
upper <- dmgbox.built$data[[1]]$ymax

# create a new data frame with only the outliers
xdmg <- totacts %>% filter(ACCDMG > upper)
  
# how many outliers are there
count(xdmg)

# What proportion of accidents are extreme?
count(xdmg)/count(totacts)

# Proportion of costs

#summation of extreme ACCDMG 
extreme.sum <- totacts %>% 
  select(ACCDMG) %>% 
  filter(ACCDMG > dmgbox.built$data[[1]]$ymax) %>% 
  sum()
#summation total ACCDMG
total.sum <- sum(totacts$ACCDMG)

#proportion of cost of extreme accident
extreme.sum/total.sum

# Let's look at the most extreme cost accidents. Are there any of particular interest?

# returns row index
which(xdmg$ACCDMG > 15e6)

# returns values filtered on condition
xdmg %>% select(ACCDMG) %>% filter(ACCDMG> 15e6)

# look at the narrative for the first event
xdmg[179,c(122:136)]


#***********************************************************
#		2.2.2 Duplicate data points
#***********************************************************

# The max
which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# how many entries have a max values
xdmg %>% 
  select(ACCDMG) %>% 
  filter(ACCDMG == max(ACCDMG))

# Look at the narrative
as.matrix(names(xdmg)) 
narrative = xdmg %>% 
  select(ACCDMG,c(122:136)) %>% 
  filter(ACCDMG == max(ACCDMG))
# or we can select columns start with NARR
narrative = xdmg %>%  
  select(ACCDMG,starts_with("NARR")) %>% 
  filter(ACCDMG == max(ACCDMG))


# Are there any duplication in these selected columns?
duplicated(xdmg[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# select an observation with an incident number?
which(xdmg$INCDTNO == "110058")
xdmg[which(xdmg$INCDTNO == "110058"),  c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]
xdmg %>% 
  select(INCDTNO,YEAR, MONTH, DAY, TIMEHR, TIMEMIN) %>% 
  filter(INCDTNO == "110058")

# Are there any duplication in these selected columns?
duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# Not duplicated
!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))

# remove the duplicates

# base R
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
# tidyverse
#	keep all variables in .data. If a combination of ... is not distinct, this keeps the first row of values.
xdmgnd <- xdmg %>% distinct(INCDTNO,YEAR, MONTH, DAY, TIMEHR, TIMEMIN, .keep_all = TRUE)

# check out the number of duplicated entries
# dimensions of xdmg and xdmgnd
dim(xdmg)
dim(xdmgnd)

# number of duplicates
count(xdmg) - count(xdmgnd)

