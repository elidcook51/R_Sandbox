#********************************************************************************
#             
#					Univariate Graphics
#
#********************************************************************************

#***************************
# 0.1 installing and loading the library for the visualization
#***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session


#***************************
# 0.2 installing and loading the library for this session
#***************************
# First install packages (only needs to be done once), and next load the packages
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)
#https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************
# 1.1 Set working directory
#***************************

traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"


# set the working directory to traindir
setwd(traindir)
#check the current working directory
#it should be same as your traindir directory
getwd()

#list all files in this directory
dir(traindir)


#***************************
# 1.2 loading the data
#***************************

# Read in the accident files one at at time
# for the first questions in the in-class assignment we will 
# only use data from 2024
accident_data_24 <- read.csv("RailAccidents24.csv")


#***************************
# 1.3 information about the selected year accident data
#***************************

head(accident_data_24) # the first 6 rows (observations) of data
dim(accident_data_24) #number of rows (observation) and number of columns (variables)
summary(accident_data_24) #summary of each column (variable)
str(accident_data_24) #type of each column (variable) of data

# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
str(accident_data_24[,c("ACCDMG", "TOTKLD", "CARS", "STATION")])

colnames(accident_data_24) # number of columns (variable)
class(accident_data_24$TOTKLD) #type of one specific variable 
var(accident_data_24$TOTKLD) #variance of one specific variable (total killed) - quantitative
mean(accident_data_24$TOTKLD) #mean of one specific variable (total killed) - quantitative
table(accident_data_24$TOTKLD) #frequency of each value in the selected variable

class(accident_data_24$STATION) #type of one specific variable 
levels(accident_data_24$STATION) #different values of categorical variable (station) - qualitative
levels(as.factor(accident_data_24$STATION)) #different values of categorical variable (station) - qualitative
table(as.factor(accident_data_24$STATION)) #frequency of each value in the selected variable

# recode STATION as a factor
accident_data_24$STATION = as.factor(accident_data_24$STATION)
levels(accident_data_24$STATION)

# You can round your answer using the round() function
print(mean(accident_data_24$TOTKLD))
print(round(mean(accident_data_24$TOTKLD),3))

#***************************
# 1.4 loading all years data
#***************************

# You will need to read in all 20 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:
setwd(sourcedir)
source("AccidentInput.R")

# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

setwd(traindir)
my.files <- list.files(traindir)

# get only csv files
library(stringi)
my.csv.files <- my.files[which(stri_detect_fixed(my.files,"csv")==TRUE)]

# 2 different ways of loading multiple files in a directory

# the first uses the lapply function
acts <- lapply(my.csv.files, read.csv)

# the second uses file.inputl function from AccidentInput.R
acts <- file.inputl(traindir) 

# the argument for the list.files and file.inputl functions is the
# specification of the path to your file.  In my case traindir

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.
acts[[1]]$YEAR #it shows 1 --> 2001
acts[[5]]$YEAR #it shows 5 --> 2005

# Before we put all the data into one data frame
# we must clean the data
##################################################
#
#	2. Data Cleaning
#
##################################################

#***************************
# 2.1 combine all years of data
#***************************
# Now combine the data frames for all 23 years
# Use combine.data() function from AccidentInput.R
# which takes as an argument the list of dataframes
totacts <- combine.data(acts)

#check the number of rows and columns in this merged dataset
dim(totacts)

# the total accident should be combination of all data
rows = 0
for(acc_year in acts){
  print(dim(acc_year))
  rows = rows + dim(acc_year)[1]
  print(rows)
}
# the following numbers should be the same
print(rows) #total number of observations
dim(totacts)[1] #total number of observations


# How many entries (observations) does each year of accidents have?
library(tidyverse)
annualCounts <- totacts %>% count(YEAR)
annualCounts

testCounts = count(totacts, YEAR)
testCounts

#***********************************
#
# 	3. Investigating particular accidents
#
#***********************************

# Which accident has the most damage?
which(totacts$ACCDMG == max(totacts$ACCDMG))
totacts[which(totacts$ACCDMG == max(totacts$ACCDMG)),]

totacts %>% filter(ACCDMG == max(ACCDMG))

# create a new data frame of the filtered data
worst_dmg <- totacts %>% filter(ACCDMG == max(ACCDMG))
worst_dmg

# what type of accident had the most damage?
totacts$TYPE[which(totacts$ACCDMG == max(totacts$ACCDMG))]
worst_dmg %>% select(TYPE)

# Find out the worst accident for total killed and injured
max_totinj = which(totacts$TOTINJ == max(totacts$TOTINJ))
max_totinj
totacts$TOTINJ[max_totinj]

totacts[max_totinj,]
totacts[max_totinj,122:136]
totacts$ACCDMG[max_totinj]

# find the same information (TOTINJ of worst accident) with tidyverse
worst_inj <- totacts %>% filter(TOTINJ == max(TOTINJ))
worst_inj
worst_inj %>% select(TOTINJ)

# What's the narrative of the worst accident for total injured?
worst_inj %>% select(starts_with("NARR"))
worst_inj %>% select(ACCDMG)


#***********************************
#
# 	4. Visualization
#
#***********************************

#***************************
# 4.1 Histogram
#***************************

# These examples are for year 2011 
# Histograms are both in default way and also with ggplot2 package

# for 2011
ggplot(totacts, 
       aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins =20) + 
  ggtitle("Total Accident Damage across all years") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Different bin widths

# Bin FD
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.FD(acts[[11]]$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Bin Scott
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.scott(acts[[11]]$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Bin 20
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 20) + ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

# Bin 2
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 2) + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))


# other years
selected_years = totacts %>% filter(YEAR %in% c(1,4,8,11))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)

# random years\
set.seed(1)
selected_years = totacts %>% filter(YEAR %in% sample(1:24,4))
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)


# Accident damage 2009-2020

# filter the selected years
selected_years = totacts %>% filter(YEAR %in% c(9:20))
# histogram of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)

# histogram of accident damage based on each year with bins = 10
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 10) + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)


# Damage in constant scales
selected_years = totacts %>% filter(YEAR %in% c(1:12))
# histogram of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  xlim(c(0,1.7e7)) + ylim(c(0,1000)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~YEAR)


#***************************
# 4.2 Boxplots of ACCDMG
#***************************

# Different marker shapes for outliers
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) +
  geom_boxplot(col= "steelblue",outlier.shape=1) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = "*", outlier.size = 5) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip() 

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = 4, outlier.size = 2) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()


# multiple years on a single graph
selected_years = totacts %>% filter(YEAR %in% c(1,4,8,11))
# box plot of accident damage based on each year
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# changing boxplot direction with coord_flip()
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  facet_wrap(~YEAR)


# box plot of accident damage based on each year
# With a constant scale
ggplot(as.data.frame(selected_years), aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  xlim(c(0,1.7e7)) +
  facet_wrap(~YEAR)


#***************************
# 4.3 QQ plots of ACCDMG
#***************************

#Does our data come from a Gaussian distribution
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(sample=acts[[11]]$ACCDMG)) + 
  stat_qq() + 
  stat_qq_line() +
  ggtitle("Total Accident Damage") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(acts[[11]]$TEMP), aes(sample=acts[[11]]$TEMP)) + 
  stat_qq() + 
  stat_qq_line() + 
  ggtitle("Accident Temperature") + 
  theme(plot.title = element_text(hjust = 0.5))


#***************************
# 4.4 Density plot of ACCDMG
#***************************

d <- density(acts[[11]]$ACCDMG,
             kernel = "gaussian")
plot(d,  main = "gaussian kernel", col = 'red')

#density plot on top of histogram
h <- hist(acts[[20]]$ACCDMG, breaks = "FD", plot = FALSE)
ggplot(data=acts[[20]], aes(ACCDMG)) + 
  geom_histogram(aes(y =..density..),
                 breaks=h$breaks,
                 col="green",
                 fill="green",
                 alpha=.2) +
  geom_density(fill="red", color=1, alpha=0.4) +
  labs(title="Density and Histogram for ACCDMG", x="ACCDMG", y="Count") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))

#***************************
# 4.5 Bar plot of Accident Types
#***************************

ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity")

ggplot(as.data.frame(totacts$TYPE), aes(x = totacts$TYPE)) + 
  geom_bar(stat="bin")

# Add color, a title, and change the text size and rotate text
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) +
  geom_bar(stat="identity",fill= "steelblue")+ 
  ggtitle("Accident Frequency by Type") +
  labs(x = "Type of Accident")+
  theme(axis.text.x = element_text(size = 10,  angle = -30))

#Now, let's put more meaningful labels on TYPE variable
class(totacts$TYPE)
unique(totacts$TYPE)
unique(as.factor(totacts$TYPE))
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", 
                "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", 
                "GradeX", "Obstruction", "Explosive", "Fire","Other",
                "SeeNarrative" ))

# Add color, a title, and change the text size and rotate text
ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) +
  geom_bar(stat="identity",fill= "steelblue")+ 
  ggtitle("Accident Frequency by Type") +
  labs(x = "Type of Accident")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))

# Look at TYPEQ
class(totacts$TYPEQ)
unique(totacts$TYPEQ)
unique(as.factor(totacts$TYPEQ))

# Now convert to factor- use actual categories from data dictionary to be more informative
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("NA", "Freight", "Passenger", "Commuter", 
                 "Work",  "Single", "CutofCars", "Yard", "Light", "Maint",
                 "MaintOfWay", "Passenger", "Commuter", "ElectricMulti", "ElectricMulti"))

# Use barplot() to graph frequencies corresponding to different types of trains
ggplot(as.data.frame(table(totacts$TYPEQ)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  labs(x = "Type of Consist")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))

# Look at CAUSE with summary
# CAUSE: primary cause of incident
summary(totacts$CAUSE)
unique(totacts$CAUSE)

# Create a new variable called Cause
# that uses labels for cause.
# Add it to totacts.
totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor
totacts$Cause <- factor(totacts$Cause)

# use table() and barplot() to see it.
table(totacts$Cause)

# Use barplot() to graph frequencies corresponding to different types of trains
ggplot(as.data.frame(table(totacts$Cause)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  labs(x = "Cause of Accident")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))

#***************************
# 4.6 Time series
#***************************

# Time series of total damages
df <- data.frame(year=2001:2024,damages=tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum))
ggplot(data=df, aes(x=year, y=damages)) + 
  geom_line() + 
  geom_point()

