#******************************************************
#
#					    
#			 		Multivariate Viz
#
#******************************************************



#***************************
# 0. installing and loading the library for this session
#***************************
# First install packages (only needs to be done once), and next load the packages
library(ggplot2)
library(dplyr)
#install.packages('psych')
library(psych)
#install.packages('lattice')
library(lattice)


#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************
# 1.1 Set working directory
#***************************

# Set working directory
traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"


# set the working directory to traindir
setwd(traindir)
#check the current working directory
#it should be same as your traindir directory
getwd()

#***************************
# 1.2 loading all years data
#***************************
# Source AccidentInput
setwd(sourcedir)
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2023
totacts <- combine.data(acts)

# Get dimensions of the combined dataframe
dim(totacts)


#*************************************************
#
#		2. More Data Cleaning
#
#*************************************************

# renaming categorical variables (see UnivarViz.R)
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


#***********************************
#
# 	3. Visualization
#
#***********************************


#***************************
# 3.1 scatter plots
#***************************

# scatter plot matrix of severity metrics for 2024
pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = acts[[24]])

# with panel function from psych package - a little more detail
pairs.panels(acts[[24]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

# Do this for all accidents
pairs.panels(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

# Save as png to avoid problems in the document- make sure not to save in directory with data files
png("metrics.png")
pairs.panels(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])
dev.off()


#***************************
# 3.3 Trellis Categorical Plots
#***************************
# Plotting damage per year
ggplot(data = totacts, aes(x = as.factor(YEAR), y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(x = "Year", y = "Damage ($)")


# Plotting accident cause vs. damage
ggplot(data = totacts, aes(x = Cause, y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(y = "Damage ($)", x = "Accident Cause")

# Plot scaled (log) accident damage using bwplot (from lattice package)
bwplot(Cause~ log(ACCDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = totacts)

# Plot cause vs. scaled (log) accident damage
ggplot(data = totacts, aes(x = Cause, y = log(ACCDMG+1))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Log(Accident Damage)") +
  labs(y = "log(Damage ($))", x = "Accident Cause")

# Plot cause vs. no. killed 
ggplot(data = totacts, aes(x = Cause, y = TOTKLD)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Box Plots of Total Killed")+
  labs(y = "Total Killed", x = "Accident Cause")

# Plot cause vs. injured
ggplot(data = totacts, aes(x = Cause, y = TOTINJ)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Total Injured") +
  labs(y = "Total Injured", x = "Accident Cause")


# X-Y plots conditioned on cause for both killed and injured
qplot(ACCDMG, TOTKLD, data = totacts) + geom_smooth() +
  facet_wrap(~Cause, scales = "free") +
  ggtitle("Damage vs. Killed Conditioned on Cause") + 
  labs(x =  "Total Accident Damage", y  = "Total Killed") +
  theme(plot.title = element_text(hjust = 0.5))

xyplot(ACCDMG~TRNSPD | Cause, main = "Extreme Damage vs. Train Speed Conditioned on Cause", 
       xlab = "Train Speed", ylab = "Total Accident Damage", data = totacts, type = c("p", "r"))


#*****************************************************
# 3.4 Heatmaps of Categorical Variables
#*****************************************************

# frequency of events of each Cause and TYPE
table(totacts$Cause, totacts$TYPE)
heatmap(table(totacts$Cause, totacts$TYPE), Rowv = NA, Colv = NA)

# damages from events of each Cause and TYPE
heatmapTable <- with(totacts, tapply(ACCDMG, list("Cause#"=Cause, "TYPE#"=TYPE), sum))
heatmap(heatmapTable, Rowv = NA, Colv = NA)

# With legend (function from gplots)
library(gplots)
heatmap.2(heatmapTable, Rowv = F, Colv = F)

source("http://www.phaget4.org/R/myImagePlot.R")
# if you are getting "invalid graphics state" error, try executing dev.off() on the Console
myImagePlot(heatmapTable, title = "ACCDMG by Cause and Type of Accident")

# replace NAs (never had an accident with that combination) with 0 damages
heatmapTable <- replace(heatmapTable, is.na(heatmapTable), 0)
myImagePlot(heatmapTable, title = "ACCDMG by Cause and Type of Accident")


#*****************************************************
# 3.5 Barplots of Paired Categorical Variables
#*****************************************************
# causes of each type
ggplot(totacts, aes(x=TYPE, fill=Cause), reorder(Cause)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Set2") + #https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
  coord_flip()

# reverse: types of each cause
ggplot(totacts, aes(x=Cause, fill=TYPE), reorder(TYPE)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired") + 
  coord_flip()

library(RColorBrewer)
colorCount = length(unique(totacts$TYPE))
getPalette = colorRampPalette(brewer.pal(11, "Paired"))
palette = getPalette(colorCount)

ggplot(totacts, aes(x=Cause, fill=TYPE), reorder(TYPE)) +
  geom_bar(position="fill") +
  scale_fill_manual(values = palette) + 
  coord_flip()
