
library(ggplot2)
library(dplyr)

traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

setwd(traindir)
dir()

setwd(sourcedir)

source("AccidentInput.R")
acts <- file.inputl(traindir) 
totacts = combine.data(acts)
dim(totacts)

summary(totacts)
summary(totacts$HIGHSPD)

acts_over_100 <- totacts %>% filter(HIGHSPD > 100)
acts_over_100 %>% nrow()

spec_years = totacts %>% filter(YEAR %in% c(9, 12))

ggplot(spec_years, aes(CARSDMG)) + 
  geom_boxplot(fill = 'steelblue') +
  ggtitle('Equipment Damage over the Years') +
  facet_wrap(~YEAR)

ggplot(totacts, aes(CARSDMG)) + 
  geom_boxplot(fill = 'steelblue') +
  ggtitle('Cars Damage over the Years') +
  facet_wrap(~YEAR)

worst_inj <-  totacts %>% filter(TOTINJ == max(TOTINJ))
worst_inj$INCDTNO
worst_inj$TOTINJ
max(totacts$TOTINJ)

most_dmg <- totacts %>% filter(ACCDMG == max(ACCDMG))
most_dmg$YEAR
most_dmg$TYPE
most_dmg$INCDTNO
most_dmg$RAILROAD
most_dmg$ACCDMG
max(totacts$ACCDMG)

most_deaths <- totacts %>% filter(TOTKLD == max(TOTKLD))
most_deaths$INCDTNO
most_deaths$YEAR
most_deaths$TOTKLD
max(totacts$TOTKLD)

dmg_over <- totacts %>% filter(ACCDMG > 1500000)
dmg_over %>% nrow()

deaths <- totacts %>% filter(TOTKLD > 0)
deaths %>% nrow()

ggplot(totacts, aes(TRNSPD)) +
  geom_histogram(fill = 'steelblue')

# Use barplot() to graph frequencies corresponding to different types of trains
ggplot(as.data.frame(table(totacts$Cause)), aes(x = Var1, y= Freq)) + 
  geom_bar(stat="identity")
