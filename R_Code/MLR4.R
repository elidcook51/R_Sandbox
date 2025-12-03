#
#      	    
#			Multiple Linear Regression
#	 Transformations, Qualitative Variables & ANCOVA
#
# ******************************************************

traindir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/TrainData"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

setwd(sourcedir)
source("AccidentInput.R")

library(ggplot2)

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


##******************************************* 
## Investigate interactions visually ##
##*******************************************

# Interaction plots with quantitative variables
Speed <- cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD),median(xdmgnd$TRNSPD),max(xdmgnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))
Cars <- cut(xdmgnd$CARS, c(min(xdmgnd$CARS),1,max(xdmgnd$CARS)), include.lowest = T, right=F, labels = c("low hzd", "high hzd"))

# Plot interaction between Speed and Cars
interaction.plot(Speed, Cars, log(xdmgnd$ACCDMG))

# ggplot version with points
qplot(x = TRNSPD, y = log(ACCDMG), data = xdmgnd, colour = Cars) +
  geom_smooth(method = "lm") 

# ggplot version without points
ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = Cars, color = Cars) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Mean of Accident Damage ($)")+
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Cars on Accident Damage")


# Plot interaction between Freight and Speed
xdmgnd$Freight <- rep(0, nrow(xdmgnd))
xdmgnd$Freight[which(xdmgnd$TYPEQ == "Freight")] <- 1 
xdmgnd$Freight <- as.factor(xdmgnd$Freight)
interaction.plot(Speed, xdmgnd$Freight, log(xdmgnd$ACCDMG))

qplot(x = TRNSPD, y = log(ACCDMG), data = xdmgnd, colour = Freight) +
  geom_smooth(method = "lm") 

ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = xdmgnd$Freight, color = xdmgnd$Freight) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Mean of Accident Damage ($)")+
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Freight on Accident Damage")


# Plot interaction between Derailments and Speed
xdmgnd$Derail <- rep(0, nrow(xdmgnd))
xdmgnd$Derail[which(xdmgnd$TYPE == "Derailment")] <- 1 
xdmgnd$Derail <- as.factor(xdmgnd$Derail)
interaction.plot(xdmgnd$Derail, Speed, log(xdmgnd$ACCDMG))

qplot(x = TRNSPD, y = log(ACCDMG), data = xdmgnd, colour = Derail) +
  geom_smooth(method = "lm")

ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = xdmgnd$Derail, color = xdmgnd$Derail) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  ylab("Mean of Accident Damage ($)")+
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Derailment on Accident Damage")

# Plot interaction between Speed and Tons
Tons <- cut(xdmgnd$TONS, c(min(xdmgnd$TONS),median(xdmgnd$TONS),max(xdmgnd$TONS)), include.lowest = T, labels = c("low tons", "high tons"))
interaction.plot(Speed, Tons, log(xdmgnd$ACCDMG))

qplot(x = TRNSPD, y = log(ACCDMG), data = xdmgnd, colour = Tons) +
  geom_smooth(method = "lm")

ggplot() +
  aes(x = Speed, y = log(xdmgnd$ACCDMG), group = Tons, color = Tons) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  ylab("Mean of Accident Damage ($)") +
  xlab("") +
  theme_bw() + 
  labs(color = "") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  ggtitle("Interaction of Speed and Tons on Accident Damage")

##******************************************* 
## Build linear regression models with interactions in R: lm ##
##*******************************************

# Build a multiple linear regression models with the data fram xdmgnd: 
# xdmgnd.lm1  log(ACCDMG) ~ TEMP + TRNSPD + TONS + CARS + HEADEND1

xdmgnd.lm1<-lm(log(ACCDMG) ~ TEMP + TRNSPD  + CARS + HEADEND1, data=xdmgnd)

# Display regression results for each model
summary(xdmgnd.lm1)

#  create models with interactions between all of the quantitative predictors ----
xdmgnd.lm2<-lm(log(ACCDMG)~(TEMP+TRNSPD+CARS+HEADEND1)^2,data=xdmgnd)

summary(xdmgnd.lm2) 


# Is this the complete second order model?

#  I() allows your model to contain normal mathematical sysmbols 
#  Create complete second order model                            
xdmgnd.lm3 <- lm(log(ACCDMG)~(TEMP+TRNSPD+CARS+HEADEND1)^2+I(TEMP^2)+I(TRNSPD^2)+I(CARS^2)+I(HEADEND1^2),data=xdmgnd)
summary(xdmgnd.lm3)

library(car)
vif(xdmgnd.lm3) # lots of multicollinearity

# Create a backwards regression model on xdmgnd.lm3 
# and see if it reduces multicollinearity
xdmgnd.lm3.step <- step(xdmgnd.lm3)
summary(xdmgnd.lm3.step)
vif(xdmgnd.lm3.step) # some issues with multicollinearity
# could try centering TRNSPD, CARS and HEADEND1

xdmgnd$TRNSPD_anomaly = xdmgnd$TRNSPD - mean(xdmgnd$TRNSPD)
xdmgnd$CARS_anomaly = xdmgnd$CARS - mean(xdmgnd$CARS)
xdmgnd$HEADEND1_anomaly = xdmgnd$HEADEND1 - mean(xdmgnd$HEADEND1)

xdmgnd.lm4 <- lm(log(ACCDMG) ~ TEMP + TRNSPD_anomaly + CARS_anomaly + HEADEND1_anomaly +
                   I(TRNSPD_anomaly^2) + I(CARS_anomaly^2) + I(HEADEND1_anomaly^2) +
                   I(TEMP*CARS_anomaly) + I(TRNSPD_anomaly*CARS_anomaly) +
                   I(TRNSPD_anomaly*HEADEND1_anomaly), data=xdmgnd)
summary(xdmgnd.lm4)
vif(xdmgnd.lm4) # all VIFs < 10, but a few > 5


# Alternatively, try PC regression with the variables remaining in backwards regression model
source("princompreg.R")
pca.df = data.frame(xdmgnd$TEMP,xdmgnd$TRNSPD,xdmgnd$CARS,xdmgnd$HEADEND1,
                    I(xdmgnd$TRNSPD^2),I(xdmgnd$CARS^2),I(xdmgnd$HEADEND1^2),
                    xdmgnd$TEMP*xdmgnd$CARS,xdmgnd$TRNSPD*xdmgnd$CARS,
                    xdmgnd$TRNSPD*xdmgnd$HEADEND1)
xdmgnd.pca = princomp(pca.df,cor=T)

# try with # of PCs to explain 90% of variability
pca.reg = pc.reg(xdmgnd.pca, 90, log(xdmgnd$ACCDMG))

summary(pca.reg)
vif(pca.reg)

# no variance inflation by definition, 
# but lower adjusted R^2 and less interpretable mpdel

# try with # of PCs to explain 95% of variability
pca.reg2 = pc.reg(xdmgnd.pca, 95, log(xdmgnd$ACCDMG))

summary(pca.reg2)
vif(pca.reg2)

# marginal increase in adjusted R^2

##************************* 
## Qualitative Variables: ANOVA ##
##************************* 

# Lets look at the default treatment coding for Cause
contrasts(xdmgnd$Cause)

# What is the base case for Cause?



# Write a model to predict ACCDMG in terms of Cause 
xdmgnd.lm5<-lm(log(ACCDMG)~Cause,data=xdmgnd)
summary(xdmgnd.lm5) 

# the coefficients indicate how much higher or lower the mean is from the base case
# and their significance indicates if that is statistically different than the base case
tapply(as.numeric(log(xdmgnd$ACCDMG)), as.factor(xdmgnd$Cause), mean)

# How do we interpret the model xdmgnd.lm5?



# Change base case to H ----
contrasts(xdmgnd$Cause)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(xdmgnd$Cause)) <-matrix(c("E","M","S","T"),ncol=4)
contrasts(xdmgnd$Cause)

xdmgnd.lm6<-lm(log(ACCDMG)~Cause,data=xdmgnd)
summary(xdmgnd.lm6)

#How do we interpret the model xdmgnd.lm6?


#More qualitative variables:
xdmgnd$WEATHER <- factor(xdmgnd$WEATHER, labels = c("Clear","Cloudy","Rain","Fog","Sleet","Snow"))
contrasts(xdmgnd$WEATHER) #What is the base case?

xdmgnd.lm7<-lm(log(ACCDMG)~WEATHER,data=xdmgnd)
summary(xdmgnd.lm7)


## Create 2 ANCOVA models (quantitative and qualitative variables):

# Build a main effects model Cause + TEMP + TRNSPD +  CARS + HEADEND1
xdmgnd.lm8 <-lm(log(ACCDMG)~Cause + TEMP + TRNSPD +  CARS + HEADEND1,data=xdmgnd)
summary(xdmgnd.lm8)

# Build a main effects + interaction model Cause + TEMP + TRNSPD + CARS + HEADEND1 
xdmgnd.lm9<-lm(log(ACCDMG)~(Cause+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm9)
vif(xdmgnd.lm9) # Generalized VIF shown when there are categorical variables.
#You should compare the right column squared (GVIF^(1/(2*Df)))^2 to 10
VIFs = vif(xdmgnd.lm9)
VIFs^2 # Several > 10, so need to reduce multicollinearity

#Perform a Partial F Test: xdmgnd.lm8 vs. xdmgnd.lm9
anova(xdmgnd.lm8,xdmgnd.lm9)

# Which model do you choose?


## Use stepwise regression on your main effects + interaction model xdmgnd.lm1
xdmgnd.lm9.step <- step(xdmgnd.lm9)

# What is your new model?
summary(xdmgnd.lm9.step)
VIFs = vif(xdmgnd.lm9.step)
VIFs^2 # Cause, TRNSPD, and CARS > 10 so perhaps drop more variables

xdmgnd.lm10<-lm(log(ACCDMG)~(Derail+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm10)
vif(xdmgnd.lm10) # lots of multicollinearity

xdmgnd.lm10.step <- step(xdmgnd.lm10)
vif(xdmgnd.lm10.step) # still some multicollinearity to address (CARS > 10, others > 5)

anova(xdmgnd.lm10, xdmgnd.lm10.step)


##*****************************************Exercise*****************************************
# Choose 1 of your project 1 hypotheses based on a practical, 
# controllable variable.  Build a model with both qualitative
# and quantitative variables to test your hypothesis.  Make sure 
# to address the following steps:
# 1.  Variable selection for linear models.
# 2.  Treatment of categorical variables for your linear model.
# 3.  Measure the performance of the models.
# 4.  Adjust your models based on analytical and graphical diagnostics.
# 5.  Reassess models based on adjustments.
# 6.  Compare your best models.
# 7.  Use your best model to reject or accept your hypothesis and provide a 
#     recommendation supported by statistical evidence.

