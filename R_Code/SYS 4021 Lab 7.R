datadir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data"
sourcedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"

#libraries
library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lattice)
library(psych)
library(dplyr)
library(mtsdi)
library(tidyverse)
library(ggfortify)
library(ggpubr)
library(tseries)
library("car")
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
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(ggResidpanel)
setwd(sourcedir)
source('AccidentInput.R')
source('PCAplots.R')
source("pc.glm.R")
source("ROC.R")
source("TestSet.R")

setwd(datadir)
titanic <- read.table('titanic.csv', header = T, sep = ',')
setwd(sourcedir)

sum(titanic$Survived)

titanic$Sex
mSubset <- titanic[titanic$Sex == 'male',]
wSubset <- titanic[titanic$Sex == 'female',]
propMale <- sum(mSubset$Survived) / nrow(mSubset)
propMale
propFemale <- sum(wSubset$Survived) / nrow(wSubset)
propFemale

titanic$Pclass
p1class <- titanic[titanic$Pclass == 1,]
p2class <- titanic[titanic$Pclass == 2,]
p3class <- titanic[titanic$Pclass == 3,]

propP1 <- sum(p1class$Survived) / nrow(p1class)
propP2 <- sum(p2class$Survived) / nrow(p2class)
propP3 <- sum(p3class$Survived) / nrow(p3class)

propP1
propP2
propP3

titanic$Embarked
Cclass <- titanic[titanic$Embarked == 'C',]
Sclass <- titanic[titanic$Embarked == 'S',]
Qclass <- titanic[titanic$Embarked == 'Q',]

propC <- sum(Cclass$Survived) / nrow(Cclass)
propS <- sum(Sclass$Survived) / nrow(Sclass)
propQ <- sum(Qclass$Survived) / nrow(Qclass)

propC
propS
propQ

pairs.panels(titanic[,c('Age', 'SibSp', 'Parch', 'Fare', 'Survived')])

null.glm <- glm(Survived~1, data = titanic, family = binomial)

mainEffects <- glm(Survived ~ Sex + Pclass + Embarked + Age, data = titanic, family = binomial)
summary(mainEffects)
summary(null.glm)
anova(null.glm, mainEffects, test = 'Chi')

second.model <- glm(Survived ~ Age + Fare + Sex + Pclass, data = titanic, family = binomial)
summary(second.model)
anova(null.glm, second.model, test = 'Chi')

second.model.step <- step(second.model, trace = 0)
summary(second.model.step)
summary(second.model)

step.model.pred <- predict(second.model.step, type = 'response', newdata = titanic$Survived)
score.table(step.model.pred, titanic$Survived, 0.5)

roc.plot.gg <- plot.roc.gg(step.model.pred, titanic$Survived, 'Only one')
roc.plot.gg

setwd(datadir)
auto <- read.table('auto.csv', header = T, sep = ',')
setwd(sourcedir)

auto$cylinders <- as.factor(auto$cylinders)
auto$origin <- as.factor(auto$origin)
auto$car <- as.factor(auto$car)

ggplot(auto, aes(mpg)) + geom_boxplot()

pairs.panels(auto[,c('horsepower', 'displacement', 'weight', 'acceleration', 'year', 'mpg')])

mpg.main <- lm(mpg ~ cylinders + weight + displacement, data = auto)
summary(mpg.main)


contrasts(auto$cylinders) <- matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1), nrow = 5)
colnames(contrasts(auto$cylinders)) <- matrix(c(3,5,6,8), ncol =4)
contrasts(auto$cylinders)

mpg.main.rec <- lm(mpg~cylinders + weight + displacement, data = auto)
summary(mpg.main.rec)

autoplot(mpg.main.rec)
ols_test_breusch_pagan(mpg.main.rec)

setwd(datadir)
unemployment <- read.table('unemployment.txt', header = T, sep = ',')
setwd(sourcedir)

unemp.ts <- ts(unemployment)
autoplot(unemp.ts)
acf(unemp.ts)
pacf(unemp.ts)

auto.model <- auto.arima(unemp.ts, approximation = FALSE)
summary(auto.model)

unemp.forecast <- forecast(auto.model, h = 1)
summary(unemp.forecast)

temp.time <- c(1:(length(unemp.ts) - 1))
unemp.auto <- auto.arima(unemp.ts[temp.time], approximation = FALSE)
new.forecast <- forecast(unemp.auto, h = 1)
summary(new.forecast)
unemp.ts[130]
(unemp.ts[130] - 4.63)^2
