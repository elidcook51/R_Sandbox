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
houses <- read.table('housing-prices.csv', header = T, sep = ',')
setwd(sourcedir)

houses$Age
ggplot(houses, aes(Price)) + geom_boxplot()

pairs(~ Rooms + Baths + Size + Price, data = houses)
pairs.panels(houses[,c('Rooms', 'Baths', 'Size', 'Price')])

houses.lm1 <- lm(Price ~ Baths, data = houses)
summary(houses.lm1)

houses.fullmain <- lm(Price ~ Baths + Rooms + Age + Size, data = houses)
summary(houses.fullmain)

anova(houses.lm1, houses.fullmain)

houses.fullinteract <- lm(Price ~ (Baths + Rooms + Age + Size)^2, data = houses)
summary(houses.fullinteract)

houses.step = step(houses.fullinteract, trace = 0)
summary(houses.step)
AIC(houses.step)

autoplot(houses.step)
ols_test_breusch_pagan(houses.step)
houses[9,]

boxcox(houses.step)
L<-boxcox(houses.step, plotit = F)$x[which.max(boxcox(houses.step, plotit = F)$y)] 
L

houses.transform <- lm((Price^L - 1)/L ~ (Baths + Rooms + Age + Size)^2, data = houses)
houses.transform.step <- step(houses.transform, trace = 0)
summary(houses.transform)
summary(houses.transform.step)

autoplot(houses.transform.step)
ols_test_breusch_pagan(houses.transform.step)

houses.pca.corr <- princomp(houses[,c('Price', 'Rooms', 'Baths', 'Size')], cor = T)
biplot(houses.pca.corr)

corr.scree <- ggscreeplot(houses.pca.corr)
corr.scree

houses.cumplot <- cumplot(houses.pca.corr)
houses.cumplot
houses[88,]

setwd(datadir)
heart <- read.table('heart.csv', header = T, sep = ',')
setwd(sourcedir)

heart$cp <- as.factor(heart$cp)
heart$sex <- as.factor(heart$sex)

heart.lm1 <- glm(diag~age + cp + sex, data = heart, family = binomial)
summary(heart.lm1)
heart.null <- glm(diag~1, data = heart, family = binomial)
anova(heart.lm1, heart.null, test = 'Chi')

heart.lm2 <- glm(diag~age + cp + sex + restbps + chol + fbs, data = heart, family = binomial)
anova(heart.lm1, heart.lm2, test = 'Chi')

heart$restecg <- as.factor(heart$restecg)

heart.lm3 <- glm(diag~., data = heart, family = binomial)
summary(heart.lm3)
drop1(heart.lm3, response~., test = 'Chi', data = heart)

heart.step <- step(heart.lm3)
summary(heart.step)
drop1(heart.step, response~., test = 'Chi', data =heart)

smallHeart = heart[,c('age', 'sex', 'cp', 'restbps', 'chol', 'fbs', 'restecg')]

heart.step.pred <- predict(heart.step, type = 'response', newdata = smallHeart)
score.table(heart.step.pred, heart$diag, 0.5)

roc.plot.gg <- plot.roc.gg(heart.step.pred, heart$diag, 'Step model')
roc.plot.gg

heart$fbs
heart.pca <- princomp(heart[,c('age', 'restbps', 'chol')], cor = T)
heart.cumplot <- cumplot(heart.pca)
heart.cumplot

source("pc.glm.R")
heart.glm75 <- pc.glm(heart.pca, 75, heart$diag)
heart.pcanull <- pc.null(heart.pca, 75, heart$diag)

anova(heart.pcanull, heart.glm75, test = 'Chi')

AIC(heart.glm75)
AIC(heart.step)

heart.pca.pred <- predict.pc.glm(heart.glm75, heart.pca, heart[,c('age', 'restbps', 'chol')])
score.table(heart.pca.pred, heart$diag, 0.5)

setwd(datadir)
sunspot <- read.table("sunspot.csv", sep = ",", header = T)
setwd(sourcedir)

sunspot.ts = ts(sunspot$sunspotarea)
autoplot(sunspot.ts)

pg.sunspot <- spec.pgram(sunspot.ts, demean = T, log = 'no')
pg.sunspot.spans <- spec.pgram(sunspot.ts, spans = 12, demean = T, log = 'no')

spec.sunspot <- data.frame(freq=pg.sunspot$freq, spec=pg.sunspot$spec)
spec.sunspot.spans <- data.frame(freq=pg.sunspot.spans$freq, spec=pg.sunspot.spans$spec)

max.omega.sunspot <-pg.sunspot$freq[which(pg.sunspot$spec==max(pg.sunspot$spec))]
1/max.omega.sunspot

max.omega.sunspot.spans <- pg.sunspot.spans$freq[which(pg.sunspot.spans$spec==max(pg.sunspot.spans$spec))]
1/max.omega.sunspot.spans

time.temp = c(1:(length(sunspot.ts)))
sunspot.trend.season <- lm(sunspot.ts ~ time.temp + sin(2*pi*time.temp/10.7) + cos(2*pi*time.temp/10.7))
summary(sunspot.trend.season)
e.sunspot.trend.season <- ts(sunspot.trend.season$residuals)
autoplot(e.sunspot.trend.season)
acf(e.sunspot.trend.season)
pacf(e.sunspot.trend.season)

sunspot.auto <- auto.arima(e.sunspot.trend.season, approximation = FALSE)
summary(sunspot.auto)

tsdiag(sunspot.auto, gof.lag = 20)
