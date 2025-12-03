sourcedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"
datadir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/Spam"

setwd(sourcedir)
spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

#*****************************************
# Source code
#*****************************************

source("PCAplots.R")
source("FactorPlots.R")

library(ggplot2)
library(ggpubr)
library(psych)

source("pc.glm.R")
source("PCAplots.R")
source("ROC.R")
source("TestSet.R")

library(ggplot2)
library(ggpubr)
library(ggfortify)
library(ggResidpanel)

sum(spam[,58])

mainEffects = glm(V58 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57, data = Lspam, family = binomial)

nullModel = glm(V58~1, data = Lspam, family = binomial)
summary(mainEffects)
anova(nullModel, mainEffects, test = 'Chi')

library(MASS)

drop1(mainEffects, response~., test = 'Chi', data = Lspam)

mainEffects.pred = predict(mainEffects, type = 'response')

score.table(mainEffects.pred, Lspam[,58], 0.5)

stepModel = step(mainEffects, trace = 0)
summary(stepModel)

anova(stepModel, mainEffects, test = 'Chi')

AIC(mainEffects)
AIC(stepModel)

stepModel.pred = predict(stepModel, type = 'response')

score.table(stepModel.pred, Lspam[,58], 0.5)

roc.plot.gg <- plot.roc.gg(mainEffects.pred, Lspam[,58], 'Main Effects')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, stepModel.pred, Lspam[,58], 'Step Model')
roc.plot.gg

Lspam.sub <- Lspam[,c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V48", "V49", "V50", "V51", "V52", "V53", "V54", "V55", "V56", "V57")]
Lspam.sub.pc <- princomp(Lspam.sub, cor = T)
cumplot(Lspam.sub.pc)


Lspampca.glm98 <- pc.glm(Lspam.sub.pc, 98, Lspam[,58])
summary(Lspampca.glm98)

Lspam.pca.pred <- predict(Lspampca.glm98, type = 'response')

score.table(Lspam.pca.pred, Lspam[,58], 0.5)
score.table(stepModel.pred, Lspam[,58], 0.5)
score.table(mainEffects.pred, Lspam[,58], 0.5)

summary(mainEffects)
