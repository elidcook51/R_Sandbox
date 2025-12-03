#****************************************************************************
#
#  In-Class Diabetic Retinopathy Logistic Regression Classification Contest
#   		   
#****************************************************************************

#****************************************************************************
#
#  Read in the data
#
#****************************************************************************

# Set working directory
sourcedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"
datadir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/Contest"
setwd(sourcedir)

# Read in the data
DR.train <- read.csv(paste(datadir,"/DR_train.csv", sep=""), sep = ",", header = T)
DR.test <- read.csv(paste(datadir,"/DR_test.csv", sep=""), sep = ",", header = T)


# Source potentially useful functions
source("pc.glm.R")
source("PCAplots.R")
source("FactorPlots.R")
source("ROC.R")
source("TestSet.R")

#****************************************************************************
#
# Build the best GLM you can to classify images as DR (1)
# or not DR (0) and write classifications to file
#
#****************************************************************************
set.seed(123)
Train = test.set(DR.train,.33)
mainEffects <- glm(dr~.,data = Train$train, family = binomial)
mainEffects.pred <- predict(mainEffects, type = "response", newdata = Train$test)

mainEffects.step = step(mainEffects, trace = F)
mainEffect.step.pred <- predict(mainEffects.step, type = "response", newdata = Train$test)

Ltrain <- log(Train$train[,-c(1,2,19,20)] + .1)
Ltrain$quality <- Train$train$quality
Ltrain$pre.screening <- Train$train$pre.screening
Ltrain$am_fm_classification <- Train$train$am_fm_classification
Ltrain$dr <- Train$train$dr

Ltest <- log(Train$test[,-c(1,2,19,20)] + .1)
Ltest$quality <- Train$test$quality
Ltest$pre.screening <- Train$test$pre.screening
Ltest$am_fm_classification <- Train$test$am_fm_classification
Ltest$dr <- Train$test$dr

Lmodel <- glm(dr~.,data = Ltrain, family = binomial)
Lmodel.pred <- predict(Lmodel, type = "response", newdata = Ltest)

Lmodel.step <- step(Lmodel, trace = F)
Lmodel.step.pred <- predict(Lmodel.step, type = "response", newdata = Ltest)

secondOrder <- glm(dr~(.^2), data = Train$train, family = binomial)
secondOrder.pred <- predict(secondOrder, type = "response", newdata = Train$test)

secondOrder.step = step(secondOrder, trace = F)
secondOrder.step.pred <- predict(secondOrder.step, type = "response", newdata = Train$test)

elismodel <- glm(dr~ma1 + ma4 + ma6 + quality + pre.screening + exudate1 + exudate4 + exudate7 + opticdisc_diameter + macula_opticdisc_distance, data = Ltrain)
elismodel.pred <- predict(elismodel, type = 'response', newdata = Ltest)
            
elismodel.step = step(elismodel)
elismodel.step.pred <- predict(elismodel.step, type = 'response', newdata = Ltest)

roc.plot.gg <- plot.roc.gg(mainEffects.pred, Train$test[,20], 'mainEffects')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, mainEffect.step.pred, Train$test[,20], 'mainEffectsStepped')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, Lmodel.pred, Train$test[,20], 'Log')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, Lmodel.step.pred, Train$test[,20], 'Log step')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, secondOrder.pred, Train$test[,20], "second order")
#roc.plot.gg <- lines.roc.gg(roc.plot.gg, secondOrder.step.pred, Train$test[,20], 'second order step')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, elismodel.pred, Train$test[,20], 'elismodel')
roc.plot.gg <- lines.roc.gg(roc.plot.gg, elismodel.step.pred, Train$test[,20], 'elismodelstep')
roc.plot.gg


# Build model to training set:
DR.glm <- glm(dr~., DR.train, family = binomial)

# Predict probabilities on testing set:
DR.pred <- predict(DR.glm, type = "response", newdata = DR.test)

# Convert probabilities to 0/1 classification based on threshold T
T <- 0.5
DR.classifications <- matrix(0,nrow=length(DR.pred),ncol=2)
DR.classifications[,1] <- c(1:length(DR.pred))
DR.classifications[which(DR.pred > T),2] <- 1

# compute F1 score
DR.fit <- predict(DR.glm, type="response")
calcF1 = function(p, r, threshold){
  Pred <- p > threshold
  Actual <- r
  table <- table(Actual,Pred)
  tp <- table[2,2]
  fp <- table[1,2]
  fn <- table[2,1]
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  F1 <- 2*precision*recall / (precision + recall)
}
F1 = calcF1(DR.fit, DR.train$dr, T)

# Write predictions to file
colnames(DR.classifications) = c("Id","Predicted")
write.csv(DR.classifications, paste(datadir,"/sample_submission.csv", sep=""), row.names=FALSE)
