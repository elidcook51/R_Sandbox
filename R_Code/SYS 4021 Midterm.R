csvdir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/app.csv"
sourcedir <-"C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/R_Code"


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
setwd(sourcedir)
source('AccidentInput.R')
source('PCAplots.R')

data <- read.table(csvdir, sep = ",", header = T)
summary(data)

boxplot = ggplot(data, aes(x = Price)) + 
  geom_boxplot()
outliers = ggplot_build(boxplot)$data[[1]]$outliers
outliers

maxCost = max(data$Price)
maxCost
filter(data, Price == maxCost)$Develop

pairs.panels(data)

data$advertYes = as.factor(data$Advert)
Price.lm.main1 = lm(Price ~ CompSites + Date + advertYes, data = data)
summary(Price.lm.main1)

Price.lm.interact1 = lm(Price ~ (CompSites + Date + advertYes)^2, data = data)
summary(Price.lm.interact1)

anova(Price.lm.main1, Price.lm.interact1)

AIC(Price.lm.main1)

data$platform = as.factor(data$Platform)
Price.lm.fullmain = lm(Price ~ platform + Develop + X5Star + CompSites + Date + advertYes + Users, data = data)
summary(Price.lm.fullmain)

library(car)
vif(Price.lm.fullmain)

ols_test_breusch_pagan(Price.lm.fullmain)

autoplot(Price.lm.fullmain, which = 4)
data$Platform[5]
data$Price[5]

boxcox(Price.lm.fullmain)

logPrice.lm.fullmain = lm(log(Price) ~ platform + Develop + X5Star + CompSites + Date + advertYes + Users, data = data)
summary(logPrice.lm.fullmain)
AIC(logPrice.lm.fullmain)

logPrice.lm.step = step(logPrice.lm.fullmain)
AIC(logPrice.lm.step)
summary(logPrice.lm.step)

app.pca = princomp(data[,c('Develop', 'X5Star', 'CompSites', 'Date', 'Users', 'Price')], cor = T)
app.pca$loadings

cumsum = cumplot(app.pca)
cumsum$cumvar

countryDataDir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/Country-data.csv"
countryData = read.table(countryDataDir, sep = ",", header = T)

boxplot = ggplot(countryData, aes(x = child_mort)) + geom_boxplot()
outliers = ggplot_build(boxplot)$data[[1]]$outliers
outliers
filter(countryData, child_mort %in% c(149, 150, 208, 160))$country
lowestChild = min(countryData$child_mort)
filter(countryData, child_mort == lowestChild)$country
pairs.panels(countryData[,c('child_mort', 'exports', 'health', 'imports', 'income', 'inflation', 'life_expec', 'gdpp', 'total_fer')])

country.pca = princomp(countryData[,c('child_mort', 'exports', 'health', 'imports', 'income', 'inflation', 'life_expec', 'gdpp', 'total_fer')], cor = T)
cumsum = cumplot(country.pca)
cumsum$cumvar

biplot(country.pca)
countryData$child_mort[134]
