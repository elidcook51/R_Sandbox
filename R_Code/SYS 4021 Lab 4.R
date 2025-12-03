housedir <- "C:/Users/ucg8nb/OneDrive - University of Virginia/SYS 4021/Data/housing.csv"
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

setwd(sourcedir)
source('AccidentInput.R')
source('PCAplots.R')

housing = read.csv(housedir)

boxplot = ggplot(housing, aes(x = price)) +
  geom_boxplot()
upper = ggplot_build(boxplot)$data[[1]]$xmax
upper

pairs.panels(housing[,c('price', 'sqft', 'bedrooms', 'baths')])

house.main = lm(price ~ sqft + bedrooms + baths, data = housing)
summary(house.main)

house.inter = lm(price ~ (sqft + bedrooms + baths)^2, data = housing)
summary(house.inter)

anova(house.main, house.inter)
AIC(house.inter)

autoplot(house.main, which = 1)

ols_test_breusch_pagan(house.main)
boxcox(house.main)

house.city = lm(price ~ City, data = housing)
summary(house.city)

house.pca.cor = princomp(housing[,c('price', 'sqft', 'bedrooms', 'baths')], cor= T)
house.pca.cor$loadings

cumsum = cumplot(house.pca.cor)
cumsum$cumvar

biplot(house.pca.cor)
housing$sqft[64]
