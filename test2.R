
# Nacitat vzdycky
library("dplyr")
library("tidyr")
library("ggplot2")
library("Hmisc")		  #	rcorr
library("ppcor")		  #	pcor
library("rgl")			  # 3D graphics
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("ggplot2")
library(data.table)
library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")


dt <- read.csv2(file = "data/heptathlon.csv")
View(dt)

model <- lm(Pct.BF ~ ., data = dt)
summary(model)

model.back <- step(model, direction = "backward", trace = 1)
summary(model.back)


dt <- read.csv2(file = "data/heptathlon.csv")
View(dt)

str(dt)
summary(dt)
M <- as.matrix(dt)
View(dt)

#	Scatterplot
GGally::ggpairs(dt, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

#	Mutiple correlation between expense and (income, members)
model.M <- lm(hurdles ~ ., data = dt)
cor(dt$score, fitted.values(model.M)) # by definition, as correlation between variable and its best linear approximation
sqrt(summary(model.M)$r.squared)        # or, as square root of R squared

