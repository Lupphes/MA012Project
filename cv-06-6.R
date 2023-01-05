library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("rgl")			  	# 3D graphics
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")

dt <- read.csv2("data/decathlon.csv", header = TRUE)
str(dt)
summary(dt)
M <- as.matrix(dt)
View(dt)


#	Scatterplot
#GGally::ggpairs(dt, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

#	Mutiple correlation between expense and (income, members)
model.M <- lm(Points ~ X100m + Long.jump + Shot.put + High.jump + Discus + Pole.vault + Javeline + X1500m, data = dt)
cor(dt$Points, fitted.values(model.M)) # by definition, as correlation between variable and its best linear approximation
sqrt(summary(model.M)$r.squared)        # or, as square root of R squared
