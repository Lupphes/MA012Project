library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")
library("devtools")
# install_github("vqv/ggbiplot")
library("ggbiplot")

dt <- read.csv2 (file = "data/heart.csv")
str(dt)
View(dt)

dt <- dt |> mutate(
  chd = as.factor(chd)
)
View(dt)

model.logit <- glm(chd ~ age, data = dt, family = binomial(link = "logit"))
summary(model.logit)


# probit link 
model.probit <- glm(chd ~ age, data = dt, family = binomial(link = "probit"))
summary(model.probit)


model.CLogLog <- glm(chd ~ age, data = dt, family = binomial(link = "cloglog"))
summary(model.CLogLog)