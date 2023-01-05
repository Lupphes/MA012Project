
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




#########
test <- c(13, 16, 12,  21,  0,  6,  3,  1,  2,  5) 

wilcox.test(test, mu = 10) # 60 sekund timer, na co se ptalia sloupec byl X
##########


dt <- read.csv2(file = "data/tyty.csv")
View(dt)

dt <- dt |> mutate(
  G = as.factor(G)
)

# ANOVA
aov.model <- aov(Y ~ G, data = dt)                 #|| Muney
# ANOVA using "aov" function
aov.model <- aov(Y ~ G, data = dt)
# ANOVA table
summary(aov.model)
# Design matrix
model.matrix(aov.model)
# Coefficients of the linear regression model
aov.model$coefficients
# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")


TukeyTest <- TukeyHSD(aov.model)
TukeyTest
###########


dt <- read.csv2(file = "data/lala.csv")
View(dt)

k <- nrow(dt)
n <- sum(dt$pocet) # Soucet celku #|| Frequency

dt <- dt |> mutate(
  pravy = as.factor(pravy))

dt <- dt |> mutate(  #| Light, faktoruju kaminky
  N.j = pocet,                     #| NumFish, zase moje frekvence
  p.j = 1 / 10,                             # Tohle musim vymyslet
  n.j = n * p.j
)

# Musi se rovnat
sum(dt$N.j)
sum(dt$n.j)
# Musi byt jednicka
sum(dt$p.j)


# Pearson’s chi-squared test (Pearsonův test dobré shody)
K <- sum(dt$N.j^2 / (dt$n.j)) - n
K
# χ2_0.95
qchisq (0.95, df = k - 1)
K >= qchisq (0.95, df = k - 1)
#	p-value
1 - pchisq (K, df = k - 1)
# built-in function

# Cela funkce
chisq.test (dt$N.j, p = dt$p.j)
# Nezamitam