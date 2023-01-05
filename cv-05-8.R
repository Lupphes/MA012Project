library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")
library(data.table)

dt <- read.csv2(file = "data/pond.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$NumFish)

dt <- dt |> mutate(
  Light = factor(Light), 
  N.j = NumFish, 
  p.j = 1 / 5,
  n.j = n * p.j
)
View(dt)

# Musi se rovnat
sum(dt$N.j)
sum(dt$n.j)
# Musi byt jednicka
sum(dt$p.j)

# Assumptions
dt$n.j >= 5
# 1. 
#	Yarnold's criterion
# testovani ze kategorie jsou spravne
q <- sum (dt$n.j < 5) / k
q
dt$n.j >= 5 * q
#	OK


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
# Zamitam