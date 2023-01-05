library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")
library(data.table)

dt <- read.csv2(file = "data/dice.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$Frequency)
dt$values <- c(seq(1,6))

sum_rolls <- sum(dt$values)

View(dt)

dt <- dt |> mutate(
  days_fact = factor(values), 
  N.j = Frequency, 
  p.j = 1 / 6,
  n.j = n * p.j
)
View(dt)


sum(dt$N.j)
sum(dt$n.j)
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
K <- sum(dt$N.j^2 / (dt$n.j)) - n
K
qchisq (0.95, df = k - 1)
K >= qchisq (0.95, df = k - 1)
# True = zamitame H0

# Cela funkce
chisq.test (dt$N.j, p = dt$p.j)

