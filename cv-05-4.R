library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")
library(data.table)

dt <- read.csv2(file = "data/Brno.csv")
str(dt)
summary(dt)
View(dt)

dt$days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

View(dt)

k <- nrow(dt)
n <- sum(dt$Frequency)

days_num <- sum(dt$days)

dt <- dt |> mutate(
  days_fact = factor(days), 
  N.j = Frequency, 
  p.j = days / days_num,
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

