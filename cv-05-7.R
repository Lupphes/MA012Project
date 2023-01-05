library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")
library(data.table)

dt <- read.csv2(file = "data/dice2.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$Frequency)

dt <- dt |> mutate(
  Number6 = seq(0, 7, by = 1))
dt <- dt |> mutate(
  N.j = Frequency, 
  p.j = dbinom(Number6, size = 12, prob = 1/6), 
  n.j = n * p.j
)
View(dt)

# Vypocet pravdepodobnosti protoze to uplne neznam
dt[dt$Number6 == 7, "p.j"] <- dt |> filter(Number6 <= 6) |> summarise(p.j = 1 - sum(p.j)) |> pull(p.j)
dt[dt$Number6 == 7, "n.j"] <- dt |> filter(Number6 <= 6) |> summarise(n.j = n - sum(n.j)) |> pull(n.j)

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
# Nezamitam
