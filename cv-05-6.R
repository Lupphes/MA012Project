library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")
library(data.table)

dt <- read.csv2(file = "data/emergency.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$Frequency)

dt <- dt |> mutate(
  N.j = Frequency, 
  lambda = (sum(NumPatients * N.j) / n),
  p.j = dpois(NumPatients, lambda),
  n.j = n * p.j
)

sum(dt$N.j)
sum(dt$n.j)
sum(dt$p.j)

dt[dt$NumPatients == 10, "p.j"] <- dt |> filter(NumPatients <= 9) |> summarise(p.j = 1 - sum(p.j)) |> pull(p.j)
dt[dt$NumPatients == 10, "n.j"] <- dt |> filter(NumPatients <= 9) |> summarise(n.j = n - sum(n.j)) |> pull(n.j)


sum(dt$N.j)
sum(dt$n.j)
sum(dt$p.j)


d2 <- dt |> filter(NumPatients >= 0 & NumPatients <= 8)
d3 <- dt |> filter(NumPatients >= 9) |> 
  summarise(across(c(Frequency, N.j, p.j, n.j), sum)) |> 
  mutate(NumPatients = NA, lambda=dt$lambda[1])
View(d2)
View(d3)

dt2 <- do.call(rbind, list(d2, d3))
View(dt)
View(dt2)

sum(dt2$N.j)
sum(dt2$n.j)
sum(dt2$p.j)

# Assumptions
dt2$n.j >= 5
# 1. 
#	Yarnold's criterion
# testovani ze kategorie jsou spravne
q <- sum (dt2$n.j < 5) / k
q
dt2$n.j >= 5 * q

k2 <- nrow(dt2)

K <- sum(dt2$N.j^2 / (dt2$n.j)) - n
K
qchisq (0.95, df = k2 -1 - 1)
K >= qchisq (0.95, df = k2 - 1 - 1)
#	p-value
1 - pchisq (K, df = k2 - 1)
# built-in function
chisq.test (dt2$N.j, p = dt2$p.j)

###################################################################
# Specific test for Poisson distribution
X <- rep(dt$NumPatients, dt$N.j)
# or, using "tidyverse" syntax
X <- dt |> select(NumPatients, N.j) |> 
  tidyr::uncount(N.j) |> 
  pull(NumPatients)

Q <- (n - 1) * var (X) / mean (X)
Q
q1 = qchisq (0.025, n - 1)
q2 = qchisq (0.975, n - 1)
c(q1, q2)
Q <= q1 | Q >= q2



