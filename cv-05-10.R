library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")
library(data.table)

dt <- read.csv2(file = "data/football.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$Matches)

dt <- dt |> mutate(
  N.j = Matches, 
  Goals = seq(0, 4, by = 1), 
  lambda = (sum(Goals * N.j) / n),
  p.j = dpois(Goals, lambda),
  n.j = n * p.j
)
View(dt)



# Vypocet pravdepodobnosti protoze to uplne neznam
dt[dt$Goals == 4, "p.j"] <- dt |> filter(Goals <= 3) |> summarise(p.j = 1 - sum(p.j)) |> pull(p.j)
dt[dt$Goals == 4, "n.j"] <- dt |> filter(Goals <= 3) |> summarise(n.j = n - sum(n.j)) |> pull(n.j)

# Musi se rovnat
sum(dt$N.j)
sum(dt$n.j)
# Musi byt jednicka
sum(dt$p.j)


# Assumptions
dt$n.j >= 5 
# Yarnold's criterion
q <- sum (dt$n.j < 5) / k
q
dt$n.j >= 5 * q
# Yarnold's criterion is OK

K <- sum(dt$N.j^2 / (dt$n.j)) - n
K
qchisq (0.95, df = k - 1 - 1) # remember -1
K >= qchisq (0.95, df = k - 1 - 1) # remember -1
#	p-value
1 - pchisq (K, df = k - 1 - 1) # remember -1
# NEZAMITAM
