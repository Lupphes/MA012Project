library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")

dt <- read.csv2(file = "data/families.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$NumFamilies)

dt <- dt |> mutate(
	boys = factor(NumBoys), 
	N.j = NumFamilies, 
	p.j = dbinom(NumBoys, size = 5, prob = 0.5), 
	n.j = n * p.j
	)
View(dt)

# Musi se rovnat
sum(dt$N.j)
sum(dt$n.j)
sum(dt$N.j) == sum(dt$n.j)
# Musi byt jednicka
sum(dt$p.j) == 1

dt.long <- dt |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt.long, aes(x = boys, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "number of boys", y = "number of families")

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

# 2.
#	Assumption
dt$n.j >= 5 
#	First and last categories have low theoretical frequencies => merge first and last pairs of categories

d1 <- dt |> filter(NumBoys <= 1) |> 
	summarise(across(c(NumFamilies, N.j, p.j, n.j), sum)) |> 
	mutate(boys = "0-1", NumBoys = NA)
d2 <- dt |> filter(NumBoys >= 2 & NumBoys <= 3)
d3 <- dt |> filter(NumBoys >= 4) |> 
	summarise(across(c(NumFamilies, N.j, p.j, n.j), sum)) |> 
	mutate(boys = "4-5", NumBoys = NA)
dt2 <- do.call(rbind, list(d1, d2, d3))

View(dt2)
k2 <- nrow(dt2)

sum(dt2$N.j)
sum(dt2$n.j)
sum(dt2$p.j)

dt2.long <- dt2 |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt2.long, aes(x = boys, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "number of boys", y = "number of families")

#	Assumption
dt2$n.j >= 5 

K <- sum(dt2$N.j^2 / (dt2$n.j)) - n
K
qchisq (0.95, df = k2 - 1)
K >= qchisq (0.95, df = k2 - 1)
#	p-value
1 - pchisq (K, df = k2 - 1)
# built-in function
chisq.test (dt2$N.j, p = dt2$p.j)

