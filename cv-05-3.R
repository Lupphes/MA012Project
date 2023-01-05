library("dplyr")
library("ggplot2")

dt <- read.csv2(file = "data/trains.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$Days)

dt <- dt |> mutate(
	N.j = Days, 
	trains = seq(0, 7, by = 1), 
	lambda = (sum(trains * N.j) / n),
	p.j = dpois(trains, lambda),
	n.j = n * p.j
)
View(dt)

sum(dt$N.j)
sum(dt$n.j) # !
sum(dt$p.j) # ! 

# Vypocet pravdepodobnosti protoze to uplne neznam
dt[dt$trains == 7, "p.j"] <- dt |> filter(trains <= 6) |> summarise(p.j = 1 - sum(p.j)) |> pull(p.j)
dt[dt$trains == 7, "n.j"] <- dt |> filter(trains <= 6) |> summarise(n.j = n - sum(n.j)) |> pull(n.j)

View(dt)

sum(dt$N.j)
sum(dt$n.j)
sum(dt$p.j)
# OK

dt.long <- dt |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt.long, aes(x = Trains, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "number of trains per hour", y = "number of days")


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


###################################################################
# Specific test for Poisson distribution
X <- rep(dt$trains, dt$N.j)
# or, using "tidyverse" syntax
X <- dt |> select(trains, N.j) |> 
	tidyr::uncount(N.j) |> 
	pull(trains)

Q <- (n - 1) * var (X) / mean (X)
Q
q1 = qchisq (0.025, n - 1)
q2 = qchisq (0.975, n - 1)
c(q1, q2)
Q <= q1 | Q >= q2

