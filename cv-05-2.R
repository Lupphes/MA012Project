library("dplyr")
library("tidyr")
library("ggplot2")
library("GGally")

dt <- read.csv2(file = "data/line.csv")
str(dt)
summary(dt)
View(dt)

k <- nrow(dt)
n <- sum(dt$NumCustomers)
categories <- dt$WaitingTime

dt <- dt |> mutate(
	time = factor(WaitingTime, labels = categories, levels = categories, ordered = TRUE), # ordered factor
	time.from = seq(0, 21, by = 3),
	time.to = seq(3, 24, by = 3),
	time.avg = (time.from + time.to) / 2, 
	N.j = NumCustomers, 
	lambda = 1 / (sum (time.avg * N.j) / n), 
	p.j = pexp(time.to, lambda) - pexp(time.from, lambda),
	n.j = n * p.j
	)
View(dt)

sum(dt$N.j)
sum(dt$n.j) # ! 
sum(dt$p.j) # ! 

dt.long <- dt |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt.long, aes(x = time, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "waiting time", y = "number of customers")

d0 <- data.frame(
		time = factor("(24; Inf)"), 
		WaitingTime = NA, 
		NumCustomers = 0, 
		N.j = 0, 
		time.from = 24, 
		time.to = Inf, 
		time.avg = Inf, 
		lambda = dt$lambda[1]) |> 
	mutate(
		p.j = 1 - pexp(24, lambda),
		n.j = n * p.j
	)

dt <- dt |> rbind(d0)

sum(dt$N.j)
sum(dt$n.j)
sum(dt$p.j)
# OK

dt.long <- dt |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt.long, aes(x = time, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "waiting time", y = "number of customers")

# Assumptions
dt$n.j >= 5 
# Yarnold's criterion
q <- sum (dt$n.j < 5) / k
q
dt$n.j >= 5 * q

# => merge, e.g, the last 2 categories
d1 <- dt |> filter(time.avg <= 19.5)
d2 <- dt |> filter(time.avg >= 22.5) |> 
	summarise(across(c(NumCustomers, N.j, p.j, n.j), sum)) |> 
	mutate(WaitingTime = "(21; Inf)", time = "(21; Inf)", time.from = 21, time.to = Inf, time.avg = Inf, lambda = dt$lambda[1])
dt2 <- d1 |> rbind(d2)

View(dt2)
k2 <- nrow(dt2)

sum(dt$N.j)
sum(dt$n.j)
sum(dt$p.j)
# OK

dt2.long <- dt2 |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt2.long, aes(x = time, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "waiting time", y = "number of customers")

# Assumptions
dt2$n.j >= 5 
# Yarnold's criterion
q <- sum (dt2$n.j < 5) / k
q
dt2$n.j >= 5 * q
# Yarnold's criterion is OK

K <- sum(dt2$N.j^2 / (dt2$n.j)) - n
K
qchisq (0.95, df = k2 - 1 - 1) # remember -1
K >= qchisq (0.95, df = k2 - 1 - 1) # remember -1
#	p-value
1 - pchisq (K, df = k2 - 1 - 1) # remember -1


###################################################################
# Specific test for exponential distribution
X <- rep(dt$time.avg, dt$N.j)
# or, using "tidyverse" syntax
X <- dt |> select(time.avg, N.j) |> 
	tidyr::uncount(N.j) |> 
	pull(time.avg)

Q <- (n - 1) * var(X) / (mean(X))^2 
Q
q1 = qchisq (0.025, n - 1)
q2 = qchisq (0.975, n - 1)
c(q1, q2)
Q <= q1 | Q >= q2

