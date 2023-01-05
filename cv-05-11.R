library("dplyr")
library("ggplot2")
library("nortest")

dt <- read.csv2(file = "data/pollution.csv")
str(dt)
summary(dt)

# JANT
dt <- dt |> mutate(
	X = (JANT - 32) * 5 / 9
	)

n <- nrow(dt)

# Lilliefors test
lillie.test(dt$X)
# estimates of mean and standard deviation of normal distribution
m <- mean(dt$X)
s <- sd(dt$X)
# Kolmogorov-Smirnov test
ks.test(dt$X, "pnorm", mean = m, sd = s)


# Histogram
sqrt(n)
k <- 8
ggplot(dt, aes(x = X, y = ..density..)) + 
	geom_histogram(bins = k, color = "black", fill = "#FFCC00") + 
	labs(x = "January temperature [degrees Celsius]", y = "density")

ggplot(dt, aes(x = X)) + 
	geom_histogram(aes(y = ..density..), bins = 8, color = "black", fill = "#FFCC00") + 
	geom_function(fun = dnorm, args = list(mean = m, sd = s), color = "red") + 
	labs(x = "January temperature [degrees Celsius]", y = "density")

# ECDF plot
ggplot(dt, aes(x = X)) + 
	stat_ecdf(color = "black") + 
	labs(x = "January temperature [degrees Celsius]", y = "ECDF")

ggplot(dt, aes(x = X)) + 
	stat_ecdf(color = "black") + 
	geom_function(fun = pnorm, args = list(mean = m, sd = s), color = "red") + 
	labs(x = "January temperature [degrees Celsius]", y = "ECDF")


# Chi-squared test
range(dt$X)
breaks <- seq(-12, 20, len = k + 1)
breaks[1] = -Inf
breaks[length(breaks)] = Inf
dt <- dt |> mutate(
		A.j = cut(X, breaks, ordered_result = TRUE)
	)

dt2 <- dt |> group_by(A.j) |> 
	summarise(
		N.j = n()
	) |> 
	mutate(
		left = breaks[-length(breaks)], 
		right = breaks[-1], 
		F.right = pnorm(right, mean = m, sd = s), 
		F.left = pnorm(left, mean = m, sd = s), 
		p.j = F.right - F.left, 
		n.j = n * p.j
	)

sum(dt2$N.j)
sum(dt2$n.j)
sum(dt2$p.j)
# OK

k <- nrow(dt2)

dt2.long <- dt2 |> tidyr::pivot_longer(cols = c(N.j, n.j), names_to = "type", values_to = "freq")
ggplot(dt2.long, aes(x = A.j, y = freq, fill = type)) + 
	geom_col(position = "dodge", color = "black") + 
	labs(x = "January temperature [degrees Celsius]", y = "number")

# Assumptions
dt2$n.j >= 5 
# Yarnold's criterion
q <- sum (dt2$n.j < 5) / k
q
dt2$n.j >= 5 * q
# => merge last trwo categories

d1 <- dt2 |> filter(right <= 12)
d2 <- dt2 |> filter(right > 12) |> 
	summarise(across(c(N.j, p.j, n.j), sum)) |> 
	mutate(A.j = "(12, Inf)", left = 12, right = Inf, F.right = 1, F.left = pnorm(12, mean = m, sd = s))
dt3 <- d1 |> rbind(d2)

View(dt2)
k3 <- nrow(dt3)

sum(dt3$N.j)
sum(dt3$n.j)
sum(dt3$p.j)
# OK

# Assumptions
dt3$n.j >= 5 
# Yarnold's criterion
q <- sum (dt3$n.j < 5) / k
q
dt3$n.j >= 5 * q
# Yarnold's criterion is OK

K <- sum(dt3$N.j^2 / (dt3$n.j)) - n
K
qchisq (0.95, df = k3 - 1 - 2) # remember -2
K >= qchisq (0.95, df = k3 - 1 - 2) # remember -2
#	p-value
1 - pchisq (K, df = k3 - 1 - 2) # remember -2

#
# Continue with June temperatures ...


