library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

#	Read data
dat <- read.csv2(file = "data/potatoes.csv")
head(dat)
View(dat)
# Structure
str(dat)
summary(dat)
# Note the data types
# categorical variable => factor
dat <- dat |> 
  mutate(Variety = factor(Variety))
str(dat)
summary(dat)
# Note different data types
dat$Variety
levels(dat$Variety)
as.character(dat$Variety)
as.numeric(dat$Variety)

# Plots
ggplot(dat, aes(x = Variety, y = Weight, colour = Variety)) + 
	geom_jitter(width = 0.1, size = 2) + 
	labs(x = "potato variety", y = "weight [kg]")

# Frequency table
table(dat$Variety)
freq <- dat |> 
	group_by(Variety) |> 
	summarise(n = n())
freq

# Frequency barplots
ggplot(dat, aes(x = Variety, fill = Variety)) + 
	geom_bar(colour = "black", size = 0.3) + 
	labs(x = "potato variety", y = "number of samples")
# or
ggplot(freq, aes(x = Variety, y = n, fill = Variety)) + 
	geom_col(colour = "black", size = 0.3) + 
	labs(x = "potato variety", y = "number of samples")

# Descriptive statistics for each potato variety
stat <- dat |> 
	group_by(Variety) |> 
	summarise(
		mean = mean(Weight), 
		stdev = sd(Weight),
		variance = var(Weight),
		median = median(Weight), 
	  lowest = min(Weight), 
	  highest = max(Weight), 
	  iqr = IQR(Weight)
		)
stat
# or
stat <- dat |> 
	group_by(Variety) |> 
	summarise(across(Weight,
		list(mean = mean, stdev = sd, variance = var, median = median, lowest = min, highest = max, iqr = IQR), 
	  .names = "{.fn}"
		))
stat
stat |> as.data.frame()

#	Plots
ggplot(dat, aes(x = Variety, y = Weight, fill = Variety)) + 
	geom_boxplot() + 
	geom_jitter(width = 0.1) + 
	labs(x = "potato variety", y = "weight [kg]")

ggplot(stat, aes(x = Variety, ymin = mean - qnorm(0.975) * stdev, ymax = mean + qnorm(0.975) * stdev, y = mean, colour = Variety)) + 
	geom_errorbar(width = 0.1) + 
	geom_point(shape = 16, size = 3) + 
	geom_point(aes(y = median), shape = 2, size = 3) + 
	labs(x = "potato variety", y = "weight [kg]")

# Homogeneity tests
bartlett.test(Weight ~ Variety, data = dat) # or
dat |> bartlett.test(Weight ~ Variety, data = _)
leveneTest(Weight ~ Variety, data = dat) # or
dat |> leveneTest(Weight ~ Variety, data = _)

# Normal QQ plots
ggplot(dat, aes(sample = Weight, colour = Variety)) + 
	geom_qq(distribution = stats::qnorm) + 
	geom_qq_line(distribution = stats::qnorm) + 
	labs(x = "theoretical gaussian quantiles", y = "empirical quantiles") + 
	facet_grid(~ Variety)
	# facet_wrap(~ Variety, ncol = 2)

# Normality tests
dat |>
	group_by(Variety) |>
	summarise(
		# Lilliefors.p.value = lillie.test(Weight)$p.value, # comment if cannot be evaluated
		Shapiro.p.value = shapiro.test(Weight)$p.value
		)

# ANOVA using "aov" function
aov.model <- aov(Weight ~ Variety, data = dat) # or
aov.model <- dat |> aov(Weight ~ Variety, data = _)
# ANOVA table
summary(aov.model)
# Design matrix
model.matrix(aov.model)
# Coefficients of the linear regression model
aov.model$coefficients
# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

# Details for the omnibus F-test
w <- summary(aov.model)
w[[1]]$Df # degrees of freedom
w[[1]]$"F value" # actual value of the test statistic
d.fn <- data.frame(x = seq(0, 11, by = 0.01)) |> 
  mutate(f = df(x, w[[1]]$Df[1], w[[1]]$Df[2]))
ggplot(d.fn, aes(x = x, y = f)) + 
  geom_line(size = 1.0) + 
  geom_vline(xintercept = qf(0.95, w[[1]]$Df[1], w[[1]]$Df[2]), linetype = "dashed") + 
  geom_vline(xintercept = w[[1]]$"F value"[1], color = "red", linetype = "dashed") + 
  labs(x = "x", y = "p.d.f. of F distribution")

# Mutiple comparison

# Tukey's method
TukeyTest <- TukeyHSD(aov.model)
TukeyTest

TukeyTest.table <- TukeyTest$Variety |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "groups") |> 
	rename("p.value" = "p adj") |> 
	mutate(significant = factor(p.value < 0.05))
ggplot(TukeyTest.table, aes(x = groups, y = diff, ymin = lwr, ymax = upr, colour = significant)) + 
  geom_hline(yintercept = 0, color = "black", linetype ="dashed", size = 0.5) + 
	geom_errorbar(width = 0.1) + 
	geom_point(size = 2) + 
	labs(x = "potato varieties", y = "difference in weights [kg]")

# Scheffe's method
ScheffeTest <- scheffe.test(aov.model, "Variety")
ScheffeTest
ScheffeTest$groups

###########################################
# Different approach, using lm and anova

# Linear regression model
M.A <- lm(Weight ~ Variety, data = dat)
# Results
summary(M.A)
# Coefficients of the linear regression model
M.A$coefficients
# Design matrix
model.matrix(M.A)
# Means
predict(M.A, newdata = data.frame(Variety = levels(dat$Variety)))
# ANOVA using results of the regresion model
anova.model <- anova(M.A)
# ANOVA table
anova.model

###########################################
# Compare full and null model

M.0 <- lm(Weight ~ 1, data = dat)
# model.matrix(M.A)
# model.matrix(M.0)
anova(M.A, M.0)

