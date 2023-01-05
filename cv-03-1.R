library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- read.csv2(file = "data/hay.csv", stringsAsFactors = TRUE)
View(dat)
dat <- dat |> mutate(group = Soil:Fertilizer)
str(dat)
summary(dat)
View(dat)

ggplot(dat, aes(color = Soil:Fertilizer, y = Yield, x = Soil:Fertilizer)) + 
	geom_jitter(width = 0.1, size = 2) + 
	labs(x = "Soil:Fertilizer", y = "Yield [tons per hectare]")

table(dat$Soil, dat$Fertilizer)
freq <- dat |> 
	group_by(Soil, Fertilizer) |> 
	summarise(n = n())
freq

stat <- dat |> 
	group_by(Soil, Fertilizer) |> 
	summarise(across(Yield,
		list(mean = mean, stdev = sd, variance = var, median = median, lowest = min, highest = max, iqr = IQR), 
	  .names = "{.fn}"
		))
stat
stat |> as.data.frame()

ggplot(dat, aes(x = Soil:Fertilizer, y = Yield, fill = Soil:Fertilizer)) + 
	geom_boxplot() + 
	geom_jitter(width = 0.1) + 
	labs(x = "Soil:Fertilizer", y = "Yield [tons per hectare]")

ggplot(stat, aes(x = Soil:Fertilizer, ymin = mean - qnorm(0.975) * stdev, ymax = mean + qnorm(0.975) * stdev, y = mean, colour = Soil:Fertilizer)) + 
	geom_errorbar(width = 0.1) + 
	geom_point(shape = 16, size = 3) + 
	geom_point(aes(y = median), shape = 2, size = 3) + 
	labs(x = "Soil:Fertilizer", y = "Yield [tons per hectare]")

ggplot(dat, aes(x = Soil, y = Yield, fill = Soil)) + 
	geom_boxplot() + 
	geom_jitter(width = 0.1) + 
	labs(x = "Soil", y = "Yield [tons per hectare]")

ggplot(dat, aes(x = Fertilizer, y = Yield, fill = Fertilizer)) + 
	geom_boxplot() + 
	geom_jitter(width = 0.1) + 
	labs(x = "Fertilizer", y = "Yield [tons per hectare]")

# Levene’s test is used to check that variances are equal for all samples when your data comes from a non normal distribution. 
bartlett.test(Yield ~ group, data = dat)
leveneTest(Yield ~ group, data = dat)

ggplot(dat, aes(sample = Yield, colour = group)) + 
	geom_qq(distribution = stats::qnorm) + 
	geom_qq_line(distribution = stats::qnorm) + 
	labs(x = "theoretical gaussian quantiles", y = "empirical quantiles") + 
	facet_grid(Soil ~ Fertilizer)

dat |>
	group_by(group) |>
	summarise(
		#Lilliefors.p.value = lillie.test(Yield)$p.value, # comment if cannot be evaluated
		Shapiro.p.value = shapiro.test(Yield)$p.value
		)

# Two-way ANOVA without interactions
aov.model <- aov(Yield ~ Soil + Fertilizer, data = dat)
summary(aov.model)
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$Soil
TukeyTest$Fertilizer

TukeyTest.table <- TukeyTest$Soil |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "groups") |> 
	rename("p.value" = "p adj") |> 
	mutate(significant = factor(p.value < 0.05))
ggplot(TukeyTest.table, aes(x = groups, y = diff, ymin = lwr, ymax = upr, colour = significant)) + 
  geom_hline(yintercept = 0, color = "black", linetype ="dashed", size = 0.5) + 
	geom_errorbar(width = 0.1) + 
	geom_point(size = 2) + 
	labs(x = "Soil", y = "difference in yields")

TukeyTest.table <- TukeyTest$Fertilizer |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "groups") |> 
	rename("p.value" = "p adj") |> 
	mutate(significant = factor(p.value < 0.05))
ggplot(TukeyTest.table, aes(x = groups, y = diff, ymin = lwr, ymax = upr, colour = significant)) + 
  geom_hline(yintercept = 0, color = "black", linetype ="dashed", size = 0.5) + 
	geom_errorbar(width = 0.1) + 
	geom_point(size = 2) + 
	labs(x = "Fertilizer", y = "difference in yields")

scheffe.test(aov.model, "Soil")$groups
scheffe.test(aov.model, "Fertilizer")$groups
scheffe.test(aov.model, c("Soil", "Fertilizer"))$groups

# Two-way ANOVA with interactions

aov.model <- aov(Yield ~ Soil + Fertilizer + Soil:Fertilizer, data = dat) # or simply
aov.model <- aov(Yield ~ Soil * Fertilizer, data = dat)
summary(aov.model)
# Compare this table with ANOVA table without interactions
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")
# Compare the coefficients, effects and means with ANOVA without interactions

# Continue with multiple comparison using Tukey's and Scheffe's method

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$Soil
TukeyTest$Fertilizer
TukeyTest$"Soil:Fertilizer"
# ...

scheffe.test(aov.model, "Soil")$groups
scheffe.test(aov.model, "Fertilizer")$groups
scheffe.test(aov.model, c("Soil", "Fertilizer"))$groups

