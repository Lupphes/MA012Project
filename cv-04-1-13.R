library("dplyr")
library("ggplot2")
library("tidyr")
library("agricolae")
library("car")
library("BSDA")

# ==================================================================
# Example 1

dt <- read.csv2(file = "data/minute.csv", header = FALSE)
dt <- dt |> 
	rename(X = V1) |> 
	mutate(R = rank(X), R.W = rank(abs(X - 60)))
View(dt)
dt |> 
	summarise(across(X,
		list(mean = mean, stdev = sd, variance = var, median = median, lowest = min, highest = max, iqr = IQR), 
	  .names = "{.fn}"
		)) |> 
	as.data.frame()
ggplot(dt, aes(x = "", y = X)) + 
	geom_boxplot() + 
	geom_jitter(size = 2.5, width = 0.1, height = 0) + 
	labs(x = "", y = "estimate of a minute [seconds]")

# ordered sample
sort(dt$X)
# ranks
rank(dt$X)
# permutation of indices
order(dt$X)
# sign test
SIGN.test(dt$X, md = 60)
# Wilcoxon signed-rank test
wilcox.test(dt$X, mu = 60)



# ==================================================================
# Example 2

dt <- read.csv2(file = "data/field.csv")
dt <- dt |> mutate(R.W = rank(Yield))
View(dt)
ggplot(dt, aes(x = Fertilizer, y = Yield, color = Fertilizer)) + 
	geom_boxplot() + 
	geom_jitter(size = 2.5, width = 0.1, height = 0) + 
	labs(x = "Fertilizer", y = "Yield [tons per hectare]")
wilcox.test (dt |> filter(Fertilizer == "A") |> pull(Yield), dt |> filter(Fertilizer == "B") |> pull(Yield))



# ==================================================================
# Example 3

dt <- read.csv2(file = "data/potatoes.csv")
dt <- dt |> 
  mutate(Variety = factor(Variety))
View(dt)

ggplot(dt, aes(x = Variety, y = Weight, fill = Variety)) + 
	geom_boxplot() + 
	geom_jitter(width = 0.1, height = 0) + 
	labs(x = "potato variety", y = "weight [kg]")

# Kruskal-Wallis test 
KWtest <- kruskal.test(dt$Weight, dt$Variety) # or
KWtest <- with(dt, kruskal(Weight, Variety, group=FALSE))
KWtest

# Median test 
Mtest <- Median.test(dt$Weight, dt$Variety) # or
Mtest <- with(dt, Median.test(Weight, Variety, group=FALSE))
Mtest


# ==================================================================
# Example 4
dt <- read.csv2(file = "data/octane.csv", header = FALSE)
dt <- dt |> mutate(
  X = gsub("X", "", V1),
  X = as.numeric(gsub(",", ".", X))
)
str(dt)
summary(dt)
View(dt)
dt |> 
  summarise(across(X,
                   list(mean = mean, stdev = sd, variance = var, median = median, lowest = min, highest = max, iqr = IQR), 
                   .names = "{.fn}"
  )) |> 
  as.data.frame()
ggplot(dt, aes(x = "", y = X)) + 
  geom_boxplot() + 
  geom_jitter(size = 2.5, width = 0.1, height = 0) + 
  labs(x = "", y = "estimate of a minute [seconds]")

# ordered sample
sort(dt$X)
# ranks
rank(dt$X)
# permutation of indices
order(dt$X)
# sign test
SIGN.test(dt$X, md = 98)
# Wilcoxon signed-rank test
wilcox.test(dt$X, mu = 98)

# ==================================================================
# Example 5
dt <- read.csv2(file = "data/bloodpressure.csv", header = TRUE)
View(dt)

# Wilcoxon signed-rank test
wilcox.test(dt$Before, dt$After,exact=FALSE)

# ==================================================================
# Example 6
dt <- read.csv2(file = "data/machines.csv", header = TRUE)
dt <- dt |> 
  mutate(Manufacturer = factor(Manufacturer))
View(dt)


# Kruskal-Wallis test 
KWtest <- kruskal.test(dt$Efficiency, dt$Manufacturer) # or
KWtest <- with(dt, kruskal(Efficiency, Manufacturer))
KWtest

# ==================================================================
# Example 7
dt <- read.csv2(file = "data/activesubstance.csv", header = TRUE)
dt <- dt |> 
  mutate(Supplier = factor(Supplier))
View(dt)

wilcox.test (dt |> filter(Supplier == "A") |> pull(Content), dt |> filter(Supplier == "B") |> pull(Content))

# ==================================================================
# Example 8
dt <- read.csv2(file = "data/minute2.csv", header = FALSE)
dt <- dt |> 
  rename(X = V1) |> 
  mutate(R = rank(X), R.W = rank(abs(X - 60)))
View(dt)


SIGN.test(dt$X, md = 60)
# Wilcoxon signed-rank test
wilcox.test(dt$X, mu = 60)
# Kaslu na one sided, neumim filtrovat, to tam neda

# ==================================================================
# Example 9
dt <- read.csv2(file = "data/nickel.csv", header = TRUE)
mutate(Laborant = factor(Laborant))
View(dt)

# Kruskal-Wallis test 
KWtest <- kruskal.test(dt$Nickel, dt$Laborant, group=FALSE) # or
KWtest <- with(dt, kruskal(Nickel, Laborant, group=FALSE))
KWtest


# Median test 
Mtest <- Median.test(dt$Nickel, dt$Laborant) # or
Mtest <- with(dt, Median.test(Nickel, Laborant, group=FALSE))
Mtest

# ==================================================================
# Example 10

dt <- read.csv2(file = "data/potatoes2.csv")
dt <- dt |> 
  mutate(Variety = factor(Variety))
View(dt)

ggplot(dt, aes(x = Variety, y = Yield, fill = Variety)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, height = 0) + 
  labs(x = "potato variety", y = "weight [kg]")

# Kruskal-Wallis test 
KWtest <- kruskal.test(dt$Yield, dt$Variety) # or
KWtest <- with(dt, kruskal(Yield, Variety, group=FALSE))
KWtest

# Median test 
Mtest <- Median.test(dt$Yield, dt$Variety) # or
Mtest <- with(dt, Median.test(Yield, Variety, group=FALSE))
Mtest

# ==================================================================
# Example 11
dt <- read.csv2(file = "data/paper.csv")
dt <- dt |> 
  mutate(Lab = factor(Lab))
View(dt)


# Kruskal-Wallis test 
KWtest <- kruskal.test(dt$Smoothness, dt$Lab) # or
KWtest <- with(dt, kruskal(Smoothness, Lab, group=FALSE))
KWtest

# Median test 
Mtest <- Median.test(dt$Smoothness, dt$Lab) # or
Mtest <- with(dt, Median.test(Smoothness, Lab, group=FALSE))
Mtest

# ==================================================================
# Example 12
dt <- read.csv2(file = "data/IQvitaminB.csv")
View(dt)

# Wilcoxon signed-rank test
wilcox.test(dt$VitaminB, dt$Placebo, exact=FALSE)


# ==================================================================
# Example 13
dt <- read.csv2(file = "data/rats.csv")
View(dt)

# Wilcoxon signed-rank test
wilcox.test(dt$Bandage, dt$Stitches, exact=FALSE)

# ==================================================================
# Example 13
dt <- read.csv(file = "data/sales.csv", header=TRUE)
dt <- dt %>%
  pivot_longer(cols=c('store1', 'store2', 'store3', 'store4', 'store5', 'store6', 'store7'), names_to = "Store", values_to = "Sales")
View(dt)
friedman.test(y=dt$Sales, groups=dt$product, blocks=dt$Store)
