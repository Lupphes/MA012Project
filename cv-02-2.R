library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- read.csv2(file = "data/salesman.csv")
str(dat)
summary(dat)


dat <- dat |> 
  select(Salesman1, Salesman2, Salesman3) |> 
  pivot_longer(everything(), names_to = "Salesman", values_to = "Muney") # wide -> long format of the data frame
dat <- dat |> mutate(
  Salesman = as.factor(trimws(Salesman))
)
str(dat)
View(dat)

# Point plot Salesman vs. Muney
dat |> ggplot(aes(x = Salesman, y = Muney, color=Salesman)) + 
  geom_point() + 
  labs(x = "Salesman", y = "Muney")

# Boxplot
dat |> ggplot(aes(x = Salesman, y = Muney, color=Salesman)) + 
  geom_boxplot() +
  labs(x = "Salesman", y = "a")


# Normality tests
dat |>
  group_by(Salesman) |>
  summarise(
    Lilliefors.p.value = lillie.test(Muney)$p.value, # comment if cannot be evaluated
    Shapiro.p.value = shapiro.test(Muney)$p.value
  )

# ANOVA using "aov" function
aov.model <- aov(Muney ~ Salesman, data = dat)
# ANOVA table
summary(aov.model)
# Design matrix
model.matrix(aov.model)
# Coefficients of the linear regression model
aov.model$coefficients
# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

# Normal QQ plots
ggplot(dat, aes(sample = Muney, colour = Salesman)) + 
  geom_qq(distribution = stats::qnorm) + 
  geom_qq_line(distribution = stats::qnorm) + 
  labs(x = "theoretical gaussian quantiles", y = "empirical quantiles") + 
  facet_grid(~ Salesman)
# facet_wrap(~ Salesman, ncol = 2)


# Scheffe's method
ScheffeTest <- scheffe.test(aov.model, "Muney")
ScheffeTest
ScheffeTest$groups

TukeyTest <- TukeyHSD(aov.model)
TukeyTest
