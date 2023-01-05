library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- read.csv2(file = "data/mice.csv", stringsAsFactors = TRUE)
str(dat)
summary(dat)
View(dat)
dat <- dat |> 
  select(Mouse1, Mouse2, Mouse3) |> 
  pivot_longer(everything(), names_to = "Mouse", values_to = "Speez") # wide -> long format of the data frame
dat <- dat |> mutate(
  Mouse = as.factor(trimws(Mouse))
)
str(dat)
View(dat)

# not normal distribution
dat |>
  group_by(Mouse) |>
  summarise(
    Shapiro.p.value = shapiro.test(Speez)$p.value
  )


aov.model <- aov(Speez ~ Mouse, data = dat)
# ANOVA table
summary(aov.model)
# Design matrix
model.matrix(aov.model)
# Coefficients of the linear regression model
aov.model$coefficients
# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest