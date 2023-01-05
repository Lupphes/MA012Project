library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")


dat <- read.csv2(file = "data/coagulation.csv", stringsAsFactors = TRUE)
str(dat)
summary(dat)

dat |> ggplot(aes(x = Coagulation, y = Diet, color=Diet)) + 
  geom_point() + 
  labs(x = "Coagulation", y = "Diet")

dat |> ggplot(aes(x = Diet, y = Coagulation, color=Diet)) + 
  geom_boxplot() + 
  labs(x = "Coagulation", y = "Diet")

# Normality tests
dat |>
  group_by(Diet) |>
  summarise(
    #Lilliefors.p.value = lillie.test(Coagulation)$p.value, # comment if cannot be evaluated
    Shapiro.p.value = shapiro.test(Coagulation)$p.value
  )

# ANOVA using "aov" function
aov.model <- aov(Coagulation ~ Diet, data = dat)
# ANOVA table
summary(aov.model)
# Design matrix
model.matrix(aov.model)
# Coefficients of the linear regression model
aov.model$coefficients
# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

ScheffeTest <- scheffe.test(aov.model, "Diet")
ScheffeTest
ScheffeTest$groups

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
