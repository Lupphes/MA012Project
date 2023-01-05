library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- data.frame(ToothGrowth)


dat <- dat |> mutate(
  supp = as.factor(supp),
  dose = as.factor(dose)
)
dat <- dat |> mutate(group = supp:dose)
str(dat)
summary(dat)
View(dat)


# Check random sampling
# inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.
bartlett.test(len ~ group, data = dat) #|| bartlett.test(Yield ~ group, data = dat)
leveneTest(len ~ group, data = dat)    #|| leveneTest(Yield ~ group, data = dat)

dat |>
  group_by(group) |>
  summarise(
    Lilliefors.p.value = lillie.test(len)$p.value, # comment if cannot be evaluated
    Shapiro.p.value = shapiro.test(len)$p.value
  )

# Two-way ANOVA without interactions
aov.model <- aov(len ~ supp + dose, data = dat)
summary(aov.model)
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$supp
TukeyTest$dose

scheffe.test(aov.model, "supp")$groups
scheffe.test(aov.model, "dose")$groups
scheffe.test(aov.model, c("supp", "dose"))$groups


# Two-way ANOVA with interactions
aov.model <- aov(len ~ supp * dose, data = dat)
summary(aov.model)
# Compare this table with ANOVA table without interactions
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")
# Compare the coefficients, effects and means with ANOVA without interactions
# Continue with multiple comparison using Tukey's and Scheffe's method

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$supp
TukeyTest$dose
TukeyTest$"supp:dose"
