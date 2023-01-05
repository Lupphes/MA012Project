library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- read.csv2(file = "data/corn.csv", stringsAsFactors = TRUE)
dat <- dat |> mutate(
  Variety = as.factor(Variety),
  Fertilizer = as.factor(Fertilizer)
)
View(dat)
dat <- dat |> mutate(group = Variety:Fertilizer)
str(dat)
summary(dat)
View(dat)

# Cannot be performed due to the low number of observations!!!!!!!!!!!!!!
# Check random sampling
# inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.
bartlett.test(Yield ~ group, data = dat) #|| bartlett.test(Yield ~ group, data = dat)
leveneTest(Yield ~ group, data = dat)    #|| leveneTest(Yield ~ group, data = dat)


dat |>
  group_by(group) |>
  summarise(
    #Lilliefors.p.value = lillie.test(Yield)$p.value, # comment if cannot be evaluated
    #Shapiro.p.value = shapiro.test(Yield)$p.value
  )

# Two-way ANOVA without interactions
aov.model <- aov(Yield ~ Variety + Fertilizer, data = dat)
summary(aov.model)
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$Variety
TukeyTest$Fertilizer

scheffe.test(aov.model, "Variety")$groups
scheffe.test(aov.model, "Fertilizer")$groups
scheffe.test(aov.model, c("Variety", "Fertilizer"))$groups

# Two-way ANOVA with interactions
aov.model <- aov(Yield ~ Variety * Fertilizer, data = dat)
summary(aov.model)
# Compare this table with ANOVA table without interactions
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")
# Compare the coefficients, effects and means with ANOVA without interactions
# Continue with multiple comparison using Tukey's and Scheffe's method

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$Variety
TukeyTest$Fertilizer
TukeyTest$"Variety:Fertilizer"
