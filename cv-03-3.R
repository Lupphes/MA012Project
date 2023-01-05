library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- read.csv2(file = "data/goods.csv", stringsAsFactors = TRUE)
View(dat)
dat <- dat |> mutate(group = Advert:Packaging)
str(dat)
summary(dat)
View(dat)

# Cannot be performed due to the low number of observations!!!!!!!!!!!!!!
# Check random sampling
# inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.
#bartlett.test(Profit ~ group, data = dat) #|| bartlett.test(Yield ~ group, data = dat)
leveneTest(Profit ~ group, data = dat)    #|| leveneTest(Yield ~ group, data = dat)

# Cannot be performed due to the low number of observations!!!!!!!!!!!!!!
dat |>
  group_by(group) |>
  summarise(
    # Lilliefors.p.value = lillie.test(Profit)$p.value, # comment if cannot be evaluated
    Shapiro.p.value = shapiro.test(Profit)$p.value
  )

# Two-way ANOVA without interactions
aov.model <- aov(Profit ~ Advert + Packaging, data = dat)
summary(aov.model)
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$Advert
TukeyTest$Packaging

scheffe.test(aov.model, "Advert")$groups
scheffe.test(aov.model, "Packaging")$groups
scheffe.test(aov.model, c("Advert", "Packaging"))$groups

# Cannot be performed due to the low number of observations!!!!!!!!!!!!!!
# Two-way ANOVA with interactions
aov.model <- aov(Profit ~ Advert * Packaging, data = dat)
summary(aov.model)
# Compare this table with ANOVA table without interactions
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")
# Compare the coefficients, effects and means with ANOVA without interactions
# Continue with multiple comparison using Tukey's and Scheffe's method

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$Advert
TukeyTest$Packaging
TukeyTest$"Advert:dose"
