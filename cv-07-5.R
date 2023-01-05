library("car")
library("Hmisc")
library("dplyr")
library("ggplot2")

dt <- read.csv2(file = "data/cement.csv", header = TRUE)
str(dt)
summary(dt)
View(dt)

n <- nrow
M <- as.matrix(dt)
alpha <- 0.05


model <- lm(y ~ ., data = dt)
summary(model)
View(dt)

dt.X <- dt |> select(-c("y"))
GGally::ggpairs(dt.X, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))


R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# backward
model.back <- step(model, direction = "backward", trace = 1)
summary(model.back)

r <- residuals(model.back)
shapiro.test(r)$p.value
