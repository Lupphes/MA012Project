library("dplyr")
library("ggplot2")
library("car")
library("nlme")		# gls
library("tidyr")
library("Hmisc")

dt <- read.csv2(file = "data/expenses.csv", header = TRUE, sep = ";")
str(dt)
summary(dt)
View(dt)
n <- nrow
M <- as.matrix(dt)
alpha <- 0.05

model <- lm(Expenses ~ ., data = dt)
summary(model)

dt.X <- dt |> dplyr::select(-c("Expenses"))
GGally::ggpairs(dt.X, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))


R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# Test of (un)correlation
R$r["Income", "Expenses"]
R$P["Income", "Expenses"]

# Correlogram
plot1 <- ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot1

n <- nrow(dt)
l <- ncol(dt) - 1
alpha <- 0.05
K <- -(n - 1 - 1/6*(2*l+7))*log(det(R$r))
c(K = K, quantile = qchisq(1-alpha, l*(l-1)/2))
K > qchisq(1-alpha, l*(l-1)/2)

d <- diag(solve(R$r))
d
Fj <- (n-l)/(l-1)*(d-1)
c(Fj = Fj, kvantil = qf(1-alpha, l-1, n-l))
Fj > qf(1-alpha, l-1, n-l)

# backward
model.back <- step(model, direction = "backward", trace = 1)
summary(model.back)

r <- residuals(model.back)
shapiro.test(r)$p.value


