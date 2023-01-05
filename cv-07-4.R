library("car")
library("Hmisc")
library("dplyr")
library("ggplot2")

str(mtcars)
summary(mtcars)
dt <- mtcars |> as.data.frame()

model <- lm(mpg ~ ., data = dt)
summary(model)
View(dt)

dt.X <- dt |> select(-c("mpg"))
GGally::ggpairs(dt.X, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

vif(model)
X <- model.matrix(model)[,-1]
det(solve(t(X)%*%X))
R <- rcorr(X)
R$r
diag(R$P) <- 0
R$P
ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
  lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

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

# forward
model0 <- update(model, . ~ 1)
model0
model.forw <- step(model0, scope = formula(model), direction = "forward", trace = 1)
summary(model.forw)

# bidirectional
model.bidir <- step(model, direction = "both", trace = 1)
summary(model.bidir)

# bidirectional
model.bidir <- step(model0, scope = formula(model), direction = "both", trace = 1)
summary(model.bidir)


r <- residuals(model.back)
shapiro.test(r)$p.value

