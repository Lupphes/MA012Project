library("dplyr")
library("ggplot2")
library("car")
library("nlme")		# gls
library("tidyr")

dt <- read.csv2(file = "data/water.csv", header = TRUE, sep = ";")
dt <- dt |> mutate(
  t = Year - min(Year)
)
str(dt)
summary(dt)
View(dt)
n <- nrow(dt)
alpha <- 0.05

ggplot(dt, aes(x = x, y = Y)) + 
  geom_point(size = 2.0) + 
  labs(x = "years from 1956", y = "square root of number of birds")

# Classical linear regression model using OLS, uncorrelated random errors
model.OLS <- lm(Y ~ x, data = dt)
summary(model.OLS)
confint(model.OLS)

new.x <- data.frame(x = seq(0, 2000, by = 0.11))
CI.OLS <- predict(model.OLS, newdata = new.x, interval = "confidence", level = 0.95) |> 
  as.data.frame() |> 
  mutate(model = "OLS") |> 
  cbind(new.x)



ggplot() + 
  geom_point(data = dt, mapping = aes(x = x, y = Y)) + 
  geom_ribbon(data = CI.OLS, mapping = aes(x = x, ymin = lwr, ymax = upr), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = CI.OLS, aes(x = x, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "produced water", y = "water loss")


# Durbin-Watson test
durbinWatsonTest(model.OLS, max.lag = 5)

# estimates of the autoregression coefficient 
dt <- dt |> mutate(
  r.OLS = residuals(model.OLS)
)
1 - unname(lmtest::dwtest(model.OLS)$statistic) / 2
cor(dt$r.OLS[-n], dt$r.OLS[-1])
as.numeric( (t(dt$r.OLS[-n]) %*% dt$r.OLS[-1]) / (t(dt$r.OLS[-n]) %*% dt$r.OLS[-n]) )

# asymptotic test
U <- abs(sqrt(n) * 0.71)
c(U = U, quantile = qnorm (1 - alpha/2)) 
U > qnorm (1 - alpha/2)  

ggplot(dt, aes(y = r.OLS)) + 
  geom_boxplot(color = "darkviolet") + 
  labs(y = "residuals in OLS model")

ggplot(dt, aes(x = t, y = r.OLS)) + 
  geom_line(linetype = "dotted", color = "darkviolet") + 
  geom_point(size = 2.0, color = "darkviolet") + 
  geom_hline(yintercept = 0) + 
  labs(x = "years from 1956", y = "residuals in OLS model")

ggplot(dt, aes(sample = r.OLS)) + 
  geom_qq_line(distribution = stats::qnorm) + 
  geom_qq(distribution = stats::qnorm, size = 2.0, color = "darkviolet") + 
  labs(x = "theoretical gaussian quantiles", y = "empirical quantiles")

w <- acf(dt$r.OLS, plot = FALSE)
acf.OLS <- data.frame(lag = c(w$lag), acf = c(w$acf))
acf.CI <- qnorm(1 - alpha/2) / sqrt(n)
ggplot(acf.OLS, aes(x = lag, y = acf)) + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = -acf.CI, linetype = "dashed") +
  geom_hline(yintercept = acf.CI, linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 2.0, color = "darkviolet") + 
  labs(x = "lag [years]", y = "autocorrelation function of OLS residuals")


# Linear regression model using GLS, autoregression AR1 structure of random errors

model.AR1 <- gls(Y ~ x, data = dt, correlation = corAR1())
summary(model.AR1, correlation = TRUE)
confint(model.AR1)

CI.AR1 <- AICcmodavg::predictSE.gls(model.AR1, newdata = new.x, se.fit = TRUE, level = 0.95) |> 
  as.data.frame() |> 
  mutate(
    lwr = fit - qnorm(1 - alpha/2) * se.fit, 
    upr = fit + qnorm(1 - alpha/2) * se.fit, 
    model = "AR1"
  ) |> 
  select(-se.fit) |> 
  cbind(new.x)

ggplot() + 
  geom_point(data = dt, mapping = aes(x = x, y = Y)) + 
  geom_ribbon(data = CI.AR1, mapping = aes(x = x, ymin = lwr, ymax = upr), size = 0.5, alpha = 0.2, fill = "red", color = "red", linetype = "dashed") + 
  geom_line(data = CI.AR1, aes(x = x, y = fit), color = "red", size = 1.0) + 
  labs(x = "years from 1956", y = "square root of number of birds")

# Linear regression model using GLS, autoregression AR2 structure of random errors
model.AR2 <- gls(Y ~ x, data = dt, correlation = corARMA(p = 2))
summary(model.AR2, correlation = TRUE)
confint(model.AR2)

CI.AR2 <- AICcmodavg::predictSE.gls(model.AR2, newdata = new.x, se.fit = TRUE, level = 0.95) |> 
  as.data.frame() |> 
  mutate(
    lwr = fit - qnorm(1 - alpha/2) * se.fit, 
    upr = fit + qnorm(1 - alpha/2) * se.fit, 
    model = "AR2"
  ) |> 
  select(-se.fit) |> 
  cbind(new.x)

ggplot() + 
  geom_point(data = dt, mapping = aes(x = x, y = Y)) + 
  geom_ribbon(data = CI.AR2, mapping = aes(x = x, ymin = lwr, ymax = upr), size = 0.5, alpha = 0.2, fill = "red", color = "red", linetype = "dashed") + 
  geom_line(data = CI.AR2, aes(x = x, y = fit), color = "red", size = 1.0) + 
  labs(x = "years from 1956", y = "square root of number of birds")


# Cochran-Orcutt method

model.CO <- orcutt::cochrane.orcutt(model.OLS)
model.CO
summary(model.CO)

CI.CO <- data.frame(
  x = new.x$x, 
  fit = cbind(rep(1, nrow(new.x)), new.x$x) %*% coefficients(model.CO), 
  lwr = NA, 
  upr = NA, 
  model = "CO"
)

dt <- dt |> mutate(
  r.CO = residuals(model.CO)
)

ggplot(dt, aes(y = r.CO)) + 
  geom_boxplot(color = "darkcyan") + 
  labs(y = "residuals in OLS model")

ggplot(dt, aes(x = x, y = r.CO[-1]-0,71*r.CO[-n])) +  # EDIT
  geom_line(linetype = "dotted", color = "darkcyan") + 
  geom_point(size = 2.0, color = "darkcyan") + 
  geom_hline(yintercept = 0) + 
  labs(x = "years from 1956", y = "residuals in OLS model")

ggplot(dt, aes(sample = r.CO)) + 
  geom_qq_line(distribution = stats::qnorm) + 
  geom_qq(distribution = stats::qnorm, size = 2.0, color = "darkviolet") + 
  labs(x = "theoretical gaussian quantiles", y = "empirical quantiles")

w <- acf(dt$r.CO, plot = FALSE)
acf.OLS <- data.frame(lag = c(w$lag), acf = c(w$acf))
acf.CI <- qnorm(1 - alpha/2) / sqrt(n)
ggplot(acf.OLS, aes(x = lag, y = acf)) + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = -acf.CI, linetype = "dashed") +
  geom_hline(yintercept = acf.CI, linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 2.0, color = "darkcyan") + 
  labs(x = "lag [years]", y = "autocorrelation function of OLS residuals")

# estimates of the autoregression coefficient 
cor(dt$r.CO[-n], dt$r.CO[-1])
as.numeric( (t(dt$r.CO[-n]) %*% dt$r.CO[-1]) / (t(dt$r.CO[-n]) %*% dt$r.CO[-n]) )

# Durbin-Watson test
lmtest::dwtest(model.CO)
# estimates of the autoregression coefficient 
1 - unname(lmtest::dwtest(model.CO)$statistic) / 2

# asymptotic test
U <- abs(sqrt(n) * theta)
c(U = U, quantile = qnorm (1 - alpha/2)) 
U > qnorm (1 - alpha/2)  



# Summary

CI <- rbind(CI.OLS, CI.AR1, CI.AR2, CI.CO)

ggplot() + 
  geom_point(data = dt, mapping = aes(x = x, y = Y)) + 
  geom_ribbon(data = CI, mapping = aes(x = x, ymin = lwr, ymax = upr, col = model, fill = model), size = 0.5, alpha = 0.2, linetype = "dashed") + 
  geom_line(data = CI, aes(x = x, y = fit, col = model), size = 1.0) + 
  labs(x = "years from 1956", y = "square root of number of birds")

# 95% confidence intervals for regression coefficients 

confint(model.OLS)
confint(model.AR1)
confint(model.AR2)
coefficients(model.CO) + t(c(-1, 1) %*% t(model.CO$std.error * qnorm(1 - alpha/2)))

