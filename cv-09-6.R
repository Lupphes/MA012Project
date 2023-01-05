library("dplyr")
library("ggplot2")

dt <- read.csv2 (file = "data/sharks.csv")
str(dt)
dt <- dt |> mutate(
  rate = Attacks / Population, 
  t = Year - min(Year)
  )
summary(dt)
n <- nrow(dt)
alpha <- 0.05
q <- qnorm(1 - alpha / 2)

ggplot(dt, aes(x = t, y = Attacks)) + 
  geom_point(size = 2.0) + 
  labs(x = "years since 1946", y = "number of attacks")

ggplot(dt, aes(x = t, y = 1e6 * rate)) + 
  geom_point(size = 2.0) + 
  labs(x = "years since 1946", y = "average number of attacks per million people")

# binomial model with logit link 

model.logistic <- glm(cbind(Attacks, Population - Attacks) ~ t + I(t^2) + I(t^3), data = dt, family = binomial(link = "logit"))
summary(model.logistic)      # all regression coefficients are significant

x.new <- data.frame(t = seq(0, 55, by = 0.01))

eta.logistic <- predict(model.logistic, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "logistic", 
    t = t, 
    eta = fit,
    eta.lower = fit - q * se.fit,
    eta.upper = fit + q * se.fit, 
    fit = 1 / (1 + exp(-eta)),
    fit.lower = 1 / (1 + exp(- eta.lower)),
    fit.upper = 1 / (1 + exp(- eta.upper))
    )



ggplot() + 
  geom_point(data = dt, mapping = aes(x = t, y = 1e6 * rate), size = 2.0) + 
  geom_ribbon(data = eta.logistic, mapping = aes(x = t, ymin = 1e6 * fit.lower, ymax = 1e6 * fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logistic, aes(x = t, y = 1e6 * fit), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "average number of attacks per million people")

ggplot() + 
  geom_ribbon(data = eta.logistic, mapping = aes(x = t, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logistic, aes(x = t, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "linear predictor")

# deviance
model.logistic$deviance
# degrees of freedom = # observations - # parameters
model.logistic$df.residual
# Deviance is more than two times larger => incorrectly modelled variance

# binomial model with logit link with dispersion coefficient as nuisance parameter

model.logistic2 <- update(model.logistic, family = quasibinomial)
summary(model.logistic2)      # only intersect is significant
# dispersion coefficient
summary(model.logistic2)$dispersion

eta.logistic2 <- predict(model.logistic2, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "logistic +", 
    t = t, 
    eta = fit,
    eta.lower = fit - q * se.fit,
    eta.upper = fit + q * se.fit, 
    fit = 1 / (1 + exp(-eta)),
    fit.lower = 1 / (1 + exp(- eta.lower)),
    fit.upper = 1 / (1 + exp(- eta.upper))
    )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = t, y = 1e6 * rate), size = 2.0) + 
  geom_ribbon(data = eta.logistic2, mapping = aes(x = t, ymin = 1e6 * fit.lower, ymax = 1e6 * fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logistic2, aes(x = t, y = 1e6 * fit), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "average number of attacks per million people")

ggplot() + 
  geom_ribbon(data = eta.logistic2, mapping = aes(x = t, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logistic2, aes(x = t, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "linear predictor")

# residuals 
dr <- dt |> mutate(r = residuals(model.logistic2))
ggplot(dr, aes(y = r)) + 
  geom_boxplot(color = "darkviolet") + 
  labs(y = "residuals")
ggplot(dr, aes(x = t, y = r)) + 
  geom_line(linetype = "dotted", color = "darkviolet") + 
  geom_point(size = 2.0, color = "darkviolet") + 
  geom_hline(yintercept = 0) + 
  labs(x = "dose", y = "residuals")
ggplot(dr, aes(sample = r)) + 
	geom_qq_line(distribution = stats::qnorm) + 
	geom_qq(distribution = stats::qnorm, size = 2.0, color = "darkviolet") + 
	labs(x = "theoretical gaussian quantiles", y = "empirical quantiles")
w <- acf(dr$r, plot = FALSE)
acf <- data.frame(lag = c(w$lag), acf = c(w$acf))
acf.CI <- q / sqrt(n)
ggplot(acf, aes(x = lag, y = acf)) + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = -acf.CI, linetype = "dashed") +
  geom_hline(yintercept = acf.CI, linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 2.0, color = "darkviolet") + 
	labs(x = "lag", y = "ACF of residuals")

# poissonian model with log link 

model.poisson <- glm(Attacks ~ t + I(t^2) + I(t^3), data = dt, family = poisson(link = "log"))
summary(model.poisson)

eta.poisson <- predict(model.poisson, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "Poisson", 
    t = t, 
    eta = fit, 
    eta.lower = fit - q * se.fit, 
    eta.upper = fit + q * se.fit,
    fit = exp(eta),
    fit.lower = exp(eta.lower), 
    fit.upper = exp(eta.upper)
    )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = t, y = Attacks), size = 2.0) + 
  geom_ribbon(data = eta.poisson, mapping = aes(x = t, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.poisson, aes(x = t, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "number of attacks")

ggplot() + 
  geom_ribbon(data = eta.poisson, mapping = aes(x = t, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.poisson, aes(x = t, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "linear predictor")

# deviance
model.poisson$deviance
# degrees of freedom = # observations - # parameters
model.poisson$df.residual
# Deviance is more than two times larger => incorrectly modelled variance

# poissonian model with log link with dispersion coefficient as nuisance parameter

model.poisson2 <- update(model.poisson, family = quasipoisson)
summary(model.poisson2)
# dispersion coefficient
summary(model.poisson2)$dispersion

eta.poisson2 <- predict(model.poisson2, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "Poisson +", 
    t = t, 
    eta = fit, 
    eta.lower = fit - q * se.fit, 
    eta.upper = fit + q * se.fit,
    fit = exp(eta),
    fit.lower = exp(eta.lower), 
    fit.upper = exp(eta.upper)
    )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = t, y = Attacks), size = 2.0) + 
  geom_ribbon(data = eta.poisson2, mapping = aes(x = t, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.poisson2, aes(x = t, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "number of attacks")

ggplot() + 
  geom_ribbon(data = eta.poisson2, mapping = aes(x = t, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.poisson2, aes(x = t, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "t", y = "linear predictor")

# residuals 
dr <- dt |> mutate(r = residuals(model.poisson2))
ggplot(dr, aes(y = r)) + 
  geom_boxplot(color = "darkviolet") + 
  labs(y = "residuals")
ggplot(dr, aes(x = t, y = r)) + 
  geom_line(linetype = "dotted", color = "darkviolet") + 
  geom_point(size = 2.0, color = "darkviolet") + 
  geom_hline(yintercept = 0) + 
  labs(x = "dose", y = "residuals")
ggplot(dr, aes(sample = r)) + 
	geom_qq_line(distribution = stats::qnorm) + 
	geom_qq(distribution = stats::qnorm, size = 2.0, color = "darkviolet") + 
	labs(x = "theoretical gaussian quantiles", y = "empirical quantiles")
w <- acf(dr$r, plot = FALSE)
acf <- data.frame(lag = c(w$lag), acf = c(w$acf))
acf.CI <- q / sqrt(n)
ggplot(acf, aes(x = lag, y = acf)) + 
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = -acf.CI, linetype = "dashed") +
  geom_hline(yintercept = acf.CI, linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), size = 2.0, color = "darkviolet") + 
	labs(x = "lag", y = "ACF of residuals")

# summary 

eta <- rbind(eta.logistic, eta.logistic2)
ggplot() + 
  geom_point(data = dt, mapping = aes(x = t, y = 1e6 * rate), size = 2.0, color = "black") + 
  geom_ribbon(data = eta, mapping = aes(x = t, ymin = 1e6 * fit.lower, ymax = 1e6 * fit.upper, color = model, fill = model), size = 0.5, alpha = 0.1, linetype = "dashed") + 
  geom_line(data = eta, aes(x = t, y = 1e6 * fit, color = model), size = 1.0) + 
  labs(x = "years since 1946", y = "average number of attacks per million people")
ggplot() + 
  geom_ribbon(data = eta, mapping = aes(x = t, ymin = eta.lower, ymax = eta.upper, color = model, fill = model), size = 0.5, alpha = 0.1,linetype = "dashed") + 
  geom_line(data = eta, aes(x = t, y = eta, color = model), size = 1.0) + 
  labs(x = "years since 1946", y = expression(paste("linear predictor ", eta)))

eta <- rbind(eta.poisson, eta.poisson2)
ggplot() + 
  geom_point(data = dt, mapping = aes(x = t, y = Attacks), size = 2.0, color = "black") + 
  geom_ribbon(data = eta, mapping = aes(x = t, ymin = fit.lower, ymax = fit.upper, color = model, fill = model), size = 0.5, alpha = 0.1, linetype = "dashed") + 
  geom_line(data = eta, aes(x = t, y = fit, color = model), size = 1.0) + 
  labs(x = "years since 1946", y = "number of attacks")
ggplot() + 
  geom_ribbon(data = eta, mapping = aes(x = t, ymin = eta.lower, ymax = eta.upper, color = model, fill = model), size = 0.5, alpha = 0.1,linetype = "dashed") + 
  geom_line(data = eta, aes(x = t, y = eta, color = model), size = 1.0) + 
  labs(x = "years since 1946", y = expression(paste("linear predictor ", eta)))

# estimate of the number of shark atacks (per 1 million people) in 2013

1e6 * predict(model.logistic2, data.frame(t = 2013-1946), type="response")
eta2013 <- predict(model.logistic2, data.frame(t = 2013-1946), type="link", se.fit = TRUE)
lower <- 1e+06 / (1 + exp(-(eta2013$fit - q * eta2013$se.fit)))
upper <- 1e+06 / (1 + exp(-(eta2013$fit + q * eta2013$se.fit)))
c(lower, upper)
