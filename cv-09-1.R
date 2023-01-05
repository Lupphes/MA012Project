library("dplyr")
library("ggplot2")

dt <- read.csv2 (file = "data/beetle.csv")
str(dt)
dt <- dt |> mutate(
  prop = killed / population, 
  survived = population - killed
  )
summary(dt)
n <- nrow(dt)
alpha <- 0.05
q <- qnorm(1 - alpha / 2)

ggplot(dt, aes(x = dose, y = killed)) + 
  geom_point(size = 2.0) + 
  labs(x = "dose", y = "number of killed beetles")

ggplot(dt, aes(x = dose, y = prop)) + 
  geom_point(size = 2.0) + 
  labs(x = "dose", y = "proportion of killed beetles") + 
  ylim(c(0, 1))

# logit link 

model.logit <- glm(cbind(killed, survived) ~ dose, data = dt, family = binomial(link = "logit"))
summary(model.logit)

x.new <- data.frame(
  dose = seq(1.6, 2.0, by = 0.001)
  )

fit.logit <- predict(model.logit, x.new, type = "response", se.fit = FALSE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  mutate(model = "logit")

eta.logit <- predict(model.logit, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "logit", 
    dose = dose, 
    eta = fit,                               # linear predictor
    eta.lower = fit - q * se.fit,            # lower bound ( 2.5%) for linear predictor
    eta.upper = fit + q * se.fit,            # upper bound (97.5%) for linear predictor
    fit = 1 / (1 + exp(-eta)),               # fit
    fit.lower = 1 / (1 + exp(- eta.lower)),  # lower bound ( 2.5%) for fit
    fit.upper = 1 / (1 + exp(- eta.upper))   # upper bound (97.5%) for fit
    )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = dose, y = prop), size = 2.0) + 
  geom_ribbon(data = eta.logit, mapping = aes(x = dose, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logit, aes(x = dose, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "dose", y = "probability")

ggplot() + 
  geom_ribbon(data = eta.logit, mapping = aes(x = dose, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logit, aes(x = dose, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "dose", y = "linear predictor")

# residuals 
dr <- dt |> mutate(r = residuals(model.logit))
ggplot(dr, aes(y = r)) + 
  geom_boxplot(color = "darkviolet") + 
  labs(y = "residuals")
ggplot(dr, aes(x = 1:n, y = r)) + 
  geom_line(linetype = "dotted", color = "darkviolet") + 
  geom_point(size = 2.0, color = "darkviolet") + 
  geom_hline(yintercept = 0) + 
  labs(x = "index", y = "residuals")
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

# probit link 

model.probit <- glm(cbind(killed, survived) ~ dose, data = dt, family = binomial(link = "probit"))
summary(model.probit)

eta.probit <- predict(model.probit, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "probit", 
    dose = dose, 
    eta = fit,
    eta.lower = fit - q * se.fit,
    eta.upper = fit + q * se.fit,
    fit = pnorm(eta),
    fit.lower = pnorm(eta.lower),
    fit.upper = pnorm(eta.upper)
    )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = dose, y = prop), size = 2.0) + 
  geom_ribbon(data = eta.probit, mapping = aes(x = dose, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.probit, aes(x = dose, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "dose", y = "probability")

ggplot() + 
  geom_ribbon(data = eta.probit, mapping = aes(x = dose, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.probit, aes(x = dose, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "dose", y = "linear predictor")

# residuals 
dr <- dt |> mutate(r = residuals(model.probit))
ggplot(dr, aes(y = r)) + 
  geom_boxplot(color = "darkviolet") + 
  labs(y = "residuals")
ggplot(dr, aes(x = 1:n, y = r)) + 
  geom_line(linetype = "dotted", color = "darkviolet") + 
  geom_point(size = 2.0, color = "darkviolet") + 
  geom_hline(yintercept = 0) + 
  labs(x = "index", y = "residuals")
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


# CLogLog link

model.CLogLog <- glm(cbind(killed, survived) ~ dose, data = dt, family = binomial(link = "cloglog"))
summary(model.CLogLog)

eta.CLogLog <- predict(model.CLogLog, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
	cbind(x.new) |> 
  transmute(
    model = "CLogLog", 
    dose = dose, 
    eta = fit,
    eta.lower = fit - q * se.fit,
    eta.upper = fit + q * se.fit,
    fit = 1 - exp(- exp(eta)), 
    fit.lower = 1 - exp(- exp(eta.lower)),
    fit.upper = 1 - exp(- exp(eta.upper))
    )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = dose, y = prop), size = 2.0) + 
  geom_ribbon(data = eta.CLogLog, mapping = aes(x = dose, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.CLogLog, aes(x = dose, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "dose", y = "probability")

ggplot() + 
  geom_ribbon(data = eta.CLogLog, mapping = aes(x = dose, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.CLogLog, aes(x = dose, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "dose", y = "linear predictor")

# residuals 
dr <- dt |> mutate(r = residuals(model.CLogLog))
ggplot(dr, aes(y = r)) + 
  geom_boxplot(color = "darkviolet") + 
  labs(y = "residuals")
ggplot(dr, aes(x = 1:n, y = r)) + 
  geom_line(linetype = "dotted", color = "darkviolet") + 
  geom_point(size = 2.0, color = "darkviolet") + 
  geom_hline(yintercept = 0) + 
  labs(x = "index", y = "residuals")
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

eta <- rbind(eta.logit, eta.probit, eta.CLogLog)

ggplot() + 
  geom_point(data = dt, mapping = aes(x = dose, y = prop), size = 2.0, color = "black") + 
  geom_line(data = eta, aes(x = dose, y = fit, color = model), size = 1.0) + 
  labs(x = expression(paste("dose ", x)), y = expression(paste("probability ", p))) + 
  xlim(c(1.65, 1.9))

ggplot() + 
  geom_point(data = dt, mapping = aes(x = dose, y = prop), size = 2.0, color = "black") + 
  geom_ribbon(data = eta, mapping = aes(x = dose, ymin = fit.lower, ymax = fit.upper, color = model, fill = model), size = 0.5, alpha = 0.1, linetype = "dashed") + 
  geom_line(data = eta, aes(x = dose, y = fit, color = model), size = 1.0) + 
  labs(x = expression(paste("dose ", x)), y = expression(paste("probability ", p))) + 
  xlim(c(1.65, 1.9))

ggplot() + 
  geom_ribbon(data = eta, mapping = aes(x = dose, ymin = eta.lower, ymax = eta.upper, color = model, fill = model), size = 0.5, alpha = 0.1,linetype = "dashed") + 
  geom_line(data = eta, aes(x = dose, y = eta, color = model), size = 1.0) + 
  labs(x = expression(paste("dose ", x)), y = expression(paste("linear predictor ", eta))) + 
  xlim(c(1.65, 1.9)) + ylim(c(-5, 5.5))
  

