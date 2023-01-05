library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

dat <- read.csv2(file = "data/data01.csv")
str(dat)
summary(dat)

ggplot(dat, aes(x = x, y = Y)) + 
  geom_point() + 
  labs(x = "x", y = "Y")

# Linear regression models
M1 <- lm(Y ~ x, data = dat)
M2 <- lm(Y ~ x + I(x^2), data = dat)
M3 <- lm(Y ~ x + I(x^2) + I(x^3), data = dat)


# define models M3, M2, M1
# ...

summary(M3)
summary(M2)
summary(M1)

# ANOVA

anova(M3, M2)

anova(M2, M1)

# Plot fitted functions
x.new <- data.frame(x = seq(0, 10, by = 0.01))
M3.fit <- predict(M3, newdata = x.new)
M2.fit <- predict(M2, newdata = x.new)
M1.fit <- predict(M1, newdata = x.new)
fits <- cbind(x.new, M3 = M3.fit, M2 = M2.fit, M1 = M1.fit) |> 
  tidyr::pivot_longer(cols = !x, names_to = "model", values_to = "fit") |> 
  mutate(model = factor(model))
ggplot(dat, aes(x = x, y = Y)) + 
  geom_point() + 
  geom_line(aes(x = x, y = fit, color = model), data = fits, size = 1.0) + 
  labs(x = "x", y = "Y")

# ... with 95% confidence intervals
x.new <- data.frame(x = seq(0, 10, by = 0.01))
M3.ci <- predict(M3, newdata = x.new, interval = "confidence") |> 
  cbind(x = x.new) |> 
  mutate(model = factor("M3"))
M2.ci <- predict(M2, newdata = x.new, interval = "confidence") |> 
  cbind(x = x.new) |> 
  mutate(model = factor("M2"))
M1.ci <- predict(M1, newdata = x.new, interval = "confidence") |> 
  cbind(x = x.new) |> 
  mutate(model = factor("M1"))
ci <- rbind(M1.ci, M2.ci, M3.ci)
ggplot(dat, aes(x = x, y = Y)) + 
  geom_point() + 
  geom_line(aes(x = x, y = fit, color = model), data = fits, size = 1.0) + 
  geom_ribbon(aes(x = x, y = x, ymin = lwr, ymax = upr, fill = model, color = model), data = ci, alpha = 0.1, linetype = "dashed") +
  labs(x = "x", y = "Y")

# Visually (at least) check the residuals
residuals <- data.frame(
    x = dat$x, 
    M1 = M1$residuals, 
    M2 = M2$residuals, 
    M3 = M3$residuals
  ) |> 
  tidyr::pivot_longer(cols = !x, names_to = "model", values_to = "residual") |> 
  mutate(model = factor(model))
# Residuals vs. x
ggplot(residuals, aes(x = x, y = residual, color = model)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  geom_line(linetype = "dashed") + 
  geom_point() + 
  labs(x = "x", y = "residual")
# Boxplot of residuals
ggplot(residuals, aes(x = model, y = residual, fill = model)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_boxplot(color = "black") + 
  geom_jitter(width = 0.1) + 
  labs(x = "model", y = "residual")
# Notmal QQ-plot of residuals
ggplot(residuals, aes(sample = residual, colour = model)) + 
	geom_qq(distribution = stats::qnorm) + 
	geom_qq_line(distribution = stats::qnorm) + 
	labs(x = "theoretical gaussian quantiles", y = "empirical quantiles") + 
	facet_grid(~ model)

