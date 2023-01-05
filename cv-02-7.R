library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")


dat <- data.frame(LakeHuron)

dat <- dat |> mutate(
  Year = seq(98),
  Measurement = LakeHuron
)
str(dat)
summary(dat)
View(dat)


ggplot(dat, aes(x = Year, y = Measurement)) + 
  geom_point() + 
  labs(x = "Year", y = "Measurement")

# Linear regression models
M3 <- lm(Measurement ~ Year + I(Year^2) + I(Year^3) + I(Year^4) + I(Year^5) + I(Year^6) + I(Year^7) + I(Year^8), data = dat)

summary(M3)

x.new <- data.frame(x = seq(0, 10, by = 0.01))
M3.ci <- predict(M3, newdata = x.new, interval = "confidence") |> 
  cbind(x = x.new) |> 
  mutate(model = factor("M3"))
