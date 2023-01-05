library("car")
library("Hmisc")
library("dplyr")
library("ggplot2")

str(longley)
summary(longley)
dt <- longley |> as.data.frame()
View(dt)

# Linear regression model using GLS, autoregression AR1 structure of random errors

model.AR1 <- gls(Employed ~ GNP + Population, data = dt, correlation = corAR1())
summary(model.AR1, correlation = TRUE)
confint(model.AR1)