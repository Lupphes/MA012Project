library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")
library("devtools")
# install_github("vqv/ggbiplot")
library("ggbiplot")

dt <- read.csv2 (file = "data/car_income.csv")
str(dt)
View(dt)

dt <- dt |> mutate(
  purchase = as.factor(purchase),
  category_age = as.factor(ifelse(age >= 3, "new", "old"))
)

n <- nrow(dt)
alpha <- 0.05
q <- qnorm(1 - alpha / 2)

ggplot(dt, aes(x = age, y = income, color=purchase)) + 
  geom_point(size = 2.0) + 
  labs(x = "age", y = "income")

# 2)
x.new <- data.frame(
  income = rep(seq(0,100, by = 0.1)),
  age.fact = rep(c("old", "new"), each = 1001)
)

# dark violet musim dat neco s ti a pak to bude fungovat , vyjmout a vlozit mimo asterisk a i v tom dolnim dat color violet a vlozit to dovnitr

View(dt)
