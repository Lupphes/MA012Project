library("dplyr")
library("tidyr")
library("ggplot2")

# Example 1

# Read data
# Correctly read the data into a data frame
dt <- read.csv("data/people.csv", header = TRUE, sep = "|", stringsAsFactors = TRUE)
str(dt)
summary(dt)



# Corrections
# Weight: decimal points
# BMI: "NULL" -> NA, -> numeric
# Sex: trim white space characters, -> factor
dt <- dt |> mutate(
    Weight = as.numeric(gsub(",", ".", Weight)),
    BMI = as.numeric(as.character(replace(BMI, BMI == "NULL", NA))),
    Sex = as.factor(trimws(Sex))
  )
str(dt)
summary(dt)

# Problems
levels(dt$Weight)
levels(dt$BMI)
levels(dt$Sex)

# Descriptive statistics
dt |> summarise(
  Height = mean(Height, na.rm = TRUE), 
  Weight = mean(Weight, na.rm = TRUE),
  BMI = mean(BMI, na.rm = TRUE)
  )

# Try
dt |> select(where(is.numeric))
dt |> select(Height, Weight, BMI)

# Descriptive statistics
dt |> select(Height, Weight, BMI) |> 
  summarise(across(everything(), mean, na.rm = TRUE))
dt |> summarise(across(c(Height, Weight, BMI), mean, na.rm = TRUE))
dt |> summarise(across(c(Height, Weight, BMI), 
		list(mean = mean, sd = sd), 
		na.rm = TRUE, 
		.names = "{.col}.{.fn}"
	))
dt |> summarise(across(c(Height, Weight, BMI), 
    list(mean = mean, sd = sd, var = var, med = median), 
    na.rm = TRUE, 
    .names = "{.col}.{.fn}"
  ))

# Grouping by sex
dt |> group_by(Sex) |> 
  summarise(across(c(Height, Weight, BMI), 
  list(mean = mean, sd = sd, var = var, med = median), 
  na.rm = TRUE, 
  .names = "{.col}.{.fn}")) |> 
  as.data.frame()

# Number of levels of each factor column
dt |> summarise(across(where(is.factor), 
    \(x){length(levels(x))} # definition of "anonymous function"
  ))

# Boxplot
dt |> ggplot(aes(x = Weight)) + 
  geom_boxplot() +
  labs(x = "vaha", y = "a")
# .. boxplot for Weight, BMI

# Boxplot - grouping by sex
dt |> ggplot(aes(x = Sex, y = Height, fill = Sex)) + 
  geom_boxplot()
# .. boxplot for Weight, BMI
# Note some unexpected valus of BMI! 

# More columns in one plot, but not much reasonable here
dn <- dt |> 
  select(Height, Weight, BMI) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "value") # wide -> long format of the data frame
str(dn)
dn |> ggplot(aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot()

# Histogram
dt |> nrow() |> sqrt() |> ceiling()
ceiling(sqrt(nrow(dt)))

dt |> ggplot(aes(x = Height)) + 
  geom_histogram(color = "black", fill = "#FFCC00", bins = 23)
# ...

# Frequency plot as barplot
dt |> ggplot(aes(x = DogWalker)) + 
  geom_bar(color = "black", fill = "#FFCC00")
# ...

# Plot boxplot, histogram and barplot for other columns

# Point plots
dt |> ggplot(aes(x = Weight, y = Height)) + 
  geom_point()
dt |> ggplot(aes(x = Weight, y = Height, color = Sex)) + 
  geom_point()

# Plot BMI vs. Weight
# ...
# Note some unexpected valus of BMI! 

summary(dt$BMI)
dt |> pull(BMI) |> summary()
# Calculate BMI using fomula for weight and height
# Find errors in original BMI calculations
# ...
dt <- dt |> mutate(
  BMI.new = Weight / (Height / 100)^2
)
dt |> ggplot(aes(x=Weight, y= BMI.new, color = Sex)) + 
  geom_point()

dt |> filter(abs(BMI - BMI.new) > 1)

names(dt)
# Rename columns to use the new BMI instead of the original, delete additional column BMI.check
# ...
dt <- dt |> rename(
  BMI = BMI.new,
  BMI.old = BMI
)
dt |> str()

# Point plot Weight vs. Height
dt |> ggplot(aes(x = Height, y = Weight)) + 
  geom_point() + 
	labs(x = "height [cm]", y = "weight [kg]")

# Linear regression model with linear function
m1 <- lm(Weight ~ 1 + Height, data = dt) # or
m1 <- lm(Weight ~ Height, data = dt)
summary(m1)
test <- t.test(Weight ~ Sex, data = dt)
test$p.value
# Comment th results
model.matrix(m1)
# Explain the columns of the design matrix

new.x <- data.frame(Height = seq(130, 210, by = 1))
CI <- predict(m1, newdata = new.x, interval = "confidence", level = 0.95) |> 
	cbind(new.x)
pred <- predict(m1, newdata = new.x, interval = "prediction", level = 0.95) |> 
	cbind(new.x)

ggplot() + 
  geom_point(data = dt, mapping = aes(x = Height, y = Weight)) + 
  geom_ribbon(data = CI, mapping = aes(x = Height, ymin = lwr, ymax = upr), size = 0.5, alpha = 0.2, fill = "red", color = "red", linetype = "dashed") + 
  geom_ribbon(data = pred, mapping = aes(x = Height, ymin = lwr, ymax = upr), size = 0.5, alpha = 0.1, fill = "red", color = "red", linetype = "dashed") + 
  geom_line(data = CI, aes(x = Height, y = fit), color = "red", size = 1.0) + 
	labs(x = "height [cm]", y = "weight [kg]")

# Point plot Weight vs. Height according to Sex
dt |> ggplot(aes(x = Height, y = Weight, color = Sex)) + 
  geom_point() + 
	labs(x = "height [cm]", y = "weight [kg]")

# separate parallel regression lines with respect to Sex
m3 <- lm(Weight ~ 1 + Height + Sex, data = dt)
summary(m3)
model.matrix(m3)
# "dummy variable" for Sex: Sexmale = 1 (male) or 0 (female)

# Calculate fit, confidence and pediction intervals and plot them
# ...

# separate regression lines with respect to Sex
m4 <- lm(Weight ~ 1 + Height + Sex + Height:Sex, data = dt) # or
m4 <- lm(Weight ~ 1 + Height * Sex, data = dt)
summary(m4)
model.matrix(m4)
# "dummy variables"

# Calculate fit, confidence and pediction intervals and plot them
# ...


# Calculate the prediction of the weight for man/woman of given height... 



# Repeat, e.g., for BMI in dependency on height or weight 


# t-test of equality of BMI for both sexes
t.test(BMI ~ Sex, data = dt)
test <- t.test(BMI ~ Sex, data = dt)
test
test$p.value

# compare mean BMI - grouped by PhoneOwner
# ... 
# Explain the result.

