library("dplyr")
library("tidyr")
library("ggplot2")
library("Hmisc")		  #	rcorr
library("ppcor")		  #	pcor
library("rgl")			  # 3D graphics
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("ggplot2")
library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

# Nacitani dat
dt <- read.csv(file = "data/SocialLife.csv")
str(dt)
summary(dt)
View(dt)

# 1. ############################## 


dt <- dt |> 
  mutate(Communicate = factor(Communicate)) #|| Nejaka faktorova mnozina

# Normality tests (vetsi jak 0.05)
# test if it comes from the normal distribution
# Testujeme jestli jednotlive faktory jsou normalni nebo ne (z normalniho rozdeleni)
dt |>
  group_by(Communicate) |>
  summarise(
    Lilliefors.p.value = lillie.test(HangHours)$p.value, # comment if cannot be evaluated || Muney
    Shapiro.p.value = shapiro.test(HangHours)$p.value                                    
  )

# Nejsou z normalniho rozdeleni protoze Shapiro a Liliford test hodnoty jsou mensi nemuzu pouzit anovu

# Point plot Communicate vs. HangHours
dt |> ggplot(aes(x = Communicate, y = HangHours, color=Communicate)) + 
  geom_point() + 
  labs(x = "Communicate", y = "HangHours")

# Boxplot
dt |> ggplot(aes(x = Communicate, y = HangHours, color=Communicate)) + 
  geom_boxplot() +
  labs(x = "Communicate", y = "a")

# Normal QQ plots
ggplot(dt, aes(sample = HangHours, colour = Communicate)) + 
  geom_qq(distribution = stats::qnorm) + 
  geom_qq_line(distribution = stats::qnorm) + 
  labs(x = "theoretical gaussian quantiles", y = "empirical quantiles") + 
  facet_grid(~ Communicate)


# 2. ############################## 

# ANOVA
aov.model <- aov(HangHours ~ Communicate, data = dt)                                              #|| Muney
# ANOVA using "aov" function
aov.model <- aov(HangHours ~ Communicate, data = dt)
# ANOVA table
summary(aov.model)
# Design matrix
model.matrix(aov.model)
# Coefficients of the linear regression model
aov.model$coefficients
# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")



# Since the overall p-value is less than .05, this is an indication that each group does not have the same average  exam score.

# Pairs
ScheffeTest <- scheffe.test(aov.model, "HangHours") #|| "Muney"
ScheffeTest
ScheffeTest$groups

TukeyTest <- TukeyHSD(aov.model)
TukeyTest


# 3. ############################## 

# Load modelu
test <- dt
test <- test |> dplyr::select(-Communicate)
test <- test |> dplyr::select(-Allergies)
View(test)
M <- as.matrix(test)

#	Sample means and standard deviations
lala |> summarise(across(
  everything(), 
  list(mean = mean, stdev = sd),
  na.rm = TRUE, 
  .names = "{.col}.{.fn}"
))

#	Scatterplot
GGally::ggpairs(test, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# Spearman's correlations 
R.S <- rcorr(M, type = "spearman")
R.S$r
R.S$P
diag(R.S$P) <- 0


# Correlogram
plot1 <- ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot1


# Correlogram
plot1S <- ggcorrplot::ggcorrplot(R.S$r, p.mat = R.S$P, title = "Spearman's correlations", 
                                 lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

# Partial correlations for all pairs, excluding all other variables
# using ppcor library
R.partial <- pcor(M)
R.partial$points
R.partial$p.value

plot2 <- ggcorrplot::ggcorrplot(R.partial$estimate, p.mat = R.partial$p.value, title = "Partial Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

# Partial Spearman's correlations 
R.S.partial <- pcor(M, method = "spearman")
R.S.partial$estimate
R.S.partial$p.value

plot2S <- ggcorrplot::ggcorrplot(R.S.partial$estimate, p.mat = R.S.partial$p.value, title = "Partial Spearman's correlations", 
                                 lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

gridExtra::grid.arrange(plot1S, plot2S, ncol = 2, nrow = 1)
gridExtra::grid.arrange(plot1, plot2, plot1S, plot2S, ncol = 2, nrow = 2)

# Vykreslil jsem 2 zpusoby jak diagrama korelovat a jak pro cele tak parcialne a jsou videt v plotech
# Data nejsou moc korelovana (vetsina je velmi blizko nule) a ty ktere nejsou preskrtnute muzeme mluvit o mirne korelaci
# 0.12 a tak



# 4. ############################## 
dt <- dt |> mutate(
  CommunicateApp = as.factor(ifelse(Communicate != "App", "InPersonPhoneText", "App"))
)
View(dt)

wilcox.test (dt |> filter(CommunicateApp == "App") |> pull(ComputerHours), dt |> filter(CommunicateApp == "InPersonPhoneText") |> pull(ComputerHours))

# Pouzil jsem wilconsuv test ktery mi rika ze p-value = 0.008957 je menzi nez 0.05
# takze plati alternativni hypoteza
# Pouzil jsem ho proto protoze pouziva presne median

# 5. ############################## 

require(vcd)
require(MASS)

# data generation
ex <- dt$HangHours  # generate some exponential distribution
control <- abs(rnorm(10000)) # generate some other distribution

# estimate the parameters
fit1 <- fitdistr(ex, "exponential") 
fit2 <- fitdistr(control, "exponential")

# goodness of fit test
ks.test(ex, "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused
ks.test(control, "pexp", fit2$estimate) #  significant p-value -> distribution refused

# plot a graph
hist(ex, freq = FALSE, breaks = 100, xlim = c(0, quantile(ex, 0.99)))
curve(dexp(x, rate = fit1$estimate), from = 0, col = "red", add = TRUE)

# Zkusil jsem goodness of fit abych overil data a podle vseho to neni exponencialni rozdeleni

# 6. ############################## 

dt <- dt |> mutate(
  VideoGame = as.factor(ifelse(VideoGameHours == 0, 0, 1))
)
View(dt)




# 7. ############################## 
dt <- read.csv(file = "data/SocialLife.csv")
dt <- dt |> mutate(
  CommunicateApp = as.factor(ifelse(Communicate != "App", "InPersonPhoneText", "App")),
  VideoGame = as.factor(ifelse(VideoGameHours == 0, 0, 1)),
  else_1 = 24 - HangHours
)

str(dt)
summary(dt)
View(dt)

x.new <- data.frame(
  HangHours = seq(0, 24, by = 0.1)
)
n <- nrow(dt)
alpha <- 0.05
q <- qnorm(1 - alpha / 2)


# Zapomnel jsem dodat zavislost
model.CLogLog <- glm(VideoGame ~ HangHours, data = dt, family = binomial(link = "cloglog"))
summary(model.CLogLog)

eta.CLogLog <- predict(model.CLogLog, x.new, type = "link", se.fit = TRUE) |> 
  as.data.frame() |> 
  cbind(x.new) |> 
  transmute(
    model = "CLogLog", 
    HangHours = HangHours, 
    eta = fit,
    eta.lower = fit - q * se.fit,
    eta.upper = fit + q * se.fit,
    fit = 1 - exp(- exp(eta)), 
    fit.lower = 1 - exp(- exp(eta.lower)),
    fit.upper = 1 - exp(- exp(eta.upper))
  )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = HangHours, y = else_1), size = 2.0) + 
  geom_ribbon(data = eta.CLogLog, mapping = aes(x = HangHours, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.CLogLog, aes(x = HangHours, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "HangHours", y = "probability")

ggplot() + 
  geom_ribbon(data = eta.CLogLog, mapping = aes(x = HangHours, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.CLogLog, aes(x = HangHours, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "HangHours", y = "linear predictor")

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


# Rychlej komentar, residualy vypadaji v pohode ne vidim zadnou velkou chybu
# a interval se zvetsuje coz znacne bude zhorsovat presnost odhadu
