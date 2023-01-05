# Nastavit working directory
setwd("~/Muni/Statistika/Statistika_New")

# Nacitat vzdycky
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
library(data.table)

# Histogram odmocnina z poctu mereni
# Histogram
dt |> nrow() |> sqrt() |> ceiling()
ceiling(sqrt(nrow(dt)))


# Pivot longer
dat <- dat |> 
  select(Mouse1, Mouse2, Mouse3) |>  # Co grupovat
  pivot_longer(everything(), names_to = "Mouse", values_to = "Speez") # wide -> long format of the data frame

# Cviceni 2 ################################

# Reading data 
# Muzes zkusit csv bez 2
dat <- read.csv2(file = "data/potatoes.csv")

# Note the data types
# categorical variable => factor
dat <- dat |> 
  mutate(<column_name> = factor(<column_name>)) #|| Nejaka faktorova mnozina

# Normality tests (vetsi jak 0.05)
# test if it comes from the normal distribution
# Testujeme jestli jednotlive faktory jsou normalni nebo ne
dt |>
  group_by(Salesman) |>
  summarise(
    Lilliefors.p.value = lillie.test(<column_name>)$p.value, # comment if cannot be evaluated || Muney
    Shapiro.p.value = shapiro.test(<column_name>)$p.value                                     #|| Muney
  )


# ANOVA
aov.model <- aov(<column_name> ~ <column_name>, data = dt)                                              #|| Muney
# ANOVA using "aov" function
aov.model <- aov(<column_name> ~ <column_name>, data = dt)
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
ScheffeTest <- scheffe.test(aov.model, "<column_name>") #|| "Muney"
ScheffeTest
ScheffeTest$groups

TukeyTest <- TukeyHSD(aov.model)
TukeyTest



# Cviceni 3 ################################
# To groups
dat <- dat |> mutate(group = <column_name>:<column_name>) #|| Soil:Fertilizer

# Sumarize and calculate stats
table(dat$<column_name>, dat$<column_name>)  #|| (dat$Soil, dat$Fertilizer)
freq <- dat |> 
	group_by(<column_name>, <column_name>) |>  #|| group_by(Soil, Fertilizer)
	summarise(n = n())
freq

stat <- dat |> 
	group_by(<column_name>, <column_name>) |>  #|| group_by(Soil, Fertilizer)
	summarise(across(Yield,
		list(mean = mean, stdev = sd, variance = var, median = median, lowest = min, highest = max, iqr = IQR), 
	  .names = "{.fn}"
		))
stat
stat |> as.data.frame()


# Check random sampling
# inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.
bartlett.test(<column_name> ~ <column_name>, data = dat) #|| bartlett.test(Yield ~ group, data = dat)
leveneTest(<column_name> ~ <column_name>, data = dat)    #|| leveneTest(Yield ~ group, data = dat)

# Normality tests (vetsi jak 0.05)
# test if it comes from the normal distribution
dat |>
  group_by(group) |>
  summarise(
    #Lilliefors.p.value = lillie.test(Yield)$p.value, # comment if cannot be evaluated
    Shapiro.p.value = shapiro.test(Yield)$p.value
  )


# Two-way ANOVA without interactions
aov.model <- aov(<column_name> ~ <column_name> + <column_name>, data = dat) #|| Yield ~ Soil + Fertilizer
summary(aov.model)
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$<column_name>            #|| TukeyTest$Soil
TukeyTest$<column_name>            #|| TukeyTest$Fertilizer     

scheffe.test(aov.model, "<column_name>")$groups                  #|| scheffe.test(aov.model, "Soil")$groups
scheffe.test(aov.model, "<column_name>")$groups                  #|| scheffe.test(aov.model, "Fertilizer")$groups
scheffe.test(aov.model, c("<column_name>", "<column_name>"))$groups #|| scheffe.test(aov.model, c("Soil", "Fertilizer"))$groups

#############################

# Two-way ANOVA with interactions
aov.model <- aov(Yield ~ Soil + Fertilizer + Soil:Fertilizer, data = dat) # or simply
aov.model <- aov(<column_name> ~ <column_name> * <column_name>, data = dat)  #|| aov.model <- aov(Yield ~ Soil * Fertilizer, data = dat)

summary(aov.model)
# Compare this table with ANOVA table without interactions
aov.model$coefficients
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")
# Compare the coefficients, effects and means with ANOVA without interactions

# Continue with multiple comparison using Tukey's and Scheffe's method
# Tady testujeme navic interakce
TukeyTest <- TukeyHSD(aov.model)
TukeyTest$<column_name>                         #|| TukeyTest$Soil
TukeyTest$<column_name>                         #|| TukeyTest$Fertilizer  
TukeyTest$"<column_name>:<column_name>"         #|| TukeyTest$"Soil:Fertilizer"

scheffe.test(aov.model, "<column_name>")$groups       #|| scheffe.test(aov.model, "Soil")$groups  
scheffe.test(aov.model, "<column_name>")$groups       #|| scheffe.test(aov.model, "Fertilizer")$groups 
scheffe.test(aov.model, c("<column_name>", "<column_name>"))$groups  #|| scheffe.test(aov.model, c("Soil", "Fertilizer"))$groups


# Cviceni 4 ################################

# ordered sample
sort(dt$X<column_name>)    #|| sort(dt$X)
# ranks
rank(dt$X<column_name>)    #|| sort(dt$X) 
# permutation of indices
order(dt$<column_name>)    #|| sort(dt$X)  
# sign test
SIGN.test(dt$<column_name>, md = 60) # 60 sekund timer, na co se ptali a sloupec byl X
# Wilcoxon signed-rank test
wilcox.test(dt$<column_name>, mu = 60) # 60 sekund timer, na co se ptalia sloupec byl X

# More complex pull and push manipulation with R for Wilcoxon rank-sum test, factorem si selectuju co chci
# wilcox.test (dt |> filter(Fertilizer == "A") |> pull(Yield), dt |> filter(Fertilizer == "B") |> pull(Yield))
wilcox.test (dt |> filter(<column_name> == "<factor>") |> pull(<column_name>), dt |> filter(<column_name> == "<factor>") |> pull(<column_name>))


# Kruskal-Wallis test 
# KWtest <- kruskal(dt$Weight, dt$Variety) # or
# KWtest <- kruskal.test(dt$Weight, dt$Variety)  # Mi rekne p-value
# KWtest <- with(dt, kruskal(Weight, Variety, group=FALSE)) # rekne groups
# KWtest


KWtest <- kruskal(dt$<column_name>, dt$<column_name>) # or
KWtest <- kruskal.test(dt$<column_name>, dt$<column_name>)  # Mi rekne p-value
KWtest <- with(dt, kruskal(<column_name>, <column_name>, group=FALSE)) # rekne groups
KWtest

# Median test 
# Mtest <- Median.test(dt$Weight, dt$Variety)      # Mi rekne p-value
# Mtest <- with(dt, Median.test(Weight, Variety, group=FALSE))  # rekne groups
# Mtest

Mtest <- Median.test(dt$<column_name>, dt$<column_name>)      # Mi rekne p-value
Mtest <- with(dt, Median.test(<column_name>, <column_name>, group=FALSE))  # rekne groups
Mtest

# Friedman test
dt <- dt %>%
  pivot_longer(cols=c('store1', 'store2', 'store3', 'store4', 'store5', 'store6', 'store7'), names_to = "Store", values_to = "Sales")
# friedman.test(y=dt$Sales, groups=dt$product, blocks=dt$Store)
friedman.test(y=dt$<column_name>, groups=dt$<column_name>, blocks=dt$<column_name>)


# Cviceni 5 ################################

k <- nrow(dt)
n <- sum(dt$<column_name>) # Soucet celku #|| Frequency

# Binomicke rozdeleni
dt <- dt |> mutate(
  <column_name> = seq(0, 7, by = 1) #|| Number6
) # Pokud mam nejake '+' hodnoty tak to musim tady zaznacit
dt <- dt |> mutate(
  N.j = <column_name>, # Tu frekvence kterou merim #|| Frequency
  p.j = dbinom(<column_name>, size = 12, prob = 1/6), # Zadane binomicke rozdeleni #|| Number6
  n.j = n * p.j # Moje nova pravdepodobnost
)

# Rovnomerne rozdeleni
dt <- dt |> mutate(
  <column_name> = factor(<column_name>),   #| Light, faktoruju kaminky
  N.j = <column_name>,                     #| NumFish, zase moje frekvence
  p.j = 1 / 5,                             # Tohle musim vymyslet
  n.j = n * p.j
)

# Poisonovo rozdeleni
dt <- dt |> mutate(
  N.j = <column_name>,                     #| Matches, zase moje frekvence
  <column_name> = seq(0, 4, by = 1),       #| Matches, zase moje frekvence
  lambda = (sum(<column_name> * N.j) / n), #| Goals, Lamda se pouziva pro vypocet
  p.j = dpois(<column_name>, lambda),      #| Goals, Dane rozdeleni
  n.j = n * p.j
)

# Exponentialni rozdeleni
# To tam neda, cv-05-2
dt <- dt |> mutate(
  time = factor(WaitingTime, labels = categories, levels = categories, ordered = TRUE), # ordered factor
  time.from = seq(0, 21, by = 3),
  time.to = seq(3, 24, by = 3),
  time.avg = (time.from + time.to) / 2, 
  N.j = NumCustomers, 
  lambda = 1 / (sum (time.avg * N.j) / n), 
  p.j = pexp(time.to, lambda) - pexp(time.from, lambda),
  n.j = n * p.j
)


# Vypocet pravdepodobnosti protoze to uplne neznam
# Poznam to tak ze tam mam neco +, tak musim zahrnout i ten interval
# dt[dt$Number6 == 7, "p.j"] <- dt |> filter(Number6 <= 6) |> summarise(p.j = 1 - sum(p.j)) |> pull(p.j)
# dt[dt$Number6 == 7, "n.j"] <- dt |> filter(Number6 <= 6) |> summarise(n.j = n - sum(n.j)) |> pull(n.j)
dt[dt$<column_name> == 7, "p.j"] <- dt |> filter(<column_name> <= 6) |> summarise(p.j = 1 - sum(p.j)) |> pull(p.j)
dt[dt$<column_name> == 7, "n.j"] <- dt |> filter(<column_name> <= 6) |> summarise(n.j = n - sum(n.j)) |> pull(n.j)
# to cislo je pocet pres ktere to budu davat jakoby dal, vzdy to nejvyssi a pak o jedno mene

# Musi se rovnat
sum(dt$N.j)
sum(dt$n.j)
# Musi byt jednicka
sum(dt$p.j)


# Assumptions
dt$n.j >= 5
# 1. 
#	Yarnold's criterion
# testovani ze kategorie jsou spravne
q <- sum (dt$n.j < 5) / k
q
dt$n.j >= 5 * q
#	OK
# Pokud je aspon jednou vsechno TRUE jsem ready to go

# Pokud to nevyjde merguju
d2 <- dt |> filter(NumPatients >= 0 & NumPatients <= 8) # Sloupce co chci nechat
d3 <- dt |> filter(NumPatients >= 9) |>  # Co chci sloucit
  summarise(across(c(Frequency, N.j, p.j, n.j), sum)) |> 
  mutate(NumPatients = NA, lambda=dt$lambda[1]) # Doplneni sloupcu 

dt2 <- do.call(rbind, list(d2, d3))


# Pearson’s chi-squared test (Pearsonův test dobré shody)
K <- sum(dt$N.j^2 / (dt$n.j)) - n
K
# χ2_0.95
qchisq (0.95, df = k - 1)
K >= qchisq (0.95, df = k - 1)
#	p-value
1 - pchisq (K, df = k - 1)
# built-in function

# Cela funkce
chisq.test (dt$N.j, p = dt$p.j)
# Nezamitam

# Pokud pouzivan paranetr lambda nesmim zapomenout odecist, exponencialni a poisonovo rozdeleni
K <- sum(dt$N.j^2 / (dt$n.j)) - n
K
qchisq (0.95, df = k - 1 - 1) # remember -1
K >= qchisq (0.95, df = k - 1 - 1) # remember -1
#	p-value
1 - pchisq (K, df = k - 1 - 1) # remember -1




###################################################################
# Specific test for Poisson distribution
X <- rep(dt$<column_name>, dt$N.j)    #|| NumPatients, Vypocet X, tady je ten prvni sloupec, faktor to 1 az neco

Q <- (n - 1) * var (X) / mean (X)
Q
q1 = qchisq (0.025, n - 1)
q2 = qchisq (0.975, n - 1)
c(q1, q2)
Q <= q1 | Q >= q2


###################################################################
# Specific test for Exponential distribution
X <- rep(dt$<column_name>, dt$N.j)    #|| time.avg, Vypocet X, tady je ten prvni sloupec, faktor to 1 az neco

Q <- (n - 1) * var(X) / (mean(X))^2 
Q
q1 = qchisq (0.025, n - 1)
q2 = qchisq (0.975, n - 1)
c(q1, q2)
Q <= q1 | Q >= q2



# Cviceni 6 ################################
# Tohle snad vymyslis z toho examplu, good luck

# Load modelu
M <- as.matrix(dt)

#	Scatterplot
GGally::ggpairs(dt, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

#	Mutiple correlation between expense and (income, members)
model.M <- lm(HGRAD ~ UNEM + ROLL + INC + YEAR, data = dt) # To prvni je podle ceho, a pak zbytek
cor(dt$HGRAD, fitted.values(model.M)) # by definition, as correlation between variable and its best linear approximation
sqrt(summary(model.M)$r.squared)      # or, as square root of R squared


# Spearman's correlations 
R.S <- rcorr(M, type = "spearman")
R.S$r
R.S$P
diag(R.S$P) <- 0

# Partial Spearman's correlations 
R.S.partial <- pcor(M, method = "spearman")
R.S.partial$estimate  # Podle ceho
R.S.partial$p.value

# E.g., Spearman's correlation
R$r["Points", "Weight"]
R$P["Points", "Weight"]


# Are Equal -> Anova (2)
# Two way analysis -> Anova (3)
# Medians are equal (4)



# Cviceni 7 ################################
# Yeet year
dt <- dt |> mutate(
  t = Year - min(Year)
)

n <- nrow(dt)


# Classical linear regression model using OLS, uncorrelated random errors
# t je to vuci cemu to modeluju
model.OLS <- lm(Y ~ t, data = dt)
summary(model.OLS)

# confidence interval for one or more parameters in a fitted regression model.
confint(model.OLS)

# Nezapomenout na to
new.x <- data.frame(t = seq(0, 50, by = 0.11))
### important

CI.OLS <- predict(model.OLS, newdata = new.x, interval = "confidence", level = 0.95) |> 
  as.data.frame() |> 
  mutate(model = "OLS") |> 
  cbind(new.x)

# Add residuals to model
dt <- dt |> mutate(
  r.OLS = residuals(model.OLS)
)

# Durbin-Watson test
durbinWatsonTest(model.OLS, max.lag = 5)
# or
lmtest::dwtest(model.OLS)
# estimates of the autoregression coefficient 
1 - unname(lmtest::dwtest(model.OLS)$statistic) / 2
cor(dt$r.OLS[-n], dt$r.OLS[-1])
as.numeric( (t(dt$r.OLS[-n]) %*% dt$r.OLS[-1]) / (t(dt$r.OLS[-n]) %*% dt$r.OLS[-n]) )

# asymptotic test
U <- abs(sqrt(n) * 0.71)
c(U = U, quantile = qnorm (1 - alpha/2)) 
U > qnorm (1 - alpha/2) 


# Linear regression model using GLS, autoregression AR1 structure of random errors

model.AR1 <- gls(Y ~ t, data = dt, correlation = corAR1())
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

# Linear regression model using GLS, autoregression AR2 structure of random errors

model.AR2 <- gls(Y ~ t, data = dt, correlation = corARMA(p = 2))
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


# Cochran-Orcutt method
model.CO <- orcutt::cochrane.orcutt(model.OLS)
model.CO
summary(model.CO)

CI.CO <- data.frame(
  t = new.x$t, 
  fit = cbind(rep(1, nrow(new.x)), new.x$t) %*% coefficients(model.CO), 
  lwr = NA, 
  upr = NA, 
  model = "CO"
)

dt <- dt |> mutate(
  r.CO = residuals(model.CO)
)

w <- acf(dt$r.CO, plot = FALSE)
acf.OLS <- data.frame(lag = c(w$lag), acf = c(w$acf))
acf.CI <- qnorm(1 - alpha/2) / sqrt(n)

# estimates of the autoregression coefficient 
cor(dt$r.CO[-n], dt$r.CO[-1])
as.numeric( (t(dt$r.CO[-n]) %*% dt$r.CO[-1]) / (t(dt$r.CO[-n]) %*% dt$r.CO[-n]) )

# 95% confidence intervals for regression coefficients 
confint(model.OLS) #|| Put model inside

# Basic bitch model
n <- nrow
M <- as.matrix(dt)
alpha <- 0.05

model <- lm(Expenses ~ ., data = dt)
summary(model)

# Correlation
R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0


# backward
model.back <- step(model, direction = "backward", trace = 1)
summary(model.back)

r <- residuals(model.back)
shapiro.test(r)$p.value

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

# Cviceni 8 ################################

# Set everything as supposed to
rownames(dt) <- dt$Country
M <- dt |> dplyr::select(-Country) |> as.matrix()
M.centered <- scale(M, center = TRUE, scale = FALSE)
M.standardized <- scale(M, center = TRUE, scale = TRUE)


# Summarize 
dt |> dplyr::summarise(across(
  everything(), 
  list(mean = mean, stdev = sd),
  na.rm = TRUE, 
  .names = "{.col}.{.fn}"
))


# Coefficients in the first two principal components
#PC1 PC2
pca <- prcomp(M, center = TRUE, scale. = TRUE)
pca$rotation
pca$x
pca$sdev
sum(pca$sdev^2)

loadings <- t(pca$rotation) * pca$sdev
loadings # Moje

dt.var <- data.frame(
  r = seq_along(pca$sdev), 
  var = (pca$sdev**2) / sum(pca$sdev**2), 
  cumvar = cumsum(pca$sdev**2) / sum(pca$sdev**2)
)

dt.var |> filter(cumvar >= 0.8)    # First 5 PCs explain at least 80 % of variance of data
dt.var |> filter(var >= mean(var)) # Kaiser's rule: take first 5 PCs  

# Correlation matrix of original variables
det(R$r) # Determinant is close to zero! Why? 
# Eigenvalues and eigenvectors
w <- eigen(R$r)
w$values
w$vectors
U <- w$vectors # Matrix of eigenvectors
t(U) %*% U # Really almost identity matrix


# Compare with correlation matrix of pricnipal components
cor(pca$x)
det(cor(pca$x))
w <- eigen(cor(pca$x))
w$values
w$vectors
U <- w$vectors
t(U) %*% U

# Cviceni 9 ################################

# Killed vs survived
# logit link 
model.logit <- glm(cbind(killed, survived) ~ dose, data = dt, family = binomial(link = "logit"))
summary(model.logit)

# Always with the part
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

