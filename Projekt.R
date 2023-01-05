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
library("agricolae")
library("car")
library("nortest")

library("devtools")
# install_github("vqv/ggbiplot")
library("ggbiplot")


dt <- read.csv(file = "2022_projekt.csv")

# Convert rows to factors and add stuff
dt <- dt |> dplyr::mutate(
    vegetarian = factor(vegetarian),
    political_aff = factor(political_aff),
    gender = factor(gender),
    GPA_mean = ((high_sch_GPA + college_GPA)/2),
    GPA_diff = high_sch_GPA - college_GPA,
    gender_pol = gender:political_aff
  )

View(dt)

huh <- dt |> ggplot(aes(x = political_aff, y = GPA_diff, color=gender)) + 
  geom_boxplot(show.legend=NULL)


# Sumarize and calculate stats
table(dt$gender, dt$political_aff)
freq <- dt |> 
  group_by(gender, political_aff) |>
  summarise(n = n())
freq

stat <- dt |> 
  group_by(gender, political_aff) |>
  summarise(across(GPA_diff,
                   list(mean = mean, stdev = sd, variance = var, median = median, lowest = min, highest = max, iqr = IQR), 
                   .names = "{.fn}"
  ))
stat
stat |> as.data.frame()


# Check random sampling
# inferential statistic used to assess the equality of variances for a variable calculated for two or more groups.
leveneTest(GPA_diff ~ gender_pol, data = dt)

# I can use ANOVA because results are bigger than 0.05

# Two-way ANOVA without interactions
aov.model <- aov(college_GPA - high_sch_GPA ~ gender + political_aff, data = dt)

# Design matrix
model.matrix(aov.model)

# Coefficients of the linear regression model
aov.model$coefficients

# Effects and means
model.tables(aov.model, type = "effects")
model.tables(aov.model, type = "means")

TukeyTest <- TukeyHSD(aov.model)
TukeyTest$political_aff
TukeyTest$gender

# One-way ANOVA for gender
aov.model.gender <- aov(college_GPA - high_sch_GPA ~ gender, data = dt)

# One-way ANOVA for political_aff
aov.model.political_aff <- aov(college_GPA - high_sch_GPA ~ political_aff, data = dt)

# Summary of the models
summary(aov.model)
summary(aov.model.gender)
summary(aov.model.political_aff)

# Test the significance of the main effects
TukeyHSD(aov.model.gender)
TukeyHSD(aov.model.political_aff)


modelAll.M <- lm(high_sch_GPA - college_GPA ~ gender + political_aff, data = dt)
cor(dt$high_sch_GPA - dt$college_GPA, fitted.values(modelAll.M))

# Summary of the models
summary(modelAll.M)


# Na základě p-hodnot pro prediktorové proměnné se zdá, že politická příslušnost 
# (political_afind) je významným prediktorem rozdílu mezi GPA na střední a vysoké škole, 
# jak naznačuje p-hodnota 0,00537. To naznačuje, že mezi těmito proměnnými může existovat vztah,
# ale bude tento vztah bude malý
# Toto i potvrdila ANOVA


# Ukol 2
dt <- read.csv(file = "2022_projekt.csv")

# Convert rows to factors
dt <- dt |> dplyr::select(
  -gender,
  -vegetarian,
  -political_aff
)
M <- as.matrix(dt)


View(dt)

#	Scatterplot
GGally::ggpairs(dt, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

#	Sample means and standard deviations
dt |> summarise(across(
  everything(), 
  list(mean = mean, stdev = sd),
  na.rm = TRUE, 
  .names = "{.col}.{.fn}"
))

# Variance-covariance matrix
R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# Partial correlations for all pairs, excluding all other variables
R.partial <- pcor(M)
R.partial$p.value

# Spearman's correlations 
R.S <- rcorr(M, type = "spearman")
R.S$r
R.S$P
diag(R.S$P) <- 0

# Partial Spearman's correlations 
R.S.partial <- pcor(M, method = "spearman")
R.S.partial$estimate
R.S.partial$p.value

# Correlogram
plot11 <- ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)


plot21 <- ggcorrplot::ggcorrplot(R.partial$estimate, p.mat = R.partial$p.value, title = "Partial Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

# Correlogram
plot12 <- ggcorrplot::ggcorrplot(R.S$r, p.mat = R.S$P, title = "Spearman's correlations", 
                                 lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)



plot22 <- ggcorrplot::ggcorrplot(R.S.partial$estimate, p.mat = R.S.partial$p.value, title = "Partial Spearman's correlations", 
                                 lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

# gridExtra::grid.arrange(plot1S, plot2S, ncol = 2, nrow = 1)
# gridExtra::grid.arrange(plot1, plot2, plot1S, plot2S, ncol = 2, nrow = 2)
# gridExtra::grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

gridExtra::grid.arrange(plot11, plot21, plot12, plot22, nrow = 2, ncol = 2)

# Vypocital jsem 4 korelogramy na danych datech pomoci Pearsonovych a Spearmanovych korelaci
# a z nich jsem jeste vypocital parcialni korelace. Vetsina hodnot neni vyznamna coz je predevsim 
# videt pokod jsou hodnoty castecne korelovane.
# Pro interpretaci hodnot budu pouzivat spise Pearsonovi korelace protoze data vysli z Levene testu jako vice podobna normalnimu rozdeleni
# Na datech je videt ze prumer ze stredni skoly a prumer z vysoke skoly je korelovany s koeficientem 0.28 a to je i videt na Spearmanovych korelaci
# s koeficientem 0.26. Na Pearsonovych koefiencentech je opet negativni korelace s koukani na televizi a prumerem na stredni skole. Cim vice 
# student koukal na televizi tim horsi ma prumer.


# Ukol 3
dt <- read.csv(file = "2022_projekt.csv")

# Create a contingency table of the counts of each combination of "gender" and "political_aff"
contingency.table <- table(dt$gender, dt$political_aff)

# Perform the chi-square test of independence
chisq.test(contingency.table)

# Pri testu zavislosti mezi Gender a political_aff byl pouzit chisq.test ktery prokazal ze zde neni vyznamna
# souvislost protoze p-hodnota testu je 0.5684 coz je vetsi nez hodnota 0.05 kterou pouzivame pro hladinu vyznamnosti


# Ukol 4
dt <- read.csv(file = "2022_projekt.csv")
dt <- dt |> dplyr::select(
  -gender,
  -vegetarian,
)
View(dt)
M <- dt |> dplyr::select(-political_aff) |> as.matrix()
M.centered <- scale(M, center = TRUE, scale = FALSE)
M.standardized <- scale(M, center = TRUE, scale = TRUE)

# Coefficients in the first two principal components
#PC1 PC2
pca <- prcomp(M, center = TRUE, scale. = TRUE)
pca$rotation
pca$x
pca$sdev
sum(pca$sdev^2)
loadings <- t(pca$rotation) * pca$sdev
loadings

ggbiplot(pca, choices = c(1, 2), obs.scale = 1, var.scale = 1, labels = rownames(M)) + 
  geom_point(aes(color = dt$political_aff), size = 2) +
  scale_color_discrete(name = "Political Affiliation", label = c("Democrat", "Independent", "Republican"))



dt.var <- data.frame(
  r = seq_along(pca$sdev), 
  var = (pca$sdev**2) / sum(pca$sdev**2), 
  cumvar = cumsum(pca$sdev**2) / sum(pca$sdev**2)
)

ggplot(dt.var, aes(x = factor(r), y = var, group = 1)) + 
  geom_line(aes(y = var), size = 0.5, linetype = "dotted", color = "blue") + 
  geom_point(aes(y = var), color = "blue", size = 2.5) + 
  geom_line(aes(y = cumvar), size = 0.5, linetype = "dotted", color = "red") + 
  geom_point(aes(y = cumvar), color = "red", size = 2.5) + 
  labs(x = "number of PCs", y = "variance explained")

dt.var <- data.frame(
  r = seq_along(pca$sdev), 
  var = (pca$sdev**2) / sum(pca$sdev**2), 
  cumvar = cumsum(pca$sdev**2) / sum(pca$sdev**2)
)

dt.var |> filter(cumvar >= 0.8)    # First 3 PCs explain at least 80 % of variance of data
dt.var |> filter(var >= mean(var)) # Kaiser's rule: take first 5 PCs  

dt.var


# Compare with correlation matrix of pricnipal components
cor(pca$x)
det(cor(pca$x))
w <- eigen(cor(pca$x))
w$values
w$vectors
U <- w$vectors
t(U) %*% U


# Ukol 5

dt <- read.csv(file = "2022_projekt.csv")
dt <- subset(dt, select = c("gender", "high_sch_GPA", "TV"))

# Convert rows to factors
dt <- dt |> dplyr::mutate(
  freetime = 168 - TV,
  gender = factor(gender)
)

str(dt)
summary(dt)
n <- nrow(dt)
alpha <- 0.05
q <- qnorm(1 - alpha / 2)

ggplot(dt, aes(x = TV, y = high_sch_GPA, color=gender)) + 
  geom_point(size = 2.0) + 
  labs(x = "TV", y = "High school GPA")

   # high_sch_GPA = seq(min(dt$high_sch_GPA), max(dt$high_sch_GPA)  high_sch_GPA = seq(min(dt$high_sch_GPA), max(dt$high_sch_GPA)
model.logit <- glm(cbind(freetime, TV) ~ high_sch_GPA + gender, data = dt, family = binomial(link = "logit"))
summary(model)

x.new <- data.frame(
  high_sch_GPA = seq(min(dt$high_sch_GPA), max(dt$high_sch_GPA), by = 0.0015),
  gender = rep(c("male", "female"), each = 1334)
)


xfit.logit <- predict(model.logit, x.new, type = "response", se.fit = FALSE) |> 
  as.data.frame() |> 
  cbind(x.new) |> 
  mutate(model = "logit")

eta.logit <- predict(model.logit, x.new, type = "link", se.fit = TRUE) |>
  as.data.frame() |> 
  cbind(x.new) |> 
  transmute(
    model = "logit", 
    high_sch_GPA = high_sch_GPA, 
    gender = gender, 
    eta = fit,                               # linear predictor
    eta.lower = fit - q * se.fit,            # lower bound ( 2.5%) for linear predictor
    eta.upper = fit + q * se.fit,            # upper bound (97.5%) for linear predictor
    fit = 1 / (1 + exp(-eta)),               # fit
    fit.lower = 1 / (1 + exp(- eta.lower)),  # lower bound ( 2.5%) for fit
    fit.upper = 1 / (1 + exp(- eta.upper))   # upper bound (97.5%) for fit
  )

ggplot() + 
  geom_point(data = dt, mapping = aes(x = high_sch_GPA, y = prop), size = 2.0) + 
  geom_ribbon(data = eta.logit, mapping = aes(x = high_sch_GPA, ymin = fit.lower, ymax = fit.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logit, aes(x = high_sch_GPA, y = fit), color = "darkviolet", size = 1.0) + 
  labs(x = "high_sch_GPA", y = "probability")

ggplot() + 
  geom_ribbon(data = eta.logit, mapping = aes(x = high_sch_GPA, ymin = eta.lower, ymax = eta.upper), size = 0.5, alpha = 0.2, fill = "darkviolet", color = "darkviolet", linetype = "dashed") + 
  geom_line(data = eta.logit, aes(x = high_sch_GPA, y = eta), color = "darkviolet", size = 1.0) + 
  labs(x = "high_sch_GPA", y = "linear predictor")


 
# model.logit <- glm(cbind(killed, survived) ~ dose, data = dt, family = binomial(link = "logit"))
# summary(model.logit)

