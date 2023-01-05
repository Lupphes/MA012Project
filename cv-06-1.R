library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("rgl")			  	# 3D graphics
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")

dt <- read.csv2("data/households.csv", header = TRUE, skip = 5)
dt <- dt |> rename("members" = "N", "income" = "I", "expense" = "E")
str(dt)
summary(dt)
M <- as.matrix(dt)

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
cov(M)
# Correlation matrix
cor(M)
# or better, using Hmisc library
R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# E.g., Pearson's correlation between expense and income
cor(dt$expense, dt$members)
cor.test(dt$expense, dt$members) # Test of (un)correlation
R$r["expense", "members"]
R$P["expense", "members"]

# Correlogram
plot1 <- ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
  lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot1

# Partial correlations for all pairs, excluding all other variables
# using ppcor library
R.partial <- pcor(M)
R.partial$estimate
R.partial$p.value

plot2 <- ggcorrplot::ggcorrplot(R.partial$estimate, p.mat = R.partial$p.value, title = "Partial Pearson's correlations", 
  lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot2
gridExtra::grid.arrange(plot1, plot2, ncol = 2, nrow = 1)



#	Multiple linear regression
#	Model
model <- lm(expense ~ income + members, data = dt)
summary(model)

#	Prediction of expanse
income <- seq(0, 150, by = 0.1)
members <- seq(1, 5, by = 0.1)
grid.2d <- expand.grid(income = income, members = members)
prediction <- predict(model, grid.2d, interval = "prediction")
expense.hat <- matrix(prediction[,1], nrow = length(income), ncol = length(members)) # reshaped as matrix consistent with lengths of income and members

#	3D graphics
open3d()
colors <- terrain.colors(length(expense.hat))[rank(expense.hat)]
# colors <- rainbow(length(expense.hat))[rank(expense.hat)]
persp3d(income, members, expense.hat, 
  col = colors, lit = FALSE, front = "fill", back = "fill", smooth = FALSE, alpha = 0.9, xlab = "income", ylab = "members", zlab = "expense")
spheres3d(dt$income, dt$members, dt$expense, col = "black", radius = 2, lit = FALSE)
rgl.close()

#	Mutiple correlation between expense and (income, members)
model.M <- lm(expense ~ income + members, data = dt)
cor(dt$expense, fitted.values(model.M)) # by definition, as correlation between variable and its best linear approximation
sqrt(summary(model.M)$r.squared)        # or, as square root of R squared

#	Partial correlation between expense and members when income excluded
R.partial$estimate["expense", "members"] # from the partial-correlation matrix
model.M1 <- lm(expense ~ income, data = dt)
model.M2 <- lm(members ~ income, data = dt)
cor(residuals(model.M1), residuals(model.M2)) # by definition, as correlation between residuals of two models
sqrt(( summary(model.M)$r.squared - summary(model.M1)$r.squared ) / ( 1 - summary(model.M1)$r.squared )) # or, using relationship with R squared



# Spearman's correlations 

R.S <- rcorr(M, type = "spearman")
R.S$r
R.S$P
diag(R.S$P) <- 0

# E.g., Spearman's correlation between expense and income
cor(dt$expense, dt$members, method = "spearman")
cor.test(dt$expense, dt$members, method = "spearman") # Test of rank order
R.S$r["expense", "members"]
R.S$P["expense", "members"]

# Spearman's correlation by definition
cor(rank(dt$expense), rank(dt$members))

# Correlogram
plot1S <- ggcorrplot::ggcorrplot(R.S$r, p.mat = R.S$P, title = "Spearman's correlations", 
  lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot1S

# Partial Spearman's correlations 
R.S.partial <- pcor(M, method = "spearman")
R.S.partial$estimate
R.S.partial$p.value

plot2S <- ggcorrplot::ggcorrplot(R.S.partial$estimate, p.mat = R.S.partial$p.value, title = "Partial Spearman's correlations", 
  lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot2S
gridExtra::grid.arrange(plot1S, plot2S, ncol = 2, nrow = 1)
gridExtra::grid.arrange(plot1, plot2, plot1S, plot2S, ncol = 2, nrow = 2)



# Kendall's tau 
cor(dt$expense, dt$members, method = "kendall")
cor.test(dt$expense, dt$members, method = "kendall") # Test of ordinal association

# find all concordant and discordant pairs
indexes <- combn(nrow(dt), 2) # all combinations of row indexes
combined <- cbind(
    dt[indexes[1, ], ] |> rename_with(.fn = \(x){paste(x, "1", sep = "")}), # select rows by 1st index, and rename columns
    dt[indexes[2, ], ] |> rename_with(.fn = \(x){paste(x, "2", sep = "")})  # select rows by 2nd index, and rename columns
  ) |> 
  mutate(
    concordant = (members1 < members2 & expense1 < expense2) | (members1 > members2 & expense1 > expense2), 
    discordant = (members1 < members2 & expense1 > expense2) | (members1 > members2 & expense1 < expense2) 
  )
View(combined)
n.0 <- nrow(combined)
n.concordant <- sum(combined$concordant)
n.discordant <- sum(combined$discordant)
print(data.frame(n.0, n.concordant, n.discordant))
# tau, uncorrected for ties)
(n.concordant - n.discordant) / n.0
# corrections for ties
(t.1 <- table(dt$expense))
(n.1 <- sum(t.1 * (t.1 - 1) / 2))
(t.2 <- table(dt$members))
(n.2 <- sum(t.2 * (t.2 - 1) / 2))
(n.concordant - n.discordant) / (sqrt(n.0 - n.1) * sqrt (n.0 - n.2))

