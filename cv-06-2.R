library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("rgl")			  	# 3D graphics
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")

dt <- read.csv2("data/children.csv", header = TRUE)
dt <- subset (dt, select = -ID)
str(dt)
summary(dt)
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
# Correlation matrix
# or better, using Hmisc library
R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# Test of (un)correlation
R$r["Points", "Weight"]
R$P["Points", "Weight"]

# Correlogram
plot1 <- ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

# Partial correlations for all pairs, excluding all other variables
# using ppcor library
R.partial <- pcor(M)
R.partial$points
R.partial$p.value

plot2 <- ggcorrplot::ggcorrplot(R.partial$estimate, p.mat = R.partial$p.value, title = "Partial Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

gridExtra::grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

# Variance-covariance matrix
cov(M)
# Correlation matrix
cor(M)
# or better, using Hmisc library
R <- rcorr(M)
R$r
R$P
diag(R$P) <- 0

# Spearman's correlations 

R.S <- rcorr(M, type = "spearman")
R.S$r
R.S$P
diag(R.S$P) <- 0

# E.g., Spearman's correlation
R$r["Points", "Weight"]
R$P["Points", "Weight"]

# Correlogram
plot1S <- ggcorrplot::ggcorrplot(R.S$r, p.mat = R.S$P, title = "Spearman's correlations", 
                                 lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

# Partial Spearman's correlations 
R.S.partial <- pcor(M, method = "spearman")
R.S.partial$estimate
R.S.partial$p.value

plot2S <- ggcorrplot::ggcorrplot(R.S.partial$estimate, p.mat = R.S.partial$p.value, title = "Partial Spearman's correlations", 
                                 lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)

gridExtra::grid.arrange(plot1S, plot2S, ncol = 2, nrow = 1)
gridExtra::grid.arrange(plot1, plot2, plot1S, plot2S, ncol = 2, nrow = 2)


#	Mutiple correlation between expense and (income, members)
model.M <- lm(Points ~ Weight + Age, data = dt)
cor(dt$Points, fitted.values(model.M)) # by definition, as correlation between variable and its best linear approximation
sqrt(summary(model.M)$r.squared)        # or, as square root of R squared
