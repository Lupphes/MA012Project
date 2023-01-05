library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("rgl")			  	# 3D graphics
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")

dt <- read.csv2("data/heptathlon.csv", header = TRUE)
str(dt)
summary(dt)
M <- as.matrix(dt)
rownames(dt) <- dt$Country
M <- dt |> dplyr::select(-X) |> as.matrix()
M.centered <- scale(M, center = TRUE, scale = FALSE)
M.standardized <- scale(M, center = TRUE, scale = TRUE)
View(dt)

#	Scatterplot
GGally::ggpairs(dt, upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

#	Mutiple correlation between expense and (income, members)
model.M <- lm(score ~ run800m + javelin + longjump + run200m + shot+ highjump + hurdles, data = dt)
cor(dt$score, fitted.values(model.M)) # by definition, as correlation between variable and its best linear approximation
sqrt(summary(model.M)$r.squared)        # or, as square root of R squared

# Partial correlations for all pairs, excluding all other variables
# using ppcor library
R.partial <- pcor(M)
R.partial$IQ
R.partial$p.value

plot2 <- ggcorrplot::ggcorrplot(R.partial$estimate, p.mat = R.partial$p.value, title = "Partial Pearson's correlations", 
                                lab = TRUE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot2
gridExtra::grid.arrange(plot1, plot2, ncol = 2, nrow = 1)


# Spearman's correlations 

R.S <- rcorr(M, type = "spearman")
R.S$r
R.S$P
diag(R.S$P) <- 0

# E.g., Spearman's correlation
R$r["IQ", "Reading"]
R$P["IQ", "Reading"]

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
