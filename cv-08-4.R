library("Hmisc")		  	#	rcorr
library("ppcor")		  	#	pcor
library("GGally")     # ggpairs
library("ggcorrplot") # ggcorrplot
library("gridExtra")  # grid.arrange
library("dplyr")
library("ggplot2")
# library("devtools")
# install_github("vqv/ggbiplot")
library("ggbiplot")

dt <- read.csv2("data/heptathlon.csv")
str(dt)
summary(dt)
View(dt)

rownames(dt) <- dt$X

M <- dt |> dplyr::select(-score, -X) |> as.matrix()
M.centered <- scale(M, center = TRUE, scale = FALSE)
M.standardized <- scale(M, center = TRUE, scale = TRUE)

dt |> dplyr::summarise(across(
  everything(), 
  list(mean = mean, stdev = sd),
  na.rm = TRUE, 
  .names = "{.col}.{.fn}"
))


GGally::ggpairs(as.data.frame(M), upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))
GGally::ggpairs(as.data.frame(M.standardized), upper = list(continuous = "points"), lower = list(continuous = "points"), diag = list(continuous = "densityDiag"))

R <- rcorr(M)
diag(R$P) <- 0
R.partial <- pcor(M)
plot1 <- ggcorrplot::ggcorrplot(R$r, p.mat = R$P, title = "Pearson's correlations", 
                                lab = FALSE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
plot2 <- ggcorrplot::ggcorrplot(R.partial$estimate, p.mat = R.partial$p.value, title = "Partial Pearson's correlations", 
                                lab = FALSE, type = "full", method = "square", outline.color = "white", show.legend = FALSE, pch.cex = 10)
gridExtra::grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

# PCA KOEFICIENTY
pca <- prcomp(M, center = TRUE, scale. = TRUE)
pca$rotation
pca$x
pca$sdev
sum(pca$sdev^2)
loadings <- t(pca$rotation) * pca$sdev

ggbiplot(pca, choices = c(1, 2), labels = rownames(M)) + 
  lims(x = c(-2.5, 2.0), y = c(-2.0, 2.0))

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

