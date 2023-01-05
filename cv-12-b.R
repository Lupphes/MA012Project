library("MASS")
library("glmnet")
library("dplyr")
library("ggplot2")
library("devtools")
install_github("vqv/ggbiplot")
library("ggbiplot")

dt <- read.csv2(file = "data/expenses.csv")
summary(dt)
str(dt)

x <- as.matrix(dt |> dplyr::select(-Expenses))
y <- as.matrix(dt |> dplyr::select(Expenses))

# LRM
model.lm <- lm(Expenses ~ ., data = dt)
summary(model.lm)
beta.lm <- coef(model.lm)

# Ridge regression
lambda <- exp(seq(-7, 10, by = 0.1))
model.ridge <- lm.ridge(Expenses ~ ., data = dt, lambda = lambda)
beta.ridge <- as.data.frame(coef(model.ridge))
colnames(beta.ridge) <- names(beta.lm)

beta.lm
beta.ridge[1,]

dr <- data.frame(
	lambda = lambda, 
	GCV = model.ridge$GCV
	) |> 
	cbind(beta.ridge)

index <- which.min(GCV)
(lambda.optim <- lambda[index])
(beta.optim <- beta.ridge[index,])

ggplot(dr, aes(x = lambda, y = GCV)) + 
	geom_line(color = "red", size = 1.0) + 
	scale_x_log10() + 
	labs(x = "lambda", y = "GCV") + 
	geom_vline(xintercept = lambda.optim, linetype = "dashed")

dr |> tidyr::pivot_longer(cols = c("(Intercept)", "Members", "Kids", "Age", "Income"), names_to = "variable", values_to = "value") |> 
	ggplot(aes(x = lambda, y = value, color = variable)) + 
	geom_line(size = 1.0) + 
	scale_x_log10() + 
	labs(x = "lambda", y = "regression coefficients") + 
	geom_vline(xintercept = lambda.optim, linetype = "dashed")

# Prediction
Y.predicted.ridge <- scale(x, center = model.ridge$xm, scale = model.ridge$scales) %*% model.ridge$coef[,index] + model.ridge$ym
# Sum of Squares Total and Error
S.T <- sum((y - mean(y))^2)
S.e <- sum((Y.predicted.ridge - y)^2)
# R-squared
(R.2.ridge <- 1 - S.e / S.T)

# Norm of the vector of estimated reression coefficients
dr <- dr |> mutate(
	norm = sqrt(Members^2 + Kids^2 + Age^2 + Income^2)
	)

ggplot(dr, aes(x = lambda, y = norm)) + 
	geom_line(color = "maroon", size = 1.0) + 
	scale_x_log10() + 
	labs(x = "lambda", y = "norm of vector of regression coefficients") + 
	geom_vline(xintercept = lambda.optim, linetype = "dashed")


#
X <- model.matrix(model.lm)
M <- t(X) %*% X
M.optim <- M + lambda.optim * diag(length(beta.optim))
det(M)
det(M.optim)

######################################################################
# using library "glmnet"

lambda <- exp(seq(-1, 13, by = 0.2))
# glmnet: alpha = 0 => ridge regression
model.glmnet <- glmnet(x, y, lambda = lambda, alpha = 0)
str(model.glmnet)

CV.fit <- cv.glmnet(x, y, lambda = lambda, alpha = 0)
str(CV.fit)

plot(CV.fit, xaxp = c(-1, 13, 14))
str(CV.fit$glmnet.fit)
beta.glmnet <- t(coef(CV.fit$glmnet.fit, s = NULL))
(lambda.glmnet.optim <- CV.fit$lambda.min)
(beta.glmnet.optim <- predict(CV.fit, s = lambda.glmnet.optim, type = "coefficients"))

matplot(log(rev(lambda)), beta.glmnet[,-1], type = "l", lty = 1, lwd = 2, col = 2:5, ylab = "beta.ridge", xlab = "log(lambda)", xaxp = c(-1, 13, 14))
abline(h = 0, lty = 2)
legend("topright", colnames(beta.glmnet)[-1], col = 2:5, lty = 1, lwd = 2, ncol = 1, bty = "n", y.intersp = 0.5, inset = c(0, 0))

abline(v = log(lambda.glmnet.optim), lty = 2)
points(rep(log(lambda.glmnet.optim), length(beta.glmnet.optim[-1])), beta.glmnet.optim[-1], col = 2:5, pch = 19)

points(rep(log(lambda[1]), length(beta.lm[-1])), beta.lm[-1], col = 2:5, pch = 19)

# Prediction
Y.predicted.glmnet <- predict(CV.fit$glmnet.fit, s = lambda.glmnet.optim, newx = x)
# Sum of Squares Total and Error
S.T <- sum((y - mean(y))^2)
S.e <- sum((Y.predicted.glmnet - y)^2)
# R-squared
(R.2.glmnet <- 1 - S.e / S.T)

# Norm of the vector of estimated reression coefficients
d.glmnet <- apply(beta.glmnet[,-1], 1, function(x){sum(x^2)})
plot(log(rev(lambda)), d.glmnet, type = "l", lwd = 2, col = "brown", xaxp = c(-1, 13, 14), ylab = "norma odhadu beta", xlab = "log(lambda)")
abline(v = log(lambda.glmnet.optim), lty = 2)
points(log(lambda.glmnet.optim), sum(beta.glmnet.optim[-1]^2), pch = 19, col = "brown")

######################################################################
# LASSO 

lambda <- exp(seq(-1, 10, by = 0.2))
# glmnet: alpha = 1 => LASSO
model.LASSO <- glmnet(x, y, lambda = lambda, alpha = 1)
str(model.LASSO)

CV.LASSO <- cv.glmnet(x, y, lambda = lambda, alpha = 1)
str(CV.LASSO)

plot(CV.LASSO, xaxp = c(-1, 10, 11))
beta.LASSO <- t(coef(CV.LASSO$glmnet.fit, s = NULL))
(lambda.LASSO.optim <- CV.LASSO$lambda.min)
(beta.LASSO.optim <- predict(CV.LASSO, s = lambda.LASSO.optim, type = "coefficients"))

matplot(log(rev(lambda)), beta.LASSO[,-1], type = "l", lty = 1, lwd = 2, col = 2:5, ylab = "beta.ridge", xlab = "log(lambda)", xaxp = c(-1, 10, 11))
abline(h = 0, lty = 2)
legend("topright", colnames(beta.LASSO)[-1], col = 2:5, lty = 1, lwd = 2, ncol = 1, bty = "n", y.intersp = 0.5, inset = c(0, 0))

abline(v = log(lambda.LASSO.optim), lty = 2)
points(rep(log(lambda.LASSO.optim), length(beta.LASSO.optim[-1])), beta.LASSO.optim[-1], col = 2:5, pch = 19)

points(rep(log(lambda[1]), length(beta.lm[-1])), beta.lm[-1], col = 2:5, pch = 19)

# Prediction
Y.predicted.LASSO <- predict(CV.LASSO$glmnet.fit, s = lambda.LASSO.optim, newx = x)
# Sum of Squares Total and Error
S.T <- sum((y - mean(y))^2)
S.e <- sum((Y.predicted.LASSO - y)^2)
# R-squared
(R.2.LASSO <- 1 - S.e / S.T)

# Norma odhadu vektoru parametru
d.LASSO <- apply(beta.LASSO[,-1], 1, function(x){sum(abs(x))})
plot(log(rev(lambda)), d.LASSO, type = "l", lwd = 2, col = "brown", xaxp = c(-1, 13, 14), ylab = "norma odhadu beta", xlab = "log(lambda)")
abline(v = log(lambda.LASSO.optim), lty = 2)
points(log(lambda.LASSO.optim), sum(abs(beta.LASSO.optim[-1])), pch = 19, col = "brown")

################################################################## 

# PCA on the predictors

pca <- prcomp(x, center = TRUE, scale. = TRUE)
pca
summary(pca)

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
  
dt.var |> filter(cumvar >= 0.8)
dt.var |> filter(var >= mean(var))

# Linear model build in PCs 
d.pca <- cbind(y, as.data.frame(pca$x))
model.pca.lm <- lm(Expenses ~ ., data = d.pca)
summary(model.pca.lm)

