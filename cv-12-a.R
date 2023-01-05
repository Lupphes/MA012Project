library("boot")
library("tidyr")
library("dplyr")
library("ggplot2")

#################################################################################
# Task 1

# bootstrap sample
y <- c(10, 15, 25, 37, 48, 23, 44, 19, 32, 20)
set.seed(30)
indices <- sample(seq_along(y), replace = TRUE)
y.star <- y[indices]
y.star

# or
set.seed(30)
sample(y, replace = TRUE)

# bootstrap sample and bootstrap parameter estimate
fn <- function(data, indices) {
  sample <- data[indices]
  return(mean(sample))
  }
fn(y, indices)

# 
set.seed(10)
n <- 10
y <- rnorm(n)

theta <- mean(y)
theta.SD <- sqrt(var(y) / n)
print(data.frame(theta, theta.SD))

B <- 200
bs <- sapply(1:B, \(b) {sample(y, replace = T)}) |> 
	t() |> 
	as.data.frame() |> 
	rowwise() |> 
	dplyr::mutate(theta = mean(c_across(everything())))
bs

theta.star.mean <- mean(bs$theta)
theta.star.sd <- sd(bs$theta)
print(data.frame(theta.star.mean, theta.star.sd))

#################################################################################
# Task 2

n <- 200
y <- runif(n, min = -10, max = 10)
theta <- mean(set$y)
theta

B <- 1e4
fn <- function(data, indices) {
  sample <- data[indices]
  return(mean(sample))
  }

bs <- boot(y, fn, B)
bs

bs$t0
bs$t

ci <- boot.ci(bs, type = "all")
ci
ci$normal
ci$basic
ci$percent
ci$bca

db <- data.frame(Z = as.vector(bs$t[, 1]))
q <- quantile(db$Z, probs = c(0.025, 0.975))
ggplot(db, aes(x = Z, y = ..density..)) + 
	geom_histogram(fill = "#FFCC00", color = "black", bins = ceiling(sqrt(B))) + 
	geom_vline(xintercept = theta, color = "red", size = 1.0) + 
	geom_vline(xintercept = q, color = "red", linetype = "dashed")
ggplot(db, aes(x = Z)) + 
	stat_ecdf(color = "black", size = 1.0) + 
	geom_vline(xintercept = q, color = "red", linetype = "dashed")
ggplot(db, aes(sample = Z)) + 
	geom_qq(distribution = "qnorm") + 
	geom_qq_line(distribution = "qnorm", color = "red")

