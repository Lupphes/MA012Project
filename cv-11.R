#	================================================================================
# 1 

emp.cetnosti <- matrix(c(50, 30, 10, 50, 30, 50, 20, 10, 10, 20, 30, 50), nrow = 3, ncol = 4, byrow = TRUE)
rownames(emp.cetnosti) <- c("U", "T", "E")
colnames(emp.cetnosti) <- c("I", "II", "III", "IV")
emp.cetnosti
(n <- sum(emp.cetnosti))
(r <- nrow(emp.cetnosti))
(s <- ncol(emp.cetnosti))
addmargins(emp.cetnosti)
radkove.soucty <- margin.table (emp.cetnosti, 1)
sloupcove.soucty <- margin.table (emp.cetnosti, 2)

library("gplots")
balloonplot(as.table(emp.cetnosti), show.margins = FALSE, cum.margins = FALSE)
mosaicplot(emp.cetnosti, shade = TRUE)

teor.cetnosti <- radkove.soucty %*% t(sloupcove.soucty) / n
teor.cetnosti

K <- sum((emp.cetnosti - teor.cetnosti)^2 / teor.cetnosti)
K
qchisq(0.95, (r-1)*(s-1))
K >= qchisq(0.95, (r-1)*(s-1))
#	hypotezu o nezavislosti na hladine 0,05 zamitame
# nebo
chisq.test(emp.cetnosti)

# Crameruv koeficient
sqrt(K / n / (min(r,s) - 1))

#	================================================================================
#	2

# Nejakej vypocet aha


#	================================================================================
#	3 

emp.cetnosti <- matrix(c(17, 11, 39, 58), nrow = 2, ncol = 2, byrow = TRUE)
rownames(emp.cetnosti) <- c("prijati", "neprijati")
colnames(emp.cetnosti) <- c("dobry dojem", "spatny dojem")
emp.cetnosti
n <- sum(emp.cetnosti)
r <- nrow(emp.cetnosti)
s <- ncol(emp.cetnosti)
addmargins(emp.cetnosti)
radkove.soucty <- margin.table(emp.cetnosti, 1)
sloupcove.soucty <- margin.table(emp.cetnosti, 2)

# pomer sanci
OR <- (emp.cetnosti[1,1] * emp.cetnosti[2,2]) / (emp.cetnosti[1,2] * emp.cetnosti[2,1])
OR
log(OR)

# test podilem sanci 
D <- log(OR) / sqrt(sum(1 / emp.cetnosti))
D
qnorm(0.975)
abs(D) >= qnorm(0.975)
#	hypotezu o nezavislosti na hladine 0,05 nezamitame

# 95% interval spolehlivosti pro prirozeny logaritmus podilu sanci
int.log <- log(OR) + c(-1,1) * qnorm(0.975) * sqrt(sum(1 / emp.cetnosti))
int.log

# 95% interval spolehlivosti pro podil sanci
exp(int.log)

# jiny test pomoci statistiky K
teor.cetnosti <- radkove.soucty %*% t(sloupcove.soucty) / n
teor.cetnosti
K <- sum ((emp.cetnosti - teor.cetnosti)^2 / teor.cetnosti)
K
qchisq (0.95, (r-1)*(s-1))
K >= qchisq (0.95, (r-1)*(s-1))
#	hypotezu o nezavislosti na hladine 0,05 nezamitame
# nebo
chisq.test (emp.cetnosti)

# Crameruv koeficient
sqrt (K / n / (min(r,s) - 1))

# Fisheruv faktorialovy (exaktni) test 
fisher.test(emp.cetnosti)



#	================================================================================
#	4 

R <- c (4, 1, 6, 5, 3, 2, 7)
S <- c (4, 2, 5, 6, 1, 3, 7)
n <- length (R)

r.S <- 1 - 6 / (n * (n^2 - 1)) * sum ((R - S)^2)
r.S
# vsimnete si, ze r.S lze pocitat take jako vyberovou Pearsonovu korelaci mezi vektory poradi
cor (R, S)

# test poradove nezavislosti 
library("pspearman")
spearman.test (R, S, approximation = "exact")
spearman.test (R, S, approximation = "AS89")
spearman.test (R, S, approximation = "t-distribution")

# test vyznamnosti r.S
cor.test (R, S, method = "spearman")


#	================================================================================
#	5 

x <- c(80, 50, 36, 58, 42, 60, 56, 68)
y <- c(65, 60, 35, 39, 48, 44, 48, 61)
data <- cbind(x, y)
n <- length(x)
# overeni 2rozmerneho normalniho rozdeleni
library("mixtools")
plot(data, type = "p", xlab = "body 1. test", ylab = "body 2. test", xlim = c (0, 100), ylim = c (0, 100))
ellipse(mu = colMeans(data), sigma = cov(data), alpha = 0.05, npoints = 100, col = "red")
ellipse(mu = colMeans(data), sigma = cov(data), alpha = 0.10, npoints = 100, col =  "green")
ellipse(mu = colMeans(data), sigma = cov(data), alpha = 0.01, npoints = 100, col = "blue")

# korelace
cor(x, y)
# test
test <- cor.test(x, y, alternative = "greater")
# kvantil
qt(0.95, n - 2)

test$statistic > qt(0.95, n - 2)
#	================================================================================


#	================================================================================
#	6

s1 = 0.85
s2 = 0.9

s1^2 / s2^2

qt(0.95, n - 2)
