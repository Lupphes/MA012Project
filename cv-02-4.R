library("dplyr")
library("ggplot2")
library("agricolae")
library("car")
library("nortest")

a <- 5
n <- list(5,7,6,8,5)
s_t <- 15
s_e <- 3

sum <- 5+7+6+8+5

s_a <- s_t - s_e
s_a

df_a <- a - 1
df_e <- 31 - a

Ms_a <- s_a / df_a
Ms_e <- s_e / df_e


f_a <- Ms_a / Ms_e
f_a
