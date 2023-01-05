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

dat <- read.csv2(file = "data/potatoes.csv")
