#author：Liyuan
#5. The Big Bang Theory


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("dplyr")
library("ggplot2")
library(tidyr)
data<-read.table("PS3_5.data",sep="\t",header=T) 
data<-data[,-1]

#5.1 [5 points] Make a scatter plot with distance as the 
#Y-axis and recession velocity as the X-axis. Describe what you see.

fit <- lm(Distance ~ Velocity,data = data)
plot(Distance ~ Velocity,data=data,     
     xlab = "Velocity(km/s)",
     ylab = "Distance(megaparsecs)",
     main = "Distance vs Velocity",
     pch = 20,
     cex = 2,
     col = "grey")
#5.2 [5 points] Add a simple linear regression line to the above scatter plot.
abline(fit, lwd = 5, col = "red")

#5.3 [15 points] If Hubble’s Big Bang Theory is correct, 
#explain why the following two assumptions about the regression 
#line you made in 5.2 need to be true:
##The intercept should be zero
##And the slope is the age of the universe
summary(fit)$coefficients