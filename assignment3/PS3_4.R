#author：Liyuan
#4. Atmospheric Lapse Rate


###################################################################
#           1. init parameters
###################################################################
rm(list=ls())
library("dplyr")
library("ggplot2")
library(tidyr)

Elevation<-	c(180,305,381,488,549,640,762,883)
Temperature<-c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)

###################################################################
#   2. fit in lieaer regression and plot the scatter plot
###################################################################
#[15 points] Draw a scatter plot with regression line, 
#and investigate if the lapse rate is 9.8 degrees C km-1.

fit <- lm(Temperature ~ Elevation)
plot(Temperature ~ Elevation,xlab="Elevation(m)",ylab="Temperature(degrees C)",
     pch = 20,
     cex = 2,
     col = "grey")
abline(fit, lwd = 5, col = "red")
summary(fit)$coefficients
