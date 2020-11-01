#author：Liyuan
#time:20201101


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("dplyr")
library("ggplot2")
library(tidyr)


Pregnant_nonvegetarians<-c(185,189,187,181,150,176,rep(NA,6))
Pregnant_vegetarians<-c(171,174,202,171,207,125,189,179,163,174,184,186)
Nonpregnant_vegetarians<-c(210,139,172,198,177,rep(NA,7))

###################################################
#           2. one-way anova
###################################################
#[10 points] What evidence is there that pregnant vegetarians tend to have lower zinc levels than pregnant nonvegetarians?
one_way_anova<- aov(Pregnant_nonvegetarians ~ Pregnant_vegetarians)
summary(one_way_anova) #0.584

