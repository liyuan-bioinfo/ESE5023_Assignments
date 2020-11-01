#author：Liyuan
#2. Was Tyrannosaurus Rex Warm-Blooded?


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("dplyr")
library("ggplot2")
library(tidyr)
#setwd("ESE5023/assignment3/")
data_tibble<- as_tibble(data.frame(
Rib_16=c(11.10,11.22,11.29,11.49,NA,NA),
  Gastralia=c(11.32,11.40,11.71,NA,NA,NA),
  Gastralia2=c(11.60,11.78,12.05,NA,NA,NA),
  Dorsal_vertebra=c(10.61,10.88,11.12,11.24,11.43,NA),
  Dorsal_vertebra2=c(10.92,11.20,11.30,11.62,11.70,NA),
  Femur=c(11.70,11.79,11.91,12.15,NA,NA),
  Tibia=c(11.33,11.41,11.62,12.15,12.30,NA),
  Metatarsal=c(11.32,11.65,11.96,12.15,NA,NA),
  Phalange=c(11.54,11.89,12.04,NA,NA,NA),
  Proximal_caudal=c(10.93,11.01,11.08,11.12,11.28,11.37),
  Mid_caudal=c(11.35,11.43,11.50,11.57,11.92,NA),
  Distal_caudal=c(11.95,12.01,12.25,12.30,12.39,NA)
))

  anova_one_way <- aov( Rib_16~ Gastralia,data=data_tibble)
  temp_p <- summary(anova_one_way)[[1]][1,5] #pvalue
  
  name <- c("Rib 16","Gastralia" ,"Gastralia2", "Dorsal vertebra", "Dorsal vertebra2", "Femur", "Tibia",
            "Metatarsal", "Phalange", "Proximal caudal", "Mid-caudal", "Distal caudal")
  
square_matrix<- matrix(nrow = 12,ncol=12,dimnames = list(name,name))
for(i in 1:12){
  for(j in 1:12){
    if(i != j){
      anova_one_way <- aov( data_tibble[[i]]~ data_tibble[[j]])
      temp_p <- summary(anova_one_way)[[1]][1,5] #pvalue
    }else{
      temp_p = 1
    }
    square_matrix[i,j] <- temp_p
  }
}
#install.packages("ggcorrplot")
library("ggcorrplot")
ggcorrplot(square_matrix,lab = T,show.legend = F,type = "lower",
           title = "Pvalue between the different bones",
           ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726"))
  