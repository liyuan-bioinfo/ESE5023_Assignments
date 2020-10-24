#author：LiYuan
#assignment3

###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("dplyr")
library("ggplot2")
library(tidyr)
setwd("../assignment3/")

###################################################
#           2. build datasets
###################################################
rainfall_unseed<- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 
                    163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 
                    36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 
                    4.9, 4.9, 1.0)
rainfall_seed<-c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 
                 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 
                 198.6, 129.6, 119.0, 
                 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
rainfall_data_tbl<-as_tibble(data.frame(rainfall=c(rainfall_unseed,rainfall_seed),
                          seed=(c(rep(0,26),rep(1,26)))
                          )) %>%
  mutate(seed = factor(seed, ordered = TRUE)) 

###################################################
#           3. plot two box plot3
###################################################
#assignment3-1.1 Plot two box plots side-by-side of data from the two groups.
rainfall_data_tbl %>% 
ggplot( aes(x = seed, y = rainfall, fill = seed)) +
  geom_boxplot() +
  theme_classic()

###################################################
#           4. one way anova
###################################################
#assignment3-1.2 Did cloud seeding have an effect on rainfall in this experiment?
anova_one_way <- aov(rainfall ~ seed, data = rainfall_data_tbl)
summary(anova_one_way) #no significant
