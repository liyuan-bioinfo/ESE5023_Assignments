#author：Liyuan
#7. Analysis of Data Sets from Your Group


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("ggplot")
library("dplyr")
data_tibble<-as_tibble(read.table("PS3_7.data",header=T))
data_tibble$label
sample1<-data_tibble %>% filter(label=="Normal" )
sample2<-data_tibble %>% filter(label=="PDAC" )
sample3<-data_tibble %>% filter(label=="Breast" )

###################################################
#           2. t-test
###################################################
hist(x = sample1[,-1]$Q9HBB8 )
hist(x = sample2[,-1]$Q9HBB8 )
t.test(sample1$Q9HBB8,sample2$Q9HBB8) #0.8365

###################################################
#           3. one-way anova
###################################################
ggplot(data_tibble, aes(x = label, y = Q9HBB8 , fill = label)) +
  geom_boxplot() +
  theme_classic()

anova_one_way <- aov(Q9HBB8 ~ label, data = data_tibble)
summary(anova_one_way)

###################################################
#           4.  linear regression model
###################################################

library(leaps)
subset_result <- regsubsets(label ~ ., data=data_tibble, nbest=2, nvmax = 6,really.big=T)
plot(subset_result, scale="bic")
# best model
data_tibble$label
label_group<-c(rep(0,15),rep(1,15),rep(2,15))
model_log <- lm(label_group ~ O60613+P02746+Q99944+P10619+Q99102+P30511+P48664, data=data_tibble )
summary(model_log) #O60613       P02746         P48664        