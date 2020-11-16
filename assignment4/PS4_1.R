#author：Liyuan
#8. 1. Plotting with ggplot2


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("ggplot2")
library("dplyr")
data_tibble<-as_tibble(read.table("PS4_1.data.txt",header=T))
data_tibble$label
sample1<-data_tibble %>% filter(label=="Normal" )
sample2<-data_tibble %>% filter(label=="PDAC" )
sample3<-data_tibble %>% filter(label=="Breast" )

########################################################
# Boxplot/Time series/Histogram/Scatter plot/Image plot
########################################################
#aesthetics, legend, panel, axis, title, theme, 
#style, text, annotation, map, … ) 
#included and the level of sophistication
#1.1. Boxplot
ggplot(data_tibble, aes(x = label, y = Q9HBB8 , color=label)) +
  geom_boxplot() +
  theme_classic()+
  labs(title="Q9HBB8 expression in different cancers", 
       x="Cancer type", y="expression of protein") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20)) +
  scale_color_discrete(name="Cancer type")
#1.2. Time series

#1.3. Histogram
hist(x = sample1[,-1]$Q9HBB8,
     xlab = "Expression of Q9HBB8",
     ylab = "Frequency",
     main = "Frequency of expression of Q9HBB8",
     pch = "+",
     cex = 2,
     col = "navy",)
#ggplot( data=sample1$A8MUM7,) +
  geom_histogram()+
  theme_classic()+
  labs(title="Q9HBB8 expression in different cancers", 
       x="Cancer type", y="expression of protein") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))
  #scale_color_discrete(name="Cancer type")
#1.4. Scatter plot
plot(Q9HBB8 ~ Q9NY25, data=data_tibble,
     xlab = "Q9NY25",
     ylab = "Q9HBB8",
     main = "Q9HBB8 vs Q9NY25",
     pch = "+",
     cex = 2,
     col = "navy")
#1.5. Image plot
library(fields); library(maps); library(RNetCDF)
x<- 1:10
y<- 1:15
image.plot(x, y, outer( x,y,"+") )
