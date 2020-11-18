#author：Liyuan
#Plotting with ggplot2
#The parts of Time Series and Image plot were inspired by TaoTaoSun


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

#-----------------------------------------------------#
#                   1.1. Boxplot
#-----------------------------------------------------#
p1<- ggplot(data_tibble, aes(x = label, y = Q9HBB8 , color=label)) +
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
ggsave("PS4_1_box_plot.png",p1)
#-----------------------------------------------------#
#                   1.2. Time series
#-----------------------------------------------------#
p2 <- ggplot(data = data_tibble,aes(x = Q9HBB8, y = Q9NY25 )) +
  geom_point(aes(color=label)) + 
  geom_line(aes(color=label)) +
  labs(
    title = "The relationship between two Proteins of different cancers",
    caption = "data origin:lab",
    x = "Q9HBB8",
    y = "Q9NY25")
ggsave("PS4_1_time-series_plot.png",p2)


#-----------------------------------------------------#
#                   1.3. Hist plot
#-----------------------------------------------------#
p3<-ggplot(sample1, aes(x=Q9NY25)) +
  geom_histogram(binwidth = 0.7)+
  theme_classic()+
  labs(title="Q9HBB8 expression in different cancers", 
       x="Cancer type", y="expression of protein") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20))
ggsave("PS4_1_hist_plot.png",p3)
#-----------------------------------------------------#
#                   1.4. Scatter plot
#-----------------------------------------------------#
p4<-data_tibble %>% 
  ggplot( aes(x=Q9HBB8, y=Q9NY25, color=label) ) + 
  geom_point() + 
  geom_smooth() +
  labs(title="Q9HBB8 vs Q9NY25", x="Q9NY25", y="Q9HBB8") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20)) + 
  scale_color_discrete(name="Cancer Type") +
  facet_wrap( ~ label, nrow=2)
ggsave("PS4_1_scatter_plot.png",p4)

#-----------------------------------------------------#
#                   1.5. Image plot
#-----------------------------------------------------#
library(fields); library(maps); library(RNetCDF)
x<- 1:10
y<- 1:15
png("PS4_1_image_plot.png", width=18, height=12, units="cm", res=400) 
image.plot(x, y, outer(x,y,"+") ,
           lwd = 2,
           legend.width = 1,
           horizontal = T,
           xlab="width",
           ylab="length",
           legend.lab = "temperature") 
title(main="Width related to length",
      cex.main=1,font.main=2)

dev.off()
