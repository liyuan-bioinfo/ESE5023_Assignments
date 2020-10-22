#author: LiYuan
#time: 20200930
#task: 


#Details
## Hourly visibility data are stored under the VIS column. 
##Read page 10 of the comprehensive user guide for the detailed format of the data
rm(list = ls())
setwd("C:\\Users\\Len\\Desktop\\PS")
data <- read.csv("./2281305/2281305.csv",header = T)

##clean the data
#str ="004000,1,N,1"
index_vis <- c()
for(i in 1:length(data$VIS))
  #1,满足1,N,1
  if(length(grep("^[0-9]+,1,N,1$",data$VIS[i])) == 1){
    #2,区间在0-160000
    tmp_vis_int <- as.integer(strsplit(as.character(data$VIS[i]),',1,N,1'))
    if(tmp_vis_int >=0 && tmp_vis_int <=160000){
      #保存当前索引
      index_vis <- c(index_vis,i)
    }
  }

data_clean <- data[index_vis,]
data_clean$vis_data <-as.integer(strsplit(as.character(data_clean$VIS),',1,N,1'))

##6.1 Plot hourly visibility data as a function of the observation time, 
##similar to the time series plot you made in Exercise #3. Based on your naked eyes, is there a trend in visibility during the past 10 years?

plot(as.Date(data_clean$DATE),data_clean$vis_data,lwd=0.1,type="l",col="blue")


##6.2 For each year, report the number of days with visibility within the respective interval

tmp <- as.Date(data_clean$DATE)
year <- c()
for(i in 1:length(tmp)){
  year <- c(year,strsplit(as.character(tmp[i]),"-")[[1]][1])
}
data_clean$year <- year

data_clean$year_day <- as.Date(data_clean$DATE) #为了合并day
#install.packages("sqldf")
#需要解决分类汇总问题
library("sqldf")
new_data <- sqldf("select year_day,year,max(vis_data) as vis_data from data_clean group by year_day")
max(new_data$vis_data) #34100
#将日期转化成年份

#进行不同年份区间的计数
#y代表不同区间


#data_clean$vis_data
labels <- c(2010,2011,2012,2013)
ranges <- c("[0,5km)","[5km,10km)","[10km,15km)",
          "[15km,20km)","[20km,25km)","[25km,30km)","[30km,>30km]")
c1 <- 1:7
c2 <- 1:7  
c3 <- 1:7  
c4 <- 1:7  
 


df <- data.frame(c1,c2,c3,c4)
row.names(df) <- ranges
colnames(df)<-labels


data_plot <- c()
#max(data_clean$vis_data) #30000
mydata.cut<-cut(new_data$vis_data[which(new_data$year==2010)],breaks=seq(0,30001,5000),right = F)
df$`2010`<-as.integer(summary(mydata.cut))
mydata.cut<-cut(new_data$vis_data[which(new_data$year==2011)],breaks=seq(0,30001,5000),right = F)
df$`2011`<-as.integer(summary(mydata.cut))
mydata.cut<-cut(new_data$vis_data[which(new_data$year==2012)],breaks=seq(0,30001,5000),right = F)
df$`2012`<-as.integer(summary(mydata.cut))
mydata.cut<-cut(new_data$vis_data[which(new_data$year==2013)],breaks=seq(0,30001,5000),right = F)
df$`2013`<-as.integer(summary(mydata.cut))


#使用不同区间的数值进行柱状图的绘制
cols <- terrain.colors(6)
barplot(as.matrix(df),  offset = 0, axis.lty = 1, names.arg = labels, col = cols,
        beside = TRUE, legend.text =ranges, xlab = "year",ylab="the number of days")



