#author: LiYuan
#time: 20201009
#task: 


#Details
##7.1 [5 points] Load the csv, XLS, or XLSX file, and clean possible data points with missing values or bad quality.
rm(list = ls())
setwd("C:/Users/Len/Desktop/PS")
data <- read.csv("MHD-ads_1994.csv",header = T)

data_rm_missing <- data[which(!is.na(data$HFC.134a)),] #rm na
data_rm_badPoint <- data[which(data_rm_missing$HFC.134a<=10),] #rm low quality

##7.2 [5 points] Plot the time series of a certain variable.

plot(data_rm_badPoint$month, data_rm_badPoint$HFC.134a,lwd=0.5,
     xlab = "month of 1994",ylab="HFC.134a", col="red")

##7.3 [5 points] Conduct at least 5 simple statistical 
##checks with the variable, and report your findings.
min(data_rm_badPoint$HFC.134a,na.rm = T) #1.343
max(data_rm_badPoint$HFC.134a,na.rm = T) #9.303
table(data_rm_badPoint$HFC.134a)
mean(data_rm_badPoint$HFC.134a,na.rm = T) #1.874309
median(data_rm_badPoint$HFC.134a,na.rm = T)#1.662

