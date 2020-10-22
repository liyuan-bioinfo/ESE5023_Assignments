#author: LiYuan
#time: 2020-10-20


#Revisit the data set you used in Problem 7 of the Assignment 01. 
#Now use functions from tidyr, dplyr, and ggplot2 packages to:
#[10 points] Reproduce the same time series you made previously in Assignment 01.
############################################################
#                  1, initial parameters
###########################################################
rm(list = ls())
getwd()
library("tidyr")
library("dplyr")
library("ggplot2")

############################################################
#                  2, main function
###########################################################
#2.1 [5 points] Load the csv, XLS, or XLSX file, 
#and clean possible data points with missing values or bad quality
data <- as_tibble(read.csv("MHD-ads_1994.csv",header = T))
clean_data_tibble <- data %>% select(month,day,HFC.134a) %>% 
  filter(!is.na(HFC.134a) & HFC.134a<=99 ) #%>% 
 # mutate(month=as.factor(month))

#2.2 [5 points] Plot the time series of a certain variable.
clean_data_tibble %>%  group_by(day) %>%   
  summarize(Dayly_HFC=mean(HFC.134a,na.rm = TRUE))  %>%
  ggplot(aes(x=day, y=Dayly_HFC)) + 
  geom_line() 

#2.3 Conduct at least 5 simple statistical checks with the variable, 
#and report your findings.
clean_data_tibble %>%  group_by(day) %>%   
  summarize(max_Dayly_HFC=max(HFC.134a),
            min_dayly_HFC=min(HFC.134a,na.rm = T),
            sum_dayly_HFC=sum(HFC.134a,na.rm = T),
            median_dayly_HFC=median(HFC.134a,na.rm = T),
            IQR_dayly_HFC=IQR(HFC.134a,na.rm = T))

#hist plot
            hist(clean_data_tibble$HFC.134a,
                 main = "Histogram of dayly HF",
                 breaks = 20,
                 col = "blue",
                 border = "red")