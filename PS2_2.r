#In this problem set, we will examine how wind speed changes in Shenzhen 
#during the past 10 years. 
#Recall the 2281305.csv you used for Exercise #3 in Section 03. 
#Read page 8-9 of the comprehensive user guide for the detailed format of the wind data. 
#Explain how you filter the data in your report. 
#Use functions from tidyr, dplyr, and ggplot2 packages to:

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

##[10 points] Plot monthly averaged wind speed as a function of the observation time. 
##Is there a trend in monthly averaged wind speed within the past 10 years?
plot_monthly_avg_ws <- function(){
  data_tibble <- as_tibble(read.csv("2281305.csv",header = TRUE))
  
  #get wind rate data
  data_tibble_monthly_wr <- data_tibble %>% mutate(Year=as.double(substr(DATE,1,4)),
                                               Month=as.factor(substr(DATE,6,7)),
                         Wind_Rate_Origin=substr(WND,9,12),
                         Wind_Rate=as.numeric(Wind_Rate_Origin)*0.1,
                         Wind_Flag=substr(WND,14,14)) %>% 
    #clean the data
    filter(Wind_Rate_Origin!=9999 & Wind_Flag==1) %>%
    select(Year,Month,Wind_Rate) %>% 
    group_by(Year,Month)  %>% 
    summarize(Monthly_Wind_Rate=mean(Wind_Rate,na.rm = TRUE))  %>%
    ggplot(aes(x=Year, y=Monthly_Wind_Rate, color=Month)) + 
    geom_line() +
    facet_wrap(~ Month)
}
#for plot
plot_monthly_avg_ws


