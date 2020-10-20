#

######################################
#       1, initial the params
######################################
rm(list=ls())
getwd()
#setwd("C:/Users/Len/Desktop/PS/")
library("dplyr")
library("ggplot2")

######################################
#       2, main
######################################
#1. Significant earthquakes since 2150 B.C.
#The Significant Earthquake Database contains information on destructive earthquakes 
#from 2150 B.C. to the present. 

##1.1 [5 points] Read the .txt (or .tsv) file (signif.txt) with R and 
##convert it to a tibble object named Sig_Eqs.

Sig_Eqs<-read.table(file = "signif.txt",sep="\t",header=TRUE,stringsAsFactors = F,quote = "")
Sig_Eqs <- as_tibble(Sig_Eqs)

##1.2 [5 points] Compute the total number of deaths caused by earthquakes 
##since 2150 B.C. in each country, 
##and then print the top ten countries along with the total number of deaths.
Sig_Eqs %>% group_by(COUNTRY) %>% 
  summarize(total_num_death = sum(TOTAL_DEATHS,na.rm = T)) %>% 
  arrange(desc(total_num_death)) %>% 
  head(10)

##1.3 [10 points] Compute the total number of earthquakes with magnitude larger than 6.0 
##(use column EQ_PRIMARY as the magnitude) worldwide each year, and then plot the time series. 
##Do you observe any trend? Explain why or why not?
Sig_Eqs %>% filter(EQ_PRIMARY >= 6.0) %>% group_by(YEAR) %>%  
  summarize(total_num_eq =n()) %>% 
  arrange(desc(YEAR)) %>% 
  ggplot(aes(x=YEAR, y=total_num_eq)) + 
  geom_line()
  
##1.4 [10 points] Write a function CountEq_LargestEq that returns both (1) 
##the total number of earthquakes since 2150 B.C. 
##in a given country AND (2) the date of the largest earthquake ever happened in this country. 
##Apply CountEq_LargestEq to each country, report your results in a descending order.
CountEq_LargestEq <- function(country){
  output <- c(  ##1count the total number
                Sig_Eqs %>% filter(COUNTRY == country) %>% 
    summarize(total_num_eq =n()))
                ##2 date of the largest earchquake ever happened in this country.  
  date_country <- Sig_Eqs %>% filter(COUNTRY == country) %>% 
    arrange(desc(EQ_PRIMARY)) %>% 
    select(YEAR,MONTH,DAY,HOUR,MINUTE,SECOND) %>% 
    head(1)
  output <- c(output, date_country$YEAR)
  return(output)
  
}


num_eq <- c()
date_eq <- c()
country_eq <-c()
#use for cycle to iterate every country
for(i in unique(Sig_Eqs$COUNTRY)){
  r <- CountEq_LargestEq(i)
  num_eq <- c(num_eq,r[1])
  date_eq <- c(date_eq,r[2])
  country_eq <- c(country_eq,i)
}
x<-as.numeric(date_eq)
y<-as.numeric(num_eq)

#report data and save to plain file
report_data <- as_tibble(data.frame(year=x,sum=y,country=country_eq)) %>% 
  arrange(desc(sum))
write.table(report_data,file="PS2_1_report.txt",sep="\t",row.names = TRUE)

