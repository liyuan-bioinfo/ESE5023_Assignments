#author：Liyuan
#5. The Big Bang Theory


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("dplyr")
library("ggplot2")
library(tidyr)
library(MASS)
data(cpus)

###################################################
#           2. get train and test set 
###################################################
# Split into two subsets
sample_index <- sample(nrow(cpus),nrow(cpus)*0.8)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]

###################################################
#           3. fit model and predict it 
###################################################

#get the best linear model
#install.packages('leaps')
library(leaps)
subset_result <- regsubsets(perf ~ syct+mmin+mmax+cach+chmin+chmax, data=cpus_train, nbest=2, nvmax = 6)
plot(subset_result, scale="bic")
# best model
model_log <- lm( perf ~ syct+mmin+mmax+cach+chmax, data=cpus_train )

# Get estimates
summary(model_log)

perf_predict <- predict(model_log,cpus_test)

# Compare predicted values with actual values 
plot(cpus_test$perf, perf_predict)

# Correlation coefficients
cor(cpus_test$perf, perf_predict) #0.97

# Mean predicted value
mean(perf_predict) #70.68673

# Mean actual value
mean(cpus_test$perf) #77.5

# Relative mean bias
(mean(perf_predict) - mean(cpus_test$perf))/
  mean(cpus_test$perf)*100 #-8.79132
