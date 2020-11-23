#author：Liyuan
#


###################################################
#           1. init parameters
###################################################
rm(list=ls())
library("sp")
library("raster")
library("sf")
library("rgdal")

setwd("秋季课程/ESE5023/assignment5")
########################################################
#1. Potential Renewable Energy Spots in China
########################################################

#-----------------------------------------------------#
#1.1 [5 points] Download the following data sets and load them in R:
#-----------------------------------------------------#
# Read tiff file
wc_pre_2.5m_11 <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_11.tif")
# Look at the raster attributes
wc_pre_2.5m_11
wc_srad_2.5m_11 <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_11.tif")
wc_wind_2.5m_11 <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_11.tif")


#-----------------------------------------------------#
#1.2 [10 points] Plot the above data sets over China. 
#You should make three plots, each should contain its own legend.
#-----------------------------------------------------#
# Define the crop extent
Crop_box <- c(73,104,27,40)
# Crop the raster
wc_pre_2.5m_11_crop <- crop(wc_pre_2.5m_11, Crop_box)
plot(wc_pre_2.5m_11_crop, main="Precipitation (mm) of 2.5min in Nov.")
# image()
# Set color
col <- terrain.colors(30)
image(wc_pre_2.5m_11,  main="Wind speed in Nov.", col=col)
# Add contour lines
contour(wc_pre_2.5m_11, add=T, col="red")

#-----------------------------------------------------#
#1.3 [5 points] First, let’s search for regions with 
#relatively high wind speed to build wind farms. 
#Define a reasonable wind speed as the threshold, 
#and describe your favorite spots.



#-----------------------------------------------------#
##1.4 [5 points] Second, let’s search for regions 
#with relatively high solar radiation 
#and low precipitation as potential locations of photovoltaics (PV) farms. 
#Describe your favorite spots of PV farms.
#-----------------------------------------------------#
