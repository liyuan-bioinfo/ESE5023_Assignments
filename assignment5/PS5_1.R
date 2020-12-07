#Author:Li-Yuan
#Date:20201201
#I got inspired by reading Section10 and having communicating with Doc.Sun.


library("sp")
library("raster")
library("sf")
library("rgdal")


setwd("C:\\Users\\Len\\Desktop\\ESE")
#1. Potential Renewable Energy Spots in China
#----------------------------------------------------#
# 1.1 Download the following data sets and load them in R:
#----------------------------------------------------#
wc_wind_2.5m_11 <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_11.tif")
wc_srad_2.5m_11 <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_11.tif")
wc_prec_2.5m_11 <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_11.tif")

China_map_crop <- readOGR("C:\\Users\\Len\\Desktop\\ESE\\China_map", "bou2_4p") 
# Crop the raster with china map
Wind_Nov_crop <- crop(wc_wind_2.5m_11, China_map_crop)
Srad_Nov_crop <- crop(wc_srad_2.5m_11, China_map_crop)
Prec_Nov_crop <- crop(wc_prec_2.5m_11, China_map_crop)

#----------------------------------------------------#
#1.2 Plot the above data sets over China. You should make three plots, each should contain its own legend.
#----------------------------------------------------#

plot(Wind_Nov_crop,  main="Wind speed in Nov.")
plot(Srad_Nov_crop,  main="Srad speed in Nov.")
plot(Prec_Nov_crop,  main="prec speed in Nov.")

#----------------------------------------------------#
#1.3 Define a reasonable wind speed as the threshold, and describe your favorite spots.
#----------------------------------------------------#
#select interest spot
Crop_box <- c(120,130,10,20)
# Crop the raster
Wind_Nov_crop <- crop(wc_wind_2.5m_11, Crop_box)
# Plot cropped DEM
plot(Wind_Nov_crop, main="High Wind speed in Nov.")
Wind_Nov_crop_crop<-crop(Wind_Nov_crop, China_map_crop)

#----------------------------------------------------#
#1.4  Search for regions with relatively high solar radiation and 
#   low precipitation as potential locations of photovoltaics (PV) farms. 
#     Describe your favorite spots of PV farms.
#----------------------------------------------------#
Crop_box_high_srad <- c(75,120,10,25)
Crop_box_low_prec <- c(75,120,15,40)
Srad_Nov_crop <- crop(Srad_Nov_crop, Crop_box_high_srad)
Prec_Nov_crop <- crop(Srad_Nov_crop, Crop_box_low_prec)
plot(Prec_Nov_crop, main="High Srad and low precipitation in Nov.")
Wind_Nov_crop_crop<-crop(Prec_Nov_crop, China_map_crop)
