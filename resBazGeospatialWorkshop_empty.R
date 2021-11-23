#README 

##Geospatial Exercise script 

#Packages to install - only needs to be run once!
#install.packages(c("dplyr", "terra", "sf", "rasterVis", "ggplot2", "ggspatial"))


# Libraries ---------------------------------------------------------------
#Convenience package
library(dplyr) #Piping

#Geospatial manipulations
library(terra) #Gridded data (primarily) 
library(sf) #Non-gridded data (point, polygon, line) 

#Mapping
library(rasterVis)
library(ggplot2)
library(ggspatial)

#Interaction
library(leaflet)

# sf package --------------------------------------------------------------


## Exercise One Loading sf data --------------------------------------------

#Raster loaded with a relative path (relative to the working directory which is where ever the geospatialResBaz.proj file is)

#1. Read in nz_coastlines shapefile data 

#2. Plot these data


## Exercise Two - sf point data --------------------------------------------
#1. Read in nz_coastlines shapefile data 


#2. Plot these data

## Exercise Three - ggspatial ----------------------------------------------
#1. Add to your map:
#Scale bar
#North arrow

#Plotting sf data

#Adding stuff to make the map pretty

#North arrow

#Saving the map ggsave?
#note: ggsave smooths jagged lines


## Exercise Four - Cropping ------------------------------------------------
#Defining a boundary box (a.k.a. extent)
stew.bbox <- st_bbox(c(xmin = 1155000, #Boundaries
                       ymin = 4723000,  
                       xmax = 1285000, 
                       ymax = 4820000), 
                     class = "bbox", #What it is
                     crs = 2193) #The ESPG code for the CRS

#1. Crop NZ coastline and drosera data layer to match
#Hint:
#Cropping the points
#Cropping the coastline


#2. Plot the cropped sf objects
#plotting the cropped data


## Exercise Five - Leaflet -------------------------------------------------

#1. Read in the drosera.csv file (read.csv)

#2.  Convert df to a leaflet object

#3. Make the species as a pop-up value

#4. Open the viewer


# terra package -----------------------------------------------------------


## Exercise Six - Loading Raster data --------------------------------------
#Raster loaded with a relative path 
#1. Load the “precip_warmQtr.tif”  file assign it to the name precip

#2. Load the “temp_meanColdQtr.tif” and assign it to temp_cold 

#3.  Plot these raster data - gg won't well work here!


## Exercise Seven - Stacks and Projections ---------------------------------
#1. Combine the precip and temp_cold objects into a single object named rastStack

#2. Load “elevation49.tif” (not elevation.tif!) and assign the name elev49

#3. Try to add the elev49 object to rastStack

#Is there a problem?


## Exercise Eight - Raster Algebra -----------------------------------------
#1. Load “temp_meanWarmQtr” and assign it to temp_warm 

#2. Create a raster of the difference between temp_cold & temp_warm and assign it to temp_range


#3. Combine these into a single stack called temps


# Combing sf and terra! ---------------------------------------------------


## Exercise Nine - rasterVis plotting --------------------------------------
#1. Use levelplot() to plot temp_range

#2. Add contours to the map

#3. Use gplot to plot a single layer

