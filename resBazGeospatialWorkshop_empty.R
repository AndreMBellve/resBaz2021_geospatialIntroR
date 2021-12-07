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
#1. Read in drosera.shp point data

#2. Transform these to NZGD2000 (ESPG 2193)

#3. Plot points (colour by species) and outline of NZ

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

#Check out this site for a great tool to create bounding boxes by hand!http://bboxfinder.com/

#1. Crop NZ coastline and drosera data layer to match
#Hint:
#Cropping the points


#Cropping the coastline


#2. Plot the cropped sf objects
#plotting the cropped data
#Plotting sf data
  
  #Adding stuff to make the map pretty

  
  #North arrow


## Exercise Five - Leaflet -------------------------------------------------

#1. Read in the drosera.csv file (read.csv)


#2.  Convert df to a leaflet object
 #making a leafet object
 #Adding background map

#4. Open the viewer

# terra package -----------------------------------------------------------

## Exercise Six - Loading Raster data --------------------------------------
#Raster loaded with a relative path 
#1. Load the “precip_warmQtr.tif”  file assign it to the name precip

#2. Load the “temp_meanColdQtr.tif” and assign it to temp_cold 

#3. Plot these raster data - gg won't well work here

## Exercise Seven - Stacks and Projections ---------------------------------
#1. Combine the precip and temp_cold objects into a single object named rastStack


#2. Load “elevation_49.tif” (not elevation.tif!) and assign the name elev49

#3. Try to add the elev49 object to rastStack


#Is there a problem? There should be but the latest version has the wrong file!

#Loading raster

#Adding to the stack

## Exercise Eight - Raster Algebra -----------------------------------------
#1. Load “temp_meanWarmQtr” and assign it to temp_warm 

#2. Create a raster of the difference between temp_cold & temp_warm and assign it to temp_range


#3. Combine these into a single stack called temps

## Exercise Nine - rasterVis plotting --------------------------------------
#1. Use levelplot() to plot temp_range

#2. Add contours to the map

#3. Use gplot to plot a single layer

#Extra for multiplots!
library(patchwork)

p1 <- gplot(rastStack[[1]]) +
  geom_raster(aes(fill = value)) +
  scale_fill_viridis_c(option = "magma", 
                       na.value = "transparent",
                       name = names(rastStack[[1]])) +
  coord_equal()

p2 <- gplot(rastStack[[2]]) +
  geom_raster(aes(fill = value)) +
  scale_fill_viridis_c(option = "viridis", 
                       na.value = "transparent",
                       name = names(rastStack[[2]])) +
  coord_equal() 


p3 <- gplot(rastStack[[3]]) +
  geom_raster(aes(fill = value)) +
  scale_fill_viridis_c(option = "cividis", 
                       na.value = "transparent",
                       name = names(rastStack[[1]])) +
  coord_equal() 

#Plotting with patchwork
p1 + p2 + p3

#Saving raster plots
png("./graphs/precip_plot.png", width = 725, height = 1024)
levelplot(precip) 
dev.off()   



# Combining sf and terra --------------------------------------------------
clim.bg <- spatSample(temps, 10000) %>% 
  mutate(species = "Background")

#Pulling the sf data
drosera.df2 <- drosera.sf %>% #Starting with my sf object I loaded from a shapefile
  st_coordinates() %>% #Extracting the coordinates I want
  as.data.frame() %>% #Converting to a dataframe
  rename("longitude" = X , "latitude" = Y) #Renaming the columns for clarity

#Prepping the data 
clim.all <- terra::extract(temps, 
                           drosera.df2)[,-1] %>% #Pulling the values for each point from the rasters
  bind_cols(species = as.character(drosera.sf$species)) %>% #Adding on the species data I had so I know which obs is for which species
  bind_rows(clim.bg) #Adding on my background data


ggplot(clim.all,
       aes(y = temp_meanWarmQtr,
           x = temp_meanColdQtr,
           colour = species)) + #Setting up the plot
  
  geom_point(data = filter(clim.all, 
                           species == "Background"),
             alpha = 0.4) + #Plotting the background points
  
  geom_point(data = filter(clim.all, 
                           species != "Background"),
             alpha = 0.6) + #Plotting my species points
  
  scale_colour_manual(values = c("grey", "#802028", "#586880"), name = "Species") + #Colouring!
  
  theme_minimal() #Because I like it.

