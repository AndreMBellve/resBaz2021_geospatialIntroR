#README TO BE ADDED
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
library(maps)
library(patchwork)
library(ggplot2)
library(ggspatial)

#Interaction
library(leaflet)

# Simple feature (vector) objects ----------------------------------------------------------
## Exercise One ------------------------------------------------------------
#Reading in the data (path relative to the working directory which is where ever the geospatialResBaz.proj file is)
nz.sf <-  st_read("./data/polygons/nz_coastlines/nz_coastlines.shp")

nz.sf

#Plotting
ggplot() +
  geom_sf(data = nz.sf) #sf plots sf objects
#data = is necessary!

plot(nz.sf)


## Exercise Two ------------------------------------------------------------
#Reading in the data
drosera.sf <-  st_read("./data/point_data/drosera/drosera.shp") %>% 
  st_transform(2193)

#Plotting the data together
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = drosera.sf, 
          aes(colour = species),
          alpha = 0.5)

## Exercise Three ----------------------------------------------------------
#Plotting sf data
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = drosera.sf, 
          aes(colour = species), alpha = 0.5) +
  #Adding stuff to make the map pretty
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  theme_minimal()

#Saving the map
ggsave("./graphs/exampleMap.png")
#note: ggsave smooths jagged lines

## Exercise Four -----------------------------------------------------------
#Defining a boundary box (a.k.a. extent)
stew.bbox <- st_bbox(c(xmin = 1155000, #Boundaries
                       ymin = 4723000,  
                       xmax = 1285000, 
                       ymax = 4820000), 
                     class = "bbox", #What it is
                     crs = 2193) #The ESPG code for the CRS

#Cropping the points
dros_stewart.sf <- st_crop(drosera.sf, stew.bbox)

#Cropping the coastline
coast_stewart.sf <- st_crop(nz.sf, stew.bbox)

#plotting the cropped data
ggplot() + 
  geom_sf(data = coast_stewart.sf) + 
  geom_sf(data = dros_stewart.sf, 
          aes(colour = species))


## Exercise Five -----------------------------------------------------------
#Reading in csv data
drosera.df <- read.csv("./data/point_data/drosera.csv")

#To show
head(drosera.df)

#Using leaflet
drosera.leaf <- leaflet(drosera.df) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~long, 
                   lat = ~lat, 
                   popup = ~species)

#Opening up viewer
drosera.leaf

# Raster (gridded) objects ---------------------------------------------------------
## Exercise Six ------------------------------------------------------------
#Raster loaded with a relative path 
precip <- rast("./data/rasters/precip_warmQtr.tif") 
temp_cold <- rast("./data/rasters/temp_meanColdQtr.tif")

#Plotting
plot(precip)
plot(temp_cold)


## Exercise Seven ----------------------------------------------------------
#Combining two rasters
rastStack <- c(precip, temp_cold)

# rastStack <-  rast(c("./data/rasters/precip_warmQtr.tif", 
#                        "./data/rasters/temp_meanColdQtr.tif"))

rastStack[[1]]
rastStack[["precip_warmQtr"]]

#Reading in elev49
elev49 <- rast("./data/rasters/elevation_49.tif")

#Trying to add
rastStack <- c(rastStack, elev49)

#Reprojecting

#This process can take a few minutes - more for large files!

elev49 <- project(elev49, #Problem raster
                  rastStack, #Target raster
                filename = "./data/rasters/elev49.tif", #This will write it as a new tif file
                overwrite = TRUE) #Allows files with the same name to be over-written

#If processing is too slow.
elev <- rast("./data/rasters/elevation.tif")

#Adding them all together in a single stack
rastStack <- c(rastStack, elev)


## Exercise Eight ----------------------------------------------------------
#Reading in the new layer
temp_warm <- rast("./data/rasters/temp_meanWarmQtr.tif")

#Raster algebra
#Finding the difference on a cell by cell basis
temp_range <-  temp_warm - temp_cold

plot(temp_range)

#Combining data
temps <- c(temp_warm, temp_cold, temp_range)

#Extra stuff as an example
#This create a raster of TRUE/FALSE (coded as 0 and 1) showing the cells that have a value as less than 10 as true
temp_range.lt10 <- temp_range < 10
plot(temp_range.lt10)


## Exercise Nine -----------------------------------------------------------
levelplot(temp_range)


levelplot(temp_range, contour = TRUE)

#rasterVis plotting 1
gplot(temp_range) +
  geom_raster(aes(fill = value)) +
  facet_wrap(~variable) +
  scale_fill_viridis_c(option = "magma", 
                      na.value = "transparent") +
  coord_equal()

#patchwork
#Multiple plots with varying axes
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


#Plotting logical vectors
gplot(temp_range.lt10) +      
  geom_raster(aes(fill = as.logical(value))) +
  scale_fill_manual(values = c('red', 'blue'), 
                    na.value = "transparent", 
                    name = "Diff < 10 °C") + 
  labs(x = 'Longitude', y = 'Latitude') +
  theme_minimal() +
  coord_equal() 


## Example of basic analysis ------------------------------------------------------------
#Getting a background sample
clim.bg <- spatSample(rastStack, 10000) %>% 
  mutate(species = "Background")

#Pulling the sf data
drosera.df2 <- drosera.sf %>%
  st_coordinates() %>% #
  as.data.frame() %>% 
  rename("longitude" = X , "latitude" = Y) 

clim.all <- terra::extract(rastStack, 
                           drosera.df2)[,-1] %>%
  bind_cols(species = as.character(drosera.sf$species)) %>% 
  bind_rows(clim.bg)


ggplot(clim.all,
       aes(y = precip_warmQtr,
           x = temp_meanColdQtr,
           colour = species)) +
  geom_point(data = filter(clim.all, 
                           species == "Background"),
             alpha = 0.4) +
  geom_point(data = filter(clim.all, 
                           species != "Background"),
             alpha = 0.6) +
  scale_colour_manual(values = c("grey", "#802028", "#586880"), name = "Species") +
  labs(y = "Precipitation of the Warmest Quarter (mm)", x = "Temperature of the Coldest Quarter (C°)") +
  theme_minimal()

#Saving
ggsave("./graphs/exampleDroseraPlot.png", width = 7.6, height = 5.2)


# Extra code --------------------------------------------------------------

#RASTER MANIPULATIONS
#Reading in a spat vector (terra polygon)
veg <- vect("./data/polygons/native_veg/native_veg.shp")

plot(veg)

#Masking
maskStack <- mask(temp_warm, veg)
plot(maskStack[[1]])

#Cropping
#Defining the extent of the cropping frame
cropBox <- c(1500000, 1650000, 
             5100000, 5200000) %>% 
  ext() #Making it a ext terra object

#Cropping it
tempCrop <- crop(temp_warm, cropBox)
plot(tempCrop)


# Workshop material prep --------------------------------------------------
#Pulling data
xx <- rgbif::occ_search(scientificName = c("Drosera binata",
                                           "Drosera spatulata"),
                        country = 'NZ',
                        hasCoordinate = TRUE)

#Point data
drosera.df <-bind_rows(xx$`Drosera binata`$data,
                       xx$`Drosera spatulata`$data) %>% 
  dplyr::select(c(species, 
                  decimalLatitude, 
                  decimalLongitude)) %>% 
  filter(!is.na(decimalLatitude))

#Converting to sf object
drosera.sf <- drosera.df %>% 
  st_as_sf(coords = c("decimalLongitude", 
                      "decimalLatitude")) %>% 
  st_set_crs(4326) %>% #WSG84
  st_transform(crs = 2193) %>% #NZGD2000
  st_join(nz.sf, join = st_within) %>% #Figuring which points are erroneous/NA
  filter(!is.na(TARGET_FID)) %>%  #Removing these from the dataset
  st_transform(4326)

st_write(drosera.sf, "./data/point_data/drosera.shp")

drosera.df <- drosera.sf %>% 
  st_transform(4326) %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(drosera.sf$species) %>% 
  rename(lat = Y, long = X, species = ..3)

write.csv(drosera.df, "./data/point_data/drosera.csv")












