#####################################################################################
#                                                                                   #
# This script is for the final submission of MB2 course of the EAGLE Master Program #
# Jana Maier  - Submission in April 2021                                            #
#                                                                                   #
# Illegal mining spots in south-western Ghana are detected                          #
# Different classifiers are used and compared                                       #
# Different input data is used: Landsat 8 and Sentinel 1                            #
#                                                                                   #
#####################################################################################


# install and load needed packages
list_of_packages <- c("raster", "ggplot2", "rasterVis", "RStoolbox", "rgdal", 
                      "cowplot", "radomForest", "patchwork", "mapview", "ggspatial", "glcm")   ###!
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) {
  print("installing : ")
  install.packages(new_packages, repos = "http://cran.rstudio.com/", dependencies = TRUE)
}

library(raster)
library(ggplot2)
library(rasterVis)
library(RStoolbox)
library(rgdal)
library(cowplot)
library(randomForest)
library(patchwork)
library(mapview)
library(viridis)
library(ggspatial)
library(glcm)
library(dplyr)
library(magrittr)

# define the working directory
working_dir <- "C:/Users/jmaie/Documents/R/MB2_final_project"  ###!
setwd(working_dir)

#create a folder for our results
dir.create("results")

# load the data
L8_2020 <- brick("data/Landsat8_May2020/L8_202005.tif")


# check the properties of the RasterStack
L8_2020

# check the Coordinate reference system (CRS), extent, spatial resolution, number of layers and layer names
crs(L8_2020)
extent(L8_2020)
res(L8_2020)
nlayers(L8_2020)
names(L8_2020)

gplot(L8_2020) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradientn(colours = c("#ffffff", "#4cc3ff", "#005884", "#004366", "#000000")) +
  facet_wrap(~variable) +
  coord_equal() +
  labs(title = "Spectral Bands of Landsat 8 image")

L8 <- ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin") +
  labs(title = "Landsat 8 RGB image") +
  theme_minimal() +
  coord_equal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

L8

# shown in an interactive map
viewRGB(L8_2020, r=4, g = 3, b = 2)



################################################################################
#### spectral indices ##########################################################
# normalized differenced vegetation index 
ndvi <- (L8_2020[[5]] - L8_2020[[4]]) / (L8_2020[[5]] + L8_2020[[4]])
gplot(ndvi) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradient(low = "white", high = "darkgreen") +
  coord_equal() +
  labs(title = "NDVI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

writeRaster(ndvi, "results/ndvi.tif")


# normalized differenced water index 
ndwi <- (L8_2020[[5]] - L8_2020[[6]]) / (L8_2020[[5]] + L8_2020[[6]])
gplot(ndwi) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradient(low = "#004289", high = "white") +
  coord_equal() +
  labs(title = "NDWI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  
writeRaster(ndwi, "results/ndwi.tif")


# another normalized differenced water index 
ndwi2 <- (L8_2020[[3]] - L8_2020[[5]]) / (L8_2020[[3]] + L8_2020[[5]])
gplot(ndwi2) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradient(low = "white", high = "#004289") +
  coord_equal() +
  labs(title = "another NDWI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  

writeRaster(ndwi2, "results/ndwi2.tif")



################################################################################
#### classification ############################################################ 

# load the training data
training_data <- readOGR(dsn = paste0(working_dir, "/data"), layer = "training_data") 

#check the training data structure and classes 
str(training_data)
training_data@data

#check if the projections match, and to be save, reproject the training data
crs(training_data)
crs(L8_2020)
training_data <- spTransform(training_data, crs(L8_2020))
crs(training_data)

class(training_data) # ggspatial package used to plot spatialpolygonsdataframe


# plotting the Landsat image with training polygons
ggplot() +
  ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin", ggLayer = TRUE) +
  layer_spatial(training_data, mapping = aes(fill = class_name)) +
  labs(title = "Landsat 8 training data", fill = "Classes") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))


#get the different classes
classes <- unique(training_data$class_name)
classes


#### using RStoolbox 
set.seed(23)

classification_result <- superClass(L8_2020, training_data, trainPartition = 0.7, 
                                model = 'rf', mode = 'classification', predict = TRUE,
                                responseCol = "class_name", filename = "results/classification",
                                overwrite = TRUE, verbose = TRUE)

# colors <- viridis(4)
# plot_class <- plot(classification_result$map, col = colors, legend = TRUE)
# plot_class
getValidation(classification_result)

classification_result_map <- brick("results/classification.grd")
classification_result.df <-  data.frame(coordinates(classification_result_map), getValues(classification_result_map))

plot_class <- ggplot(classification_result.df) +
  geom_raster(aes(x, y, fill = layer)) +
  scale_fill_viridis() +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "L8",
       caption = paste0("Accuracy: ", round(getValidation(classification_result)$Accuracy, digits=4))) +
  theme(plot.caption = element_text(face = "bold"), 
        plot.title = element_text(hjust = 0.5, face = "bold"))

plot_class



################################################################################
# less bands plus indices
L8_2020_indices <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.2, L8_2020$L8_202005.3, L8_2020$L8_202005.4, ndvi, ndwi)

set.seed(23)

classification_indices <- superClass(L8_2020_indices, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = "results/classification_indices",
                                    overwrite = TRUE, verbose = TRUE)

# plot_class_indices <- plot(classification_indices$map, col = colors, legend = TRUE)
# plot_class_indices
getValidation(classification_indices)

classification_indices_map <- brick("results/classification_indices.grd")
classification_indices.df <-  data.frame(coordinates(classification_indices_map), getValues(classification_indices_map))

plot_class_indices <- ggplot(classification_indices.df) +
  geom_raster(aes(x, y, fill= layer)) +
  scale_fill_viridis() +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "L8 and indices",
       caption = paste0("Accuracy: ", round(getValidation(classification_indices)$Accuracy, digits=4))) +
  theme(plot.caption = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

plot_class_indices



################################################################################
#### unsupervised classification ###############################################

set.seed(23)

unsuper_classification <- unsuperClass(L8_2020, nSamples = 1000, nClasses = 4, nStarts = 25, 
                                       nIter = 100, norm = T, clusterMap = T, algorithm = "Hartigan-Wong",
                                       filename= "results/unsuper_class",progress='text', format='GTiff', datatype='INT1U', overwrite = TRUE)

# plot_unsuper_class <- plot(unsuper_classification$map, col = colors, legend = TRUE)
# plot_unsuper_class

unsuper_class_map <- brick("results/unsuper_class.tif")
unsuper_class.df <-  data.frame(coordinates(unsuper_class_map), getValues(unsuper_class_map))

plot_unsuper_class <- ggplot(unsuper_class.df) +
  geom_raster(aes(x, y, fill= unsuper_class)) +
  scale_fill_viridis() +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "Unsupervised") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

plot_unsuper_class



################################################################################
#### radar data ################################################################

# this is Sentinel1 data from 2020, preprocessed in SNAP and in VV polarization

# read and check the data
S1_2020 <- brick("data/radar/S1_db_clipped.tif")
S1_2020  

#check the projection, to be sure, reproject
crs(S1_2020)
S1_2020 <- projectRaster(S1_2020, L8_2020)
crs(S1_2020)

extent(S1_2020)
res(S1_2020)
nlayers(S1_2020)
names(S1_2020)
names(S1_2020) <- "intensity"


# plotting the radar data
S1 <- ggR(S1_2020, stretch = "lin") + 
  theme_minimal() +
  coord_equal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Radar image")


# all images together
#plot_grid(S2, L8, S1, ncol = 3, align = "v")
plot_images <- (L8 + S1)
plot_images



################################################################################
#### classification using L8 and S1 together ###################################

# make sure that the extents and resolutions are the same
extent(S1_2020) <- extent(L8_2020)
S1_newres <- resample(S1_2020, L8_2020)

L8_S1 <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.2, L8_2020$L8_202005.3, L8_2020$L8_202005.4, S1_newres$intensity)

set.seed(23)

classification_full <- superClass(L8_S1, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = "results/classification_full",
                                    overwrite = TRUE, verbose = TRUE)

# plot_class_full <- plot(classification_full$map, col = colors, legend = TRUE)
# plot_class_full
getValidation(classification_full)

classification_full_map <- brick("results/classification_full.grd")
classification_full.df <-  data.frame(coordinates(classification_full_map), getValues(classification_full_map))

plot_class_full <- ggplot(classification_full.df) +
  geom_raster(aes(x, y, fill= layer)) +
  scale_fill_viridis() +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "L8 and S1",
       caption = paste0("Accuracy: ", round(getValidation(classification_full)$Accuracy, digits=4))) +
  theme(plot.caption = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

plot_class_full



################################################################################
#### classification using S1 and GLCM ##########################################

# here the grey level co-occurence matrix is calculated as additional input for the classification with S1 data

S1_glcm <- glcm(S1_2020, statistics = c("mean", "variance", "homogeneity", "dissimilarity", "entropy"))
S1_glcm
plot(S1_glcm)


# stack it
S1_glcm_stack <- stack(S1_newres, S1_glcm)

set.seed(23)

classification_glcm <- superClass(S1_glcm_stack, training_data, trainPartition = 0.7, 
                                  model = 'rf', mode = 'classification', predict = TRUE,
                                  responseCol = "class_name", filename = "results/classification_glcm",
                                  overwrite = TRUE, verbose = TRUE)

# plot_class_full <- plot(classification_full$map, col = colors, legend = TRUE)
# plot_class_full
getValidation(classification_glcm)

classification_glcm_map <- brick("results/classification_glcm.grd")
classification_glcm.df <-  data.frame(coordinates(classification_glcm_map), getValues(classification_glcm_map))

plot_class_glcm <- ggplot(classification_glcm.df) +
  geom_raster(aes(x, y, fill= layer)) +
  scale_fill_viridis() +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "S1 and GLCM",
       caption = paste0("Accuracy: ", round(getValidation(classification_glcm)$Accuracy, digits=4))) +
  theme(plot.caption = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

plot_class_glcm


################################################################################
#### comparison ################################################################

#plot_grid(plot_class, plot_class_indices, plot_unsuper_class, plot_class_full, nrow = 2, align = "hv")

plot_all <- (plot_class + plot_class_indices) / (plot_unsuper_class + plot_class_full) +
  plot_annotation(title = "Comparision of classification inputs") &
  theme(plot.title = element_text(hjus = 0.5))

plot_all



################################################################################
#### calculate mining area #####################################################

# set NAs to 0
classification_indices.df$layer[is.na(classification_indices.df$layer)] <- 0

#calculate area with pixel size and count
myval <- classification_indices.df[classification_indices.df$layer == 3, "layer"] %>% 
  length() * xres(classification_indices_map) * yres(classification_indices_map)/10000

myval
paste0(c("The illegal mining covers an area of about ", round(myval, 3), "km² in the region of interest."), collapse = "")



################################################################################
#### animation #################################################################

L7_2010 <- brick("data/Landsat_older/L7_2010.tif")
L7_2013 <- brick("data/Landsat_older/L7_2013.tif")
L8_2016 <- brick("data/Landsat_older/L8_2016.tif")
L8_2019 <- brick("data/Landsat_older/L8_2019.tif")
L8_2020_utm <- brick("data/Landsat_older/L8_2020.tif")
L8_2020 <- brick("data/Landsat8_May2020/L8_202005.tif")

ggRGB(L7_2010, r = 3, g = 2, b = 1, stretch = "lin") +
  labs(title = "Landsat 8 RGB image",  x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_equal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

res(L7_2010)
res(L8_2020_utm)

data(lsat)
lsat
res(lsat)
L8_2020
