#####################################################################################
#                                                                                   #
# This script is for the final submission of MB2 course of the EAGLE Master Program #
# Jana Maier  - Submission in April 2021                                            #
#                                                                                   #
# Illegal mining spots in south-western Ghana are detected                          #
# Random Forest classifier is used and results are compared                         #
# Different input data is used: Landsat 8 and Sentinel 1, veg indices and glcm      #
# Land Cover partition of the area of interest is calculated over recent years      #
# The results are shown in an animation                                             #
#                                                                                   #
#####################################################################################


# install and load needed packages
list_of_packages <- c("raster", "ggplot2", "RStoolbox", "rgdal", "radomForest", 
                      "patchwork", "mapview", "viridis", "ggspatial", "glcm", "magrittr", "gganimate") 
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
library(randomForest)
library(patchwork)
library(mapview)
library(viridis)
library(ggspatial)
library(glcm)
library(magrittr)


# define the working directory
working_dir <- "C:"  #! this needs to be entered
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


# visualization of all bands
gplot(L8_2020) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradientn(colours = c("#ffffff", "#4cc3ff", "#005884", "#004366", "#000000")) +
  facet_wrap(~variable) +
  coord_equal() +
  labs(title = "Spectral Bands of Landsat 8 image")

# RGB image
L8 <- ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin") +
  labs(title = "Landsat 8 RGB image") +
  theme_minimal() +
  coord_equal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

L8

# shown in an interactive map, to see the location
viewRGB(L8_2020, r=4, g = 3, b = 2)



################################################################################
#### spectral indices ##########################################################
# normalized difference vegetation index 
ndvi <- (L8_2020[[5]] - L8_2020[[4]]) / (L8_2020[[5]] + L8_2020[[4]])
gplot(ndvi) +
  geom_raster(aes(x=x, y=y, fill=value)) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  coord_equal() +
  labs(title = "NDVI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

writeRaster(ndvi, "results/ndvi.tif")


# normalized difference water index 
ndwi <- (L8_2020[[5]] - L8_2020[[6]]) / (L8_2020[[5]] + L8_2020[[6]])
gplot(ndwi) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradient(low = "#004289", high = "white") +
  coord_equal() +
  labs(title = "NDWI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  
writeRaster(ndwi, "results/ndwi.tif")


# another normalized difference water index 
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

#check the training data structure
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
set.seed(76)

classification_result <- superClass(L8_2020, training_data, trainPartition = 0.7, 
                                model = 'rf', mode = 'classification', predict = TRUE,
                                responseCol = "class_name", filename = "results/classification",
                                overwrite = TRUE, verbose = TRUE)

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

# check correlations of raster layers
L8_2020_indices_long <- stack(L8_2020, ndvi, ndwi, ndwi2)
stats <- layerStats(L8_2020_indices_long, "pearson", na.rm = TRUE)
corr_matrix <- stats$`pearson correlation coefficient`
corr_matrix

# different combinations are possible
L8_2020_indices <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.2, L8_2020$L8_202005.3, L8_2020$L8_202005.4, ndvi, ndwi)
# L8_2020_indices <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.6, L8_2020$L8_202005.9, ndvi, ndwi)

set.seed(76)

classification_indices <- superClass(L8_2020_indices, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = "results/classification_indices",
                                    overwrite = TRUE, verbose = TRUE)

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

set.seed(76)

unsuper_classification <- unsuperClass(L8_2020, nSamples = 1000, nClasses = 4, nStarts = 25, 
                                       nIter = 100, norm = T, clusterMap = T, algorithm = "Hartigan-Wong",
                                       filename= "results/unsuper_class",progress='text', format='GTiff', datatype='INT1U', overwrite = TRUE)

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

# this is Sentinel 1 data from 2020, preprocessed in SNAP and in VV polarization

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
  labs(title = "Sentinel 1 intensity")


# all images together
plot_images <- (L8 + S1)
plot_images



################################################################################
#### classification using L8 and S1 together ###################################

# make sure that the extents and resolutions are the same
extent(S1_2020) <- extent(L8_2020)
S1_newres <- resample(S1_2020, L8_2020)

L8_S1 <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.2, L8_2020$L8_202005.3, L8_2020$L8_202005.4, S1_newres$intensity)

set.seed(76)

classification_full <- superClass(L8_S1, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = "results/classification_full",
                                    overwrite = TRUE, verbose = TRUE)

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

# next, the grey level co-occurence matrix is calculated as additional input for the classification with S1 data
S1_glcm <- glcm(S1_2020, statistics = c("mean", "variance", "homogeneity", "dissimilarity", "entropy"))
S1_glcm
plot(S1_glcm)


# stack it
S1_glcm_stack <- stack(S1_newres, S1_glcm)

set.seed(76)

classification_glcm <- superClass(S1_glcm_stack, training_data, trainPartition = 0.7, 
                                  model = 'rf', mode = 'classification', predict = TRUE,
                                  responseCol = "class_name", filename = "results/classification_glcm",
                                  overwrite = TRUE, verbose = TRUE)

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

# comparing the classification results - except glcm due to bad results

plot_all <- (plot_class + plot_class_indices) / (plot_unsuper_class + plot_class_full) +
  plot_annotation(title = "Comparision of classification inputs") &
  theme(plot.title = element_text(hjus = 0.5))

plot_all



################################################################################
#### calculate mining area #####################################################

# set NAs to 0
classification_result.df$layer[is.na(classification_result.df$layer)] <- 0

#calculate area with pixel size and count
myval <- classification_result.df[classification_result.df$layer == 3, "layer"] %>% 
  length() * xres(classification_result_map) * yres(classification_result_map)/10000

paste0(c("The illegal mining covers an area of about ", round(myval, 3), "km� in the region of interest."), collapse = "")




################################################################################
#### animation of land cover over the years ####################################

library(gganimate)

# read all files from previous years
L7_2010 <- brick("data/Landsat_previous/L7_2010.tif")
L7_2013 <- brick("data/Landsat_previous/L7_2013.tif")
L8_2016 <- brick("data/Landsat_previous/L8_2016.tif")
L8_2019 <- brick("data/Landsat_previous/L8_2019.tif")
L8_2020 <- brick("data/Landsat8_May2020/L8_202005.tif")

# preparation
files <- c(L7_2010, L7_2013, L8_2016, L8_2019, L8_2020)
years <- c(2010, 2013, 2016, 2019, 2020)

# the dataframe for the animation
areas.all <- data.frame()

j = 1
# this takes a while
# several new classifications and calculation of covered area
for (i in files) {
  k = years[j]

  training_data <- readOGR(dsn = paste0(working_dir, "/data/Landsat_previous"), layer = paste0(k, "_training")) 

  training_data <- spTransform(training_data, crs(i))
  crs(training_data)
  
    
set.seed(76)
file.name <- paste0("results/", k, "_classification")
classification_result <- superClass(i, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = file.name,
                                    overwrite = TRUE, verbose = TRUE)

classification_result_map <- brick(paste(file.name, ".grd", sep = ""))
classification_result.df <-  data.frame(coordinates(classification_result_map), getValues(classification_result_map))
classification_result.df$layer[is.na(classification_result.df$layer)] <- 0



# uncomment this section to see the actual classification results
# plot_class <- ggplot(classification_result.df) +
#   geom_raster(aes(x, y, fill = layer)) +
#   scale_fill_viridis() +
#   coord_equal() +
#   theme_minimal() +
#   labs(fill = "Classes", title = k,
#        caption = paste0("Accuracy: ", round(getValidation(classification_result)$Accuracy, digits=4))) +
#   theme(plot.caption = element_text(face = "bold"),
#         plot.title = element_text(hjust = 0.5, face = "bold"))
# 
# plot_class



# get the partition of the area per land cover type - because for 2010 and 2013 the L7 errors reduce the total area
bareland <- length(classification_result.df[classification_result.df$layer == 1, "layer"]) / 
  length(classification_result.df[classification_result.df$layer != 0, "layer"]) 
forest <- length(classification_result.df[classification_result.df$layer == 2, "layer"]) / 
  length(classification_result.df[classification_result.df$layer != 0, "layer"]) 

if (j == 1) { # because there is no mining yet
  urban <- length(classification_result.df[classification_result.df$layer == 3, "layer"]) / 
    length(classification_result.df[classification_result.df$layer != 0, "layer"])
  mining <- 0
} else {
  mining <- length(classification_result.df[classification_result.df$layer == 3, "layer"]) / 
    length(classification_result.df[classification_result.df$layer != 0, "layer"])
  urban <- length(classification_result.df[classification_result.df$layer == 4, "layer"]) / 
    length(classification_result.df[classification_result.df$layer != 0, "layer"])
}


areas <- c(mining, urban, bareland, forest)
year <- c(k, k, k, k)
this.year <- data.frame(classes, areas, year)
areas.all <- rbind(areas.all, this.year)

j = j+1
}

# areas.all includes the partitions of areas of all classes over the years
areas.all


# animate the covered area over the years, errors included due to classification results
ggplot(areas.all, aes(classes)) + 
  geom_bar(aes(weight = areas, fill = classes)) +
  labs(title = 'Year: {closest_state}', x = 'class', y = 'covered area') +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1) +
  ease_aes('linear') +
  theme_minimal() +
  theme(legend.position = "none")

#save the animation
anim_save(filename = "results/animation.gif", animation = last_animation())

