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
list_of_packages <- c("raster", "ggplot2", "rasterVis", "RStoolbox", "rgdal", "cowplot", "radomForest", "patchwork", "mapview")   ###!
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


# define the working directory
working_dir <- "C:/Users/jmaie/Documents/R/MB2_final_project"  ###!
setwd(working_dir)

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
  scale_color_viridis_c() +
  facet_wrap(~variable) +
  coord_equal() +
  labs(title = "Spectral Bands of Landsat 8 image", x = "Longitude", y = "Latitude")

L8 <- ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin") +
  labs(title = "Landsat 8 RGB image",  x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_equal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

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
  labs(title = "NDVI", x = "Longitude", y = "Latitude")

writeRaster(ndvi, "results/ndvi.tif")

# normalized differenced water index 
ndwi <- (L8_2020[[5]] - L8_2020[[6]]) / (L8_2020[[5]] + L8_2020[[6]])
gplot(ndwi) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradient(low = "#0165f0", high = "white") +
  coord_equal() +
  labs(title = "NDWI", x = "Longitude", y = "Latitude")

writeRaster(ndwi, "results/ndwi.tif")

# another normalized differenced water index 
ndwi2 <- (L8_2020[[3]] - L8_2020[[5]]) / (L8_2020[[3]] + L8_2020[[5]])
gplot(ndwi2) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_fill_gradient(low = "white", high = "#0165f0") +
  coord_equal() +
  labs(title = "NDWI", x = "Longitude", y = "Latitude")

writeRaster(ndwi2, "results/ndwi2.tif")

################################################################################
#### classification ############################################################ 

# load the training data
training_data <- readOGR(dsn = paste0(working_dir, "/data"), layer = "training_data") 

#check the training data structure and classes 
str(training_data)
training_data@data

#check if the projections match, and to be save, reproject the training data
projection(training_data)
projection(L8_2020)
training_data <- spTransform(training_data, crs(L8_2020))

#get the different classes
classes <- unique(training_data$class_name)
classes


#### using RStoolbox 

classification_result <- superClass(L8_2020, training_data, trainPartition = 0.7, 
                                model = 'rf', mode = 'classification', predict = TRUE,
                                responseCol = "class_name", filename = "results/classification",
                                overwrite = TRUE, verbose = TRUE)

# colors <- viridis(4)
# plot_class <- plot(classification_result$map, col = colors, legend = TRUE)
# plot_class
getValidation(classification_result)

classification_result_map <- brick("results/classification.tif")
classification_result.df <-  data.frame(coordinates(classification_result_map), getValues(classification_result_map))
plot_class <- ggplot(classification_result.df) +
  geom_raster(aes(x, y, fill= factor(classification))) +
  scale_fill_viridis(discrete = TRUE) +
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

classification_indices <- superClass(L8_2020, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = "results/classification_indices",
                                    overwrite = TRUE, verbose = TRUE)

# plot_class_indices <- plot(classification_indices$map, col = colors, legend = TRUE)
# plot_class_indices
getValidation(classification_indices)

classification_indices_map <- brick("results/classification_indices.tif")
classification_indices.df <-  data.frame(coordinates(classification_indices_map), getValues(classification_indices_map))
plot_class_indices <- ggplot(classification_indices.df) +
  geom_raster(aes(x, y, fill= factor(classification_indices))) +
  scale_fill_viridis(discrete = TRUE) +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "L8 and indices",
       caption = paste0("Accuracy: ", round(getValidation(classification_indices)$Accuracy, digits=4))) +
  theme(plot.caption = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
plot_class_indices


################################################################################
#### unsupervised classification ###############################################

unsuper_classification <- unsuperClass(L8_2020, nSamples = 1000, nClasses = 4, nStarts = 25, 
                                       nIter = 100, norm = T, clusterMap = T, algorithm = "Hartigan-Wong",
                                       filename= "results/unsuper_class",progress='text', format='GTiff', datatype='INT1U', overwrite = TRUE)

# plot_unsuper_class <- plot(unsuper_classification$map, col = colors, legend = TRUE)
# plot_unsuper_class

unsuper_class_map <- brick("results/unsuper_class.tif")
unsuper_class.df <-  data.frame(coordinates(unsuper_class_map), getValues(unsuper_class_map))
plot_unsuper_class <- ggplot(unsuper_class.df) +
  geom_raster(aes(x, y, fill= factor(unsuper_class))) +
  scale_fill_viridis(discrete = TRUE) +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "Unsupervised") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
plot_unsuper_class

################################################################################
#### radar data ################################################################


# read and check the data
S1_2020 <- brick("data/radar/S1_clipped.tif")
S1_2020  
crs(S1_2020)
S1_2020 <- projectRaster(S1_2020, L8_2020)
extent(S1_2020)
res(S1_2020)
nlayers(S1_2020)
names(S1_2020)
names(S1_2020) <- "intensity"


# plotting the radar data
ggR(S1_2020, stretch = "lin") + 
  theme_minimal() +
  coord_equal() +
  labs(title = "Radar image", x = "Longitude", y = "Latitude")

# all images together
S1 <- ggR(S1_2020, stretch = "lin") + 
  theme_minimal() +
  coord_equal() +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Radar image", x = "Longitude", y = "Latitude")

#plot_grid(S2, L8, S1, ncol = 3, align = "v")
plot_images <- (L8 + S1)
plot_images


################################################################################
#### classification using L8 and S1 ############################################

# make sure that the extents and resolutions are the same
extent(S1_2020) <- extent(L8_2020)
S1_newres <- resample(S1_2020, L8_2020)

L8_S1 <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.2, L8_2020$L8_202005.3, L8_2020$L8_202005.4, S1_newres$intensity)

classification_full <- superClass(L8_2020, training_data, trainPartition = 0.7, 
                                    model = 'rf', mode = 'classification', predict = TRUE,
                                    responseCol = "class_name", filename = "results/classification_full",
                                    overwrite = TRUE, verbose = TRUE)

# plot_class_full <- plot(classification_full$map, col = colors, legend = TRUE)
# plot_class_full
getValidation(classification_full)

classification_full_map <- brick("results/classification_full.tif")
classification_full.df <-  data.frame(coordinates(classification_full_map), getValues(classification_full_map))
plot_class_full <- ggplot(classification_full.df) +
  geom_raster(aes(x, y, fill= factor(classification_full))) +
  scale_fill_viridis(discrete = TRUE) +
  coord_equal() +
  theme_minimal() +
  labs(fill = "Classes", title = "L8 and S1",
       caption = paste0("Accuracy: ", round(getValidation(classification_full)$Accuracy, digits=4))) +
  theme(plot.caption = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
plot_class_full



################################################################################
#### comparison ################################################################

#plot_grid(plot_class, plot_class_indices, plot_unsuper_class, plot_class_full, nrow = 2, align = "hv")

plot_all <- (plot_class + plot_class_indices) / (plot_unsuper_class + plot_class_full) +
  plot_annotation(title = "Comparision of classification inputs") &
  theme(plot.title = element_text(hjus = 0.5))
plot_all


################################################################################
#### calculate mining area #####################################################




