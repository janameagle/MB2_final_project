#####################################################################################
#                                                                                   #
# This script is for the final submission of MB2 course of the EAGLE Master Program #
# Jana Maier  - Submission in April 2021                                            #
#                                                                                   #
# Illegal mining spots in south-western Ghana are detected                          #
# Different classifiers are used and compared                                       #
# Different input data is used: Sentinel 2, Sentinel 1, Landsat                     #
#                                                                                   #
#####################################################################################


# install and load needed packages
list_of_packages <- c("raster", "ggplot2", "rasterVis", "RStoolbox", "rgdal", "cowplot", "radomForest")   ###!
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
#library()


# define the working directory
working_dir <- "C:/Users/jmaie/Documents/R/MB2_final_project"
setwd(working_dir)

# load the data
S2_2020 <- brick("data/S2_20200127.tif")
L8_2020 <- brick("data/Landsat8_May2020/L8_202005.tif")

# check the properties of the RasterStack
S2_2020
# check the Coordinate reference system (CRS), extent, spatial resolution, number of layers and layer names
crs(S2_2020)
extent(S2_2020)
res(S2_2020)
nlayers(S2_2020)
names(S2_2020)
#Change the band names
names(S2_2020) <- c("B1", "B2", "B3")  ###!


# check the properties of the RasterStack
L8_2020
# check the Coordinate reference system (CRS), extent, spatial resolution, number of layers and layer names
crs(L8_2020)
extent(L8_2020)
res(L8_2020)
nlayers(L8_2020)
names(L8_2020)


# convert to dataframe, if you want to use ggplot instead of gplot
#S2_2020 <-  data.frame(coordinates(S2_2020), getValues(S2_2020))

################################################################################
#### visualization #############################################################

# visualize the bands
#plot(S2_2020)

#ggplot(S2_2020) +
#  geom_raster(aes(x=x, y=y, fill=S2_2020[,3])) +
#  scale_fill_gradient(na.value=NA) + coord_equal()


gplot(L8_2020) +
  geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
  scale_color_viridis_c() +
  facet_wrap(~variable) +
  coord_equal() +
  labs(title = "Spectral Bands of Landsat 8 image", x = "Longitude", y = "Latitude")

# Visualize as true-color image, RED:Band3, GREEN: B2, BLUE: B1   ###!
#plotRGB(S2_2020, r = 1, g = 2, b = 3, stretch = "lin", main = "Sentinel 2 RGB image", axes = TRUE)
S2 <- ggRGB(S2_2020, r = 1, g = 2, b = 3, stretch = "lin") +
  labs(title = "Sentinel 2 RGB image") +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

L8 <- ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin") +
  labs(title = "Landsat 8 RGB image") +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))


plot_grid(S2, L8, ncol = 2, align = "v")


################################################################################
#### spectral indices ##########################################################
ndvi <- (L8_2020[[5]] - L8_2020[[4]]) / (L8_2020[[5]] + L8_2020[[4]])
gplot(ndvi) +
geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
scale_fill_gradient(low = "white", high = "darkgreen") +
coord_equal() +
labs(title = "NDVI", x = "Longitude", y = "Latitude")

ndwi <- (L8_2020[[5]] - L8_2020[[6]]) / (L8_2020[[5]] + L8_2020[[6]])
gplot(ndwi) +
geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
scale_fill_gradient(low = "#0165f0", high = "white") +
coord_equal() +
labs(title = "NDWI", x = "Longitude", y = "Latitude")

ndwi2 <- (L8_2020[[3]] - L8_2020[[5]]) / (L8_2020[[3]] + L8_2020[[5]])
gplot(ndwi2) +
geom_raster(aes(x=x, y=y, fill=value)) + ###! was ist value
scale_fill_gradient(low = "white", high = "#0165f0") +
coord_equal() +
labs(title = "NDWI", x = "Longitude", y = "Latitude")


################################################################################
#### classification ############################################################ 

# load the training data
training_data <- readOGR(dsn = paste0(working_dir, "/data/Landsat8_May2020"), layer = "training_data_small") 

#check the training data structure and classes 
str(training_data)
training_data@data

#check if the projections match
projection(training_data)
projection(L8_2020)

#get the different classes
classes <- unique(training_data$classname)
classes

# create random points in training polygons
# ... output: training_points
# set.seed(40)
#i <- 1
for (i in 1:length(classes)){
  class_data <- training_data[training_data$classname == classes[i],]
  classpts <- spsample(class_data, type = "random", n=400)
  classpts$class<- rep(classes[i], length(classpts))
  if (i == 1){
    random_points <- classpts
  } else{
    random_points <- rbind(random_points, classpts)
  }
}

#plot the image with the random points
#image(L8_2020, 1)
#points(random_points, pch = 20)

random_points.df <- as.data.frame(random_points)
head(random_points.df)
ggplot() +
  ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin", ggLayer = T) +
  geom_point(random_points.df, mapping = aes(x=x, y=y, color = class)) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Landsat 8 training points") +
  theme(plot.title = element_text(hjust = 0.5))

# less bands plus indices
L8_2020 <- stack(L8_2020$L8_202005_small.1, L8_2020$L8_202005_small.2, L8_2020$L8_202005_small.3, L8_2020$L8_202005_small.4, ndvi, ndwi)


# extract reflectance values of satellite image at positions of training points
#training_values <- raster::extract(L8_2020, y = training_points)
extracted_values <- over(x= random_points, y = training_data)
response <- factor(extracted_values$classname)
training_values <- cbind(response, extract(L8_2020, random_points))


# convert to dataframe
#training_values <- data.frame(training_values)


#the actual classification statistics using the random Forest method
random_forest <- randomForest(as.factor(response) ~. , 
                              data = training_values,
                              na.action = na.omit,
                              confusion = TRUE)

#apply the model to the full raster
predict(L8_2020, random_forest, filename= "classification", progress='text', format='GTiff', datatype='INT1U',
        type='response', overwrite=TRUE)

classification_result <- brick("classification.tif")
plot(classification_result)
classification_result.df <-  data.frame(coordinates(classification_result), getValues(classification_result))
ggplot(classification_result.df) +
  geom_raster(aes(x, y, fill= classification)) +
  scale_fill_viridis_c() +
  coord_equal()

################################################################################
#### unsupervised classification ###############################################

unsuper_classification <- unsuperClass(L8_2020, nSamples = 1000, nClasses = 4, nStarts = 25, 
                                       nIter = 100, norm = T, clusterMap = T, algorithm = "Hartigan-Wong")
plot(unsuper_classification$map)

###########smaller extent#########################
box <- extent(-1.95, 6.24, -1.86, 6.28)
L8_2020_small <- crop(L8_2020, box)
ggRGB(L8_2020_small, r = 4, g = 3, b = 2, stretch = "lin") +
labs(title = "Landsat 8 RGB image") +
theme(text = element_text(size = 14)) +
theme(plot.title = element_text(hjust = 0.5))



################################################################################
#### radar data ################################################################

S1 <- brick("data/radar/S1_clipped.tif")
S1  
crs(S1)
extent(S1)
res(S1)
nlayers(S1)
names(S1)
names(S1) <- "intensity"

# plotting the radar data


ggR(S1, stretch = "lin") + 
  theme_minimal() +
  coord_equal() +
  labs(title = "Radar image", x = "Longitude", y = "Latitude")

# all images together
S2 <- ggRGB(S2_2020, r = 1, g = 2, b = 3, stretch = "lin") +
  labs(title = "Sentinel 2 RGB image") +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

L8 <- ggRGB(L8_2020, r = 4, g = 3, b = 2, stretch = "lin") +
  labs(title = "Landsat 8 RGB image") +
  theme(text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

S1 <- ggR(S1, stretch = "lin") + 
  theme_minimal() +
  coord_equal() +
  labs(title = "Radar image", x = "Longitude", y = "Latitude")

plot_grid(S2, L8, S1, ncol = 3, align = "v")


################################################################################
#### classification using L8 and S1 ############################################

L8_S2 <- stack(L8_2020$L8_202005.1, L8_2020$L8_202005.2, L8_2020$L8_202005.3, L8_2020$L8_202005.4, S1$intensity)
