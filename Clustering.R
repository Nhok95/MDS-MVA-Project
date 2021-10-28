#####################################
##            Clustering           ##
#####################################

library(cluster)
library(factoextra)
library(rstudioapi)
library(dplyr)
library(stringr)

rm(list=ls(all=TRUE))
set.seed(123)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read the dataset which has the outliers indicated
load("bikeDataSet.RData")

# Select observations from numerical features which are not considered outliers
bikeDataSetClean <- bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDataSetClean <- select(bikeDataSetClean,
                           Temperature,
                           Humidity,
                           Wind.Speed,
                           Visibility,
                           Dew.Point.Temperature,
                           Solar.Radiation,
                           Rainfall,
                           Snowfall,
                           Rented.Bike.Count)
N <- nrow(bikeDataSetClean)
str(bikeDataSetClean)

# Scale the dataset since we have different units in each numerical feature
bikeDataSetClean <- scale(bikeDataSetClean)

# Obtain the optimal value of k
fviz_nbclust(bikeDataSetClean, clara, method = "silhouette") + labs(subtitle = "Silhouette method")

# Perform clustering with clara and the best k obtained in the previous step
k <- 3
clara.res <- clara(bikeDataSetClean, k, metric = "euclidean", stand = FALSE, samples = 50, pamLike = TRUE)

# Medoids (clusters obtained)
clara.res$medoids
write.csv(clara.res$medoids,"medoids_from_clara.csv", row.names = TRUE)

# Visualize clusters
fviz_cluster(clara.res, ellipse.type = "t", geom = "point", pointsize = 2.5) +
  theme_bw() +
  labs(title = "Clustering results from CLARA") +
  theme(legend.position = "none")

###########################################################################################################

# Check if the k changes when filtering by seasons #

bikeDataSetClean <- bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
seasons <- c("AutumnSpring", "Summer", "Winter")

for (i in 1:length(seasons)) {
  
  if (seasons[[i]] == "AutumnSpring") {
    dataset_by_season <- filter(bikeDataSetClean, Season == "Autumn" | Season == "Spring")
    
  } else {
    dataset_by_season <- filter(bikeDataSetClean, Season == seasons[[i]])
    
  }
  
  dataset_by_season <- select(dataset_by_season,
                               Temperature,
                               Humidity,
                               Wind.Speed,
                               Visibility,
                               Dew.Point.Temperature,
                               Solar.Radiation,
                               Rainfall,
                               Snowfall,
                               Rented.Bike.Count)
  dataset_by_season <- scale(dataset_by_season)
  plot_title <- paste("Silhouette method ", seasons[[i]], sep="")
  result <- fviz_nbclust(dataset_by_season, clara, method = "silhouette") + labs(subtitle = plot_title)
  plot(result)
  plot_title <- str_replace_all(trimws(plot_title), "[ ]", "_")
  dev.print(pdf, paste(plot_title, ".pdf", sep=""))
}

