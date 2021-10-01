#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(tibble)
library(dplyr)
library(data.table)
library(Hmisc)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Reading dataset
bikeDataSet <- read.csv("SeoulBikeData.csv",header=T, sep=",")
N <- nrow(bikeDataSet)

# Checking if there exist factor variables interpreted as characters
# class_of_variables <- lapply(bikeDataSet, class) # class of each variable
# print(class_of_variables)
bikeDataSet$Date <- as.factor(bikeDataSet$Date)
bikeDataSet$Season <- as.factor(bikeDataSet$Season)
bikeDataSet$Holiday <- as.factor(bikeDataSet$Holiday)
bikeDataSet$Functioning.Day <- as.factor(bikeDataSet$Functioning.Day)

# Fast NAs comprobation
which(is.na(bikeDataSet))
head(bikeDataSet)
describe(bikeDataSet) # so cool!

seasons <- c("Spring", "Summer", "Autumn", "Winter")
hours <- 0:23
holiday_values <- c("Holiday", "No Holiday")
feature_names <- c("Temperature", "Humidity", "Wind.Speed", "Visibility", "Dew.Point.Temperature", "Solar.Radiation", "Rainfall", "Snowfall", "Rented.Bike.Count")
feature_measures <- c("Degrees Celsius", "%", "m/s", "10m", "Degrees Celsius", "MJ/m2", "mm", "cm", "Number of bikes")

####################################################################################
# Function: univariate_outlier_detection_and_output                                #
# Desc:                                                                            #
# Detects the univariate outliers and outputs the results as images and .txt files #
####################################################################################
univariate_outlier_detection_and_output <- function(X_filtered, feature, feature_measure, season, hour, holiday, N) {
  
  # Outlier detection using boxplot.stats
  outliers_values <- boxplot.stats(X_filtered[1:N, feature])$out
  num_outliers <- length(outliers_values)
  num_outliers_txt <- paste("Num outliers: ", num_outliers, sep="")
  num_observations <- nrow(X_filtered)
  num_obervations_txt <- paste("Num observations: ", num_observations, sep="")

  # If there are outliers
  if (num_outliers > 0) {
    
    outliers_indices <- X_filtered[1:N, feature] %in% outliers_values
    outliers_ids <- X_filtered$Id[outliers_indices]
    
    # File route and name definition
    file_route <- paste(season, "/", season, sep="")
    file_name <- paste(file_route, hour, holiday, feature, sep="_")
    
    # Save outliers'ID in a txt file
    print(sprintf("%s outlier(s) found in [%s, hour %s, %s], feature %s", num_outliers, season, hour, holiday, feature))
    fwrite(list(c("Outliers ID:", outliers_ids, "Outliers value:", outliers_values, num_obervations_txt, num_outliers_txt)), file = paste(file_name, ".txt", sep=""))
    
    # Save boxplot in PNG format
    image_name <- paste(file_name, ".png", sep="")
    png(image_name)
    box_title <- paste(feature, feature_measure, sep=" in ")
    boxplot(X_filtered[1:N, feature], 
            main= paste(box_title, season, "hour:", hour, holiday, sep=" "), 
            xlab= feature_measure,
            ylab= feature,
            col="lightblue",
            border="black",
            horizontal=T)
    dev.off()
    
  } else {
    
    print(sprintf("No outliers found in [%s, hour %s, %s], feature %s", season, hour, holiday, feature))
    outliers_ids <- NULL
  }
  
  list_numOutliers_ids <- list("numOutliers" = num_outliers, "ids" = outliers_ids)
  return(list_numOutliers_ids)
}

## UNIVARIATE OUTLIER DETECTION ##

outliers_ids <- vector(mode = "list", length = length(feature_names))

for (i in 1:length(feature_names)) {
  
  for (season in seasons) {
    
    # Filter the observations by season
    X_by_season <- filter(bikeDataSet, Season == season) 
    
    for (hour in hours) {
      
      # Filter the season observations by hour
      X_by_season_by_hour <- filter(X_by_season, Hour == hour)
      
      # Special treatment for variable Rented.Bike.Count
      if (feature_names[i] == "Rented.Bike.Count") {
        
        # If the variable is Rented.Bike.Count, we also filter by holiday
        for (holiday_value in holiday_values) {
          
          # Filter the observations in season and hour by holiday
          X_by_season_by_hour_by_holiday <- filter(X_by_season_by_hour, Holiday == holiday_value)
          list_numOutliers_ids <- univariate_outlier_detection_and_output(X_by_season_by_hour_by_holiday, feature_names[i], feature_measures[i], season, hour, holiday_value, N)
          if (list_numOutliers_ids[["numOutliers"]] > 0) {
            outliers_ids[[i]] <- c(outliers_ids[[i]], list_numOutliers_ids[["ids"]])
          }
        }
        
      } else {
        
        list_numOutliers_ids <- univariate_outlier_detection_and_output(X_by_season_by_hour, feature_names[i], feature_measures[i], season, hour, "NO_HOLIDAY_FILTER", N)
        if (list_numOutliers_ids[["numOutliers"]] > 0) {
          outliers_ids[[i]] <- c(outliers_ids[[i]], list_numOutliers_ids[["ids"]])
        }
      }
    }
  }
  
  outliers_ids[[i]] <- sort(unique(outliers_ids[[i]]))
}

# Print ids of the different outliers found for every variable
print(outliers_ids)
# Print number of outliers found for every variable
print(lapply(outliers_ids, length))
