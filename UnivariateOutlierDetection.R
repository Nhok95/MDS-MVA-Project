#####################################
##            MVA LAB              ##
#####################################

# This script does not find outliers in Summer. We need to check if that's true.

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

seasons <- c("Spring", "Summner", "Autumn", "Winter")
hours <- 1:24
holiday_values <- c("Holiday", "No Holiday")
feature_names <- c("Temperature", "Humidity", "Wind.Speed", "Visibility", "Dew.Point.Temperature", "Solar.Radiation", "Rainfall", "Snowfall")
feature_measures <- c("Degrees Celsius", "%", "m/s", "10m", "Degrees Celsius", "MJ/m2", "mm", "cm")

## UNIVARIATE OUTLIER DETECTION (more methods in the near future) ##

for (season in seasons) {
  
  # Filter the observations by season
  X_by_season <- filter(bikeDataSet, Season == season)

  for (holiday_value in holiday_values) {
    
    # Filter the observations in season by holiday
    X_by_season_holiday <- filter(X_by_season, Holiday == holiday_value)
    
    for (hour in hours) {
      
      # Filter the observations in season & holiday by hour
      X_by_season_holiday_hour <- filter(X_by_season_holiday, Hour == hour)
      num_observations <- nrow(X_by_season_holiday_hour)
      num_obervations_txt <- paste("Num observations: ", num_observations, sep="")
      
      for (i in 1:length(feature_names)) {
        
        # Outlier detection using boxplot.stats
        outliers_values = boxplot.stats(X_by_season_holiday_hour[1:N, feature_names[i]])$out
        num_outliers <- length(outliers_values)
        num_outliers_txt <- paste("Num outliers: ", num_outliers, sep="")
        
        # If there are outliers
        if (num_outliers > 0) {
          
          outliers_indices = X_by_season_holiday_hour[1:N, feature_names[i]] %in% outliers_values
          outliers_ids = X_by_season_holiday_hour$Id[outliers_indices]
          
          # File route and name definition
          file_route <- paste(season, "/", season, sep="")
          file_name <- paste(file_route, holiday_value, hour, feature_names[i], sep="_")
          
          # Save outliers'ID in a txt file
          print(sprintf("%s outlier(s) found in [%s, %s, hour %s], feature %s", num_outliers, season, holiday_value, hour, feature_names[i]))
          fwrite(list(c("Outliers ID:", outliers_ids, "Outliers value:", outliers_values, num_obervations_txt, num_outliers_txt)), file = paste(file_name, ".txt", sep=""))
          
          # Save boxplot in PNG format
          image_name <- paste(file_name, ".png", sep="")
          png(image_name)
          box_title <- paste(feature_names[i], feature_measures[i], sep=" in ")
          boxplot(X_by_season_holiday_hour[1:N, feature_names[i]], 
                  main= paste(box_title, season, holiday_value, "hour:", hour, sep=" "), 
                  xlab= feature_measures[i],
                  ylab= feature_names[i],
                  col="lightblue",
                  border="black",
                  horizontal=T, 
                  notch=T)
          dev.off()
          
        } else {
          
          print(sprintf("No outliers found in [%s, %s, hour %s], feature %s", season, holiday_value, hour, feature_names[i]))
        }
      }
    }
  }
}