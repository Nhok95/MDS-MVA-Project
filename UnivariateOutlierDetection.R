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
library(chemometrics)

set.seed(123)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Reading dataset
bikeDataSet <- read.csv("SeoulBikeData.csv",header=T, sep=",")
N <- nrow(bikeDataSet)

# Checking if there exist factor variables interpreted as characters
print(lapply(bikeDataSet, class)) # class of each variable
bikeDataSet$Date <- as.factor(bikeDataSet$Date)
bikeDataSet$Season <- as.factor(bikeDataSet$Season)
bikeDataSet$Holiday <- as.factor(bikeDataSet$Holiday)
bikeDataSet$Functioning.Day <- as.factor(bikeDataSet$Functioning.Day)
bikeDataSet$Hour <- as.factor(bikeDataSet$Hour)
bikeDataSet$Day <- as.factor(bikeDataSet$Day)
bikeDataSet$Month <- as.factor(bikeDataSet$Month)
bikeDataSet$Year <- as.factor(bikeDataSet$Year)
print(lapply(bikeDataSet, class)) # class of each variable

## MISSING DATA ##

summary(bikeDataSet)
mis_ind = rowSums(is.na(bikeDataSet))
table(mis_ind) 
m1 = which(mis_ind>0)
bikeDataSet[m1,] # 0 rows

## Simple missing values checking for Days and Hours ##
day_levels = levels(as.factor(bikeDataSet$Date)); day_levels
length( day_levels ) #levels length == 365 days ~ No NAs

hour_levels = levels(as.factor(bikeDataSet$Hour)); hour_levels
length( hour_levels ) #levels length == 24 hours ~ No NAs

## More complex missing value checking ##
date_table <- table(bikeDataSet$Date); date_table
length(date_table) #365 days
date_table[date_table != 24] #less or more than 24 hours per day -> 0 results
table(bikeDataSet$Hour)

# Fast NAs comprobation
print(length(which(is.na(bikeDataSet))))
describe(bikeDataSet) # No missings found (in Excel neither)

###

# Checking how similar the different seasons are, we decide to join Autumn and Spring
boxplot(bikeDataSet$Rented.Bike.Count ~ bikeDataSet$Season)

seasons <- c("AutumnSpring", "Summer", "Winter")
hours <- 0:23
holiday_values <- c("Holiday", "No Holiday")
numeric_feature_names <- c("Temperature", "Humidity", "Wind.Speed", "Visibility", "Dew.Point.Temperature", "Solar.Radiation", "Rainfall", "Snowfall", "Rented.Bike.Count")
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
    outliers_ids_by_feature <- X_filtered$Id[outliers_indices]
    
    # File route and name definition
    file_route <- paste(season, "/", season, sep="")
    file_name <- paste(file_route, hour, holiday, feature, sep="_")
    
    # Save outliers'ID in a txt file
    print(sprintf("%s outlier(s) found in [%s, hour %s, %s], feature %s", num_outliers, season, hour, holiday, feature))
    fwrite(list(c("Outliers ID:", outliers_ids_by_feature, "Outliers value:", outliers_values, num_obervations_txt, num_outliers_txt)), file = paste(file_name, ".txt", sep=""))
    
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
    outliers_ids_by_feature <- NULL
  }
  
  list_numOutliers_ids <- list("numOutliers" = num_outliers, "ids" = outliers_ids_by_feature)
  return(list_numOutliers_ids)
}

## UNIVARIATE OUTLIER DETECTION ##
# Note: this section of the script needs the existence of 3 folders with the names: AutumnSpring, Summer and Winter

outliers_ids_by_feature <- vector(mode = "list", length = length(numeric_feature_names))

for (i in 1:length(numeric_feature_names)) {
  
  for (season in seasons) {
    
    # Filter the observations by season
    if (season == "AutumnSpring") {
      
      X_by_season <- filter(bikeDataSet, Season == "Autumn" | Season == "Spring")
      
    } else {
      
      X_by_season <- filter(bikeDataSet, Season == season) 
    }
    
    for (hour in hours) {
      
      # Filter the season observations by hour
      X_by_season_by_hour <- filter(X_by_season, Hour == hour)
      
      # Special treatment for variable Rented.Bike.Count
      if (numeric_feature_names[i] == "Rented.Bike.Count") {
        
        # If the variable is Rented.Bike.Count, we also filter by holiday
        for (holiday_value in holiday_values) {
          
          # Filter the observations in season and hour by holiday
          X_by_season_by_hour_by_holiday <- filter(X_by_season_by_hour, Holiday == holiday_value)
          list_numOutliers_ids <- univariate_outlier_detection_and_output(X_by_season_by_hour_by_holiday, numeric_feature_names[i], feature_measures[i], season, hour, holiday_value, N)
          if (list_numOutliers_ids[["numOutliers"]] > 0) {
            outliers_ids_by_feature[[i]] <- c(outliers_ids_by_feature[[i]], list_numOutliers_ids[["ids"]])
          }
        }
        
      } else {
        
        list_numOutliers_ids <- univariate_outlier_detection_and_output(X_by_season_by_hour, numeric_feature_names[i], feature_measures[i], season, hour, "NO_HOLIDAY_FILTER", N)
        if (list_numOutliers_ids[["numOutliers"]] > 0) {
          outliers_ids_by_feature[[i]] <- c(outliers_ids_by_feature[[i]], list_numOutliers_ids[["ids"]])
        }
      }
    }
  }
  
  outliers_ids_by_feature[[i]] <- sort(unique(outliers_ids_by_feature[[i]]))
}

# Print ids of the different potential outliers found for every variable
print(outliers_ids_by_feature)

# Print number of potential outliers found for every variable
print(lapply(outliers_ids_by_feature, length))

# Print number of observations with potential outliers
outliers_ids <- sort(unique(unlist(outliers_ids_by_feature, recursive=FALSE)))
print(outliers_ids)
print(length(outliers_ids)) # It should be 1452

## CLUSTERING ##

# Boxplot of Rented.Bike.Count by season
boxplot(bikeDataSet$Rented.Bike.Count ~ bikeDataSet$Season,
        col="lightblue",
        border="black")

## Elbow Method for finding the optimal number of clusters
# NOTE: We could scale the numerical variables of the dataset before the clustering, but if we do
# that, we loose interpretability

# Compute and plot wss for k = 1 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
                function(k){kmeans(bikeDataSet[1:N, numeric_feature_names], k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:k.max, wss,
    type="b", pch = 19, 
    xlab="Number of clusters K",
    ylab="Total within-clusters sum of squares")
xtick <- seq(0, k.max, by=1)
axis(side=1, at=xtick)

fit <- kmeans(bikeDataSet[1:N, numeric_feature_names], 3, nstart = 50, iter.max = 15)
fit$centers
table(bikeDataSet[which(fit$cluster == 1), "Season"])
table(bikeDataSet[which(fit$cluster == 2), "Season"])
table(bikeDataSet[which(fit$cluster == 3), "Season"])

# We filter by season (for example, we select the Winter) and apply clustering again
winter_dataset <- subset(bikeDataSet, Season == "Winter", select = numeric_feature_names)

# Compute and plot wss for k = 1 to k = 10.
k.max <- 10
wss <- sapply(1:k.max,
              function(k){kmeans(winter_dataset, k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
xtick <- seq(0, k.max, by=1)
axis(side=1, at=xtick)

fit <- kmeans(winter_dataset, 2, nstart = 50, iter.max = 15)
fit$centers

# THIS + MEDIANS FOR SEASON AND FUNCTIONAL DAY SPLITTING JUSTIFICATION!
