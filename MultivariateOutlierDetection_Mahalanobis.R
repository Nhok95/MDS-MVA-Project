#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(data.table)
library(Hmisc)
library(chemometrics)

set.seed(123)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))

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

## MULTIVARIATE OUTLIER DETECTION (Classical Mahalanobis Distance) ##

# It's necessary to remove the variables Rainfall, Snowfall and Solar.Radiation
# because they have mad == 0 and that prevents Moutlier function to correctly
# calculate the Mahalanobis distance
bikeDataSet_no_factors <- bikeDataSet[, sapply(bikeDataSet, class) != "factor"]
print(lapply(bikeDataSet_no_factors, var))
print(lapply(bikeDataSet_no_factors, mad))
bikeDataSet_no_factors <- bikeDataSet_no_factors[, !names(bikeDataSet_no_factors) %in% c("Rainfall", "Snowfall", "Solar.Radiation")]
describe(bikeDataSet_no_factors)

# Calculation of the Classical Mahalanobis Distance
mout_res <- Moutlier(bikeDataSet_no_factors, quantile = 0.99, plot = FALSE)

plot(mout_res$md,
     main = "Classical Mahalanobis distance",
     ylab = "Classical mahalanobis distance value")
cutoff <- rep(mout_res$cutoff, nrow(bikeDataSet_no_factors))
cutoff_extreme <- rep(mout_res$cutoff, nrow(bikeDataSet_no_factors))
lines(cutoff, col = "red")
cutoff_extreme_value <- 10
cutoff_extreme <- rep(cutoff_extreme_value, nrow(bikeDataSet_no_factors))
lines(cutoff_extreme, col = "blue")

# These indices of the dataframe need an imputation of values for all their variables
# since they're multidimensional outliers
md_extreme_indices <- which(mout_res$md > cutoff_extreme_value)
md_extreme_indices
# 4064, 4132, 4133, 4134, 4135, 4254, 4256, 4277, 4278, 4279, 4280, 4364