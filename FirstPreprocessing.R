#####################################
##            MVA LAB              ##
#####################################

## Clustering project -> Functioning Day / Hour / Season

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(tibble)
library(dplyr)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

## GETTING DATASET AND GENERATING ID, MONTH AND DAY COLUMNS ##

bikeDataSet <- read.csv("SeoulBikeDataOriginal.csv",header=T, sep=",")

n <- nrow(bikeDataSet)

Id = c(1:n)

# Add a new column with IDs
new_bikeDataSet <- add_column(bikeDataSet, Id, .before=1)

# Add 3 new columns: Year, Month and Day with the split values of the column Date (important for the
# predictive analysis part with an ML algorithm)
Year <- unlist(lapply(new_bikeDataSet[1:n, "Date"], FUN = function(x){strsplit(x, "/")[[1]][3]}))
new_bikeDataSet <- add_column(new_bikeDataSet, Year, .after=1)

Month <- unlist(lapply(new_bikeDataSet[1:n, "Date"], FUN = function(x){strsplit(x, "/")[[1]][2]}))
new_bikeDataSet <- add_column(new_bikeDataSet, Month, .after=1)

Day <- unlist(lapply(new_bikeDataSet[1:n, "Date"], FUN = function(x){strsplit(x, "/")[[1]][1]}))
new_bikeDataSet <- add_column(new_bikeDataSet, Day, .after=1)

## WRITING NEW DATASET ##

write.table(new_bikeDataSet, file = "SeoulBikeData.csv", quote = FALSE, 
            sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


## GENERATING RANDOM SAMPLE ##

#random_seed <- sample(1:10000, 1)
random_seed <- (266L)

set.seed(random_seed)
subSet <- new_bikeDataSet %>% sample_n(5, replace= FALSE)


# Using filter (with the same Random Id obtained before)
sample <- c(2600, 2682, 5880, 5975, 8096)
subSet2 <- new_bikeDataSet %>% filter(Id %in% sample)