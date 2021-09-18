#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(tibble)
library(dplyr)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

## GETTING DATASET AND GENERATING ID COLUMN ##

bikeDataSet <- read.csv("SeoulBikeDataOriginal.csv",header=T, sep=",")

n <- nrow(bikeDataSet)

Id = c(1:n)

new_bikeDataSet <- add_column(bikeDataSet, Id, .before=1)

## WRITING NEW DATASET ##

write.table(new_bikeDataSet, file = "SeoulBikeData.csv", sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


## GENERATING RANDOM SAMPLE ##

#random_seed <- sample(1:10000, 1)
random_seed <- (266L)

set.seed(random_seed)
subSet <- new_bikeDataSet %>% sample_n(5, replace= FALSE)


# Using filter (with the same Random Id obtained before)
sample <- c(2600,2682,5880,5975,8096)
subSet2 <- new_bikeDataSet %>% filter(Id %in% sample)

