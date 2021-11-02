#####################################
##      Train & Test Creation      ##
#####################################

library(rstudioapi)
library(caTools)
library(dplyr)
library(cowplot)
library(ggplot2)
library(gridExtra)

rm(list=ls(all=TRUE))
set.seed(123)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read the dataset which has the outliers indicated
load("bikeDataSet.RData")

# Select observations from numerical features which are not considered outliers
bikeDataSetClean <- bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDataSetClean <- select(bikeDataSetClean, -c("mout"))

# Perform partition
sample = sample.split(bikeDataSetClean[, 1], SplitRatio = .70)
bikeDataSet_train = subset(bikeDataSetClean, sample == TRUE)
dim(bikeDataSet_train)
bikeDataSet_test  = subset(bikeDataSetClean, sample == FALSE)
dim(bikeDataSet_test)

# Save the train dataset as an R object
save(list=c("bikeDataSet_train"), file="bikeDataSet_train.RData")

# Save the test dataset as an R object
save(list=c("bikeDataSet_test"), file="bikeDataSet_test.RData")

### Generate histograms for showing differences between train & test ###

rm(list=ls(all=TRUE))
load("bikeDataSet_train.RData")
load("bikeDataSet_test.RData")
bikeDataSet_train <- bikeDataSet_train[, sapply(bikeDataSet_train, class) != "factor"]
bikeDataSet_test <- bikeDataSet_test[, sapply(bikeDataSet_test, class) != "factor"]
bikeDataSet_train$type <- 'train'
bikeDataSet_test$type <- 'test'
train_test <- rbind(bikeDataSet_train, bikeDataSet_test)
train_test$type <- factor(train_test$type, labels = c("train", "test"))
str(train_test)

g0_with_legend <- train_test %>%
  ggplot(aes(x = Rented.Bike.Count, fill = type)) + 
    geom_histogram() +
    labs(x = 'Num of bikes', title = 'Rented Bike Count')

g0 <- train_test %>%
  ggplot(aes(x = Rented.Bike.Count, fill = type)) + 
    geom_histogram() +
    labs(x = 'Num of bikes', title = 'Rented Bike Count') +
    theme(legend.position = "none")

g1 <- train_test %>%
  ggplot(aes(x = Temperature, fill = type)) + 
    geom_histogram() +
    labs(x = 'Degrees celsius', title = 'Temperature') +
    theme(legend.position = "none")

g2 <- train_test %>%
  ggplot(aes(x = Humidity, fill = type)) + 
    geom_histogram() +
    labs(x = '%', title = 'Humidity') +
    theme(legend.position = "none")

g3 <- train_test %>%
  ggplot(aes(x = Wind.Speed, fill = type)) + 
    geom_histogram() +
    labs(x = 'm/s', title = 'Wind Speed') +
    theme(legend.position = "none")

g4 <- train_test %>%
  ggplot(aes(x = Visibility, fill = type)) + 
    geom_histogram() +
    labs(x = 'Visibility reported in 10 meters', title = 'Visibility') +
    theme(legend.position = "none")

g5 <- train_test %>%
  ggplot(aes(x = Dew.Point.Temperature, fill = type)) + 
    geom_histogram() +
    labs(x = 'Degrees celsius', title = 'Dew Point Temperature') +
    theme(legend.position = "none")

g6 <- train_test %>%
  ggplot(aes(x = Solar.Radiation, fill = type)) + 
    geom_histogram() +
    labs(x = 'MJ/m2', title = 'Solar Radiation') +
    theme(legend.position = "none")

legend <- get_legend(g0_with_legend + theme(legend.position="right", legend.text=element_text(size=15), legend.title = element_text(face = "bold", size=15)))
grid.arrange(g0, g1, g2, g3, g4, g5, g6, legend, ncol = 2)

