#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(data.table)
library(Hmisc)
library(chemometrics)
library(DMwR2)
library(Rlof)

set.seed(123)

## SETTING WORKSPACE ##

#current_path <- getActiveDocumentContext()$path
#setwd(dirname(current_path ))

setwd("C:\\Users\\Albert Pita\\Desktop\\Master\\MVA\\MDS-MVA-Project")

# Reading dataset
bikeDataSet <- read.csv("SeoulBikeData.csv",header=T, sep=",")
N <- nrow(bikeDataSet)

# DETECTING MULTIVARIATE OUTLIERS BY DENSITY OF LOCAL DISTANCES

# We get from Rented.Bike.Count to Snowfall
X <- bikeDataSet[,6:15] 

# All numeric or integer
print(lapply(X, class))

# k is the number of neighbours that will be used in the calculation of the local outlier factors.

for (k in 5:10) {
  outlier.scores.lofactor <- lofactor(X, k=k)
  
  file_name <- paste('density_plot_lofactor_k', k, sep="_")
  image_name <- paste(file_name, ".png", sep="")
  
  png(image_name, width = 1800, height = 1800, units = "px")
  plot(density(outlier.scores.lofactor))
  dev.off()
  
  outlier.scores.lof <- lof(X, k=k)
  
  file_name <- paste('density_plot_lof_k', k, sep="_")
  image_name <- paste(file_name, ".png", sep="")
  
  png(image_name, width = 1800, height = 1800, units = "px")
  plot(density(outlier.scores.lof))
  dev.off()
  
  print(all(outlier.scores.lofactor == outlier.scores.lof))
  
  # A value of approximately 1 indicates that the object is comparable to its neighbors
  # (and thus not an outlier). A value below 1 indicates a denser region (which would be
  # an inlier), while values significantly larger than 1 indicate outliers.
  
  print(length(which(outlier.scores.lofactor < 1)))
  gt2_lofactor <- length(which(outlier.scores.lofactor > 2))
  print(gt2_lofactor)
  
  # We pick the top greater than 2 outliers of lofactor func
  outliers_lofactor <- order(outlier.scores.lofactor, decreasing=T)[1:gt2_lofactor]
  sort(outlier.scores.lofactor,decreasing=T)[1:gt2_lofactor]
  
  file_name <- paste('outliers_lofactor_k', k, sep="_")
  
  fwrite(list(c(bikeDataSet[outliers_lofactor,2:6])), file = paste(file_name, ".txt", sep=""))
  
  print(length(which(outlier.scores.lof < 1)))
  gt2_lof <- length(which(outlier.scores.lof > 2))
  print(gt2_lof)
  
  # We pick the top greater than 2 outliers of lof func
  outliers_lof <- order(outlier.scores.lof, decreasing=T)[1:gt2_lof]
  sort(outlier.scores.lof,decreasing=T)[1:gt2_lof]
  
  file_name <- paste('outliers_lof_k', k, sep="_")
  
  fwrite(list(c(bikeDataSet[outliers_lof,2:6])), file = paste(file_name, ".txt", sep=""))
  
  file_name <- paste('pairs_plot_lofactor_k', k, sep="_")
  image_name <- paste(file_name, ".png", sep="")
  
  png(image_name, width = 1800, height = 1800,
      units = "px")
  pch <- rep(".", N)
  pch[outliers_lofactor] <- "+"
  col <- rep("black", N)
  col[outliers_lofactor] <- "red"
  pairs(X, pch=pch, col=col)
  dev.off()
  
  file_name <- paste('pairs_plot_lof_k', k, sep="_")
  image_name <- paste(file_name, ".png", sep="")
  
  png(image_name, width = 1080, height = 1080,
      units = "px")
  pch <- rep(".", N)
  pch[outliers_lof] <- "+"
  col <- rep("black", N)
  col[outliers_lof] <- "red"
  pairs(X, pch=pch, col=col)
  dev.off()
}
