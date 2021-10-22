#####################################
##              PCA                ##
#####################################

rm(list=ls(all=TRUE))

library(rstudioapi)
library(Hmisc)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(corrplot)

set.seed(123)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

### This chunck of code would be better inside Imputation script

# Reading dataset
bikeDataSet <- read.csv("SeoulBikeData_FirstImp.csv",header=T, sep=",")

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

# Fast NAs comprobation
print(length(which(is.na(bikeDataSet))))
describe(bikeDataSet) # No missings found

# Reading outliers that are going to be deleted from dataset
outliers_lof <- read.delim("outliers_lof.txt", header=FALSE, sep="\n")$V1
outliers_rmd <- read.delim("outliers_rmd.txt", header=FALSE, sep="\n")$V1
length(outliers_lof)
length(outliers_rmd)
outliers_indexes <- union(outliers_lof, outliers_rmd)
bikeDataSet$mout <- 0
bikeDataSet[outliers_indexes, "mout"] <- 1
bikeDataSet$mout <- factor(bikeDataSet$mout, levels=c("0", "1"), labels=c("NoMOut", "YesMOut"))
table(bikeDataSet$mout)

# We save the clean dataset as an R object
save(list=c("bikeDataSet"), file="bikeDataSet.RData")

### PCA

# We read the dataset which has the outliers indicated
load("bikeDataSet.RData")

# We only want the observations which are not considered outliers
bikeDataSetClean <- bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDataSetClean <- select(bikeDataSetClean,
                           Hour,
                           Month,
                           Year,
                           Season,
                           Functioning.Day,
                           Holiday,
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

# Correlation between numerical variables
R <- cor(bikeDataSetClean[,7:15])
corrplot(R, method = "number", number.cex = 0.75)

# PCA
res.pca <- PCA(bikeDataSetClean,
               scale.unit=TRUE,
               ncp=10,
               # "Rented.Bike.Count"
               quanti.sup=15,
               # "Hour", "Mont", "Year", "Season", "Holiday", "Functioning.Day"
               quali.sup=1:6,
               graph=TRUE)

# Selecting the number of PCs that we'll keep
eigenvalues <- get_eigenvalue(res.pca)
eigenvalues # First 3 dimensions have eigenvalues > 1
# For achieving 80% of the total variance, we need to keep dims 1, 2, 3 and 4
fviz_screeplot(res.pca, addlabels=TRUE)

## Variables

var <- get_pca_var(res.pca)

# Correlation circle with cos2: quality of the factor map
fviz_pca_var(res.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes=1:2)
fviz_pca_var(res.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes=3:4)
var$cos2[,1:4]

# Correlation circle with contrib: contributions of the variables to the PCs
fviz_pca_var(res.pca, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
var$contrib[,1:4]

# Variables which contribute the most to the different PCs
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3:4, top = 10)

# Dimension description (PCs 1 and 2)
dim.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
dim.desc$Dim.1
dim.desc$Dim.2

## Individuals

ind <- get_pca_ind(res.pca)

# Quality of the individuals, measured with cos2
fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Individuals which contribute the most to the different PCs
fviz_contrib(res.pca, choice = "ind", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "ind", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "ind", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "ind", axes = 4, top = 10)
fviz_contrib(res.pca, choice = "ind", axes = 1:2, top = 10)
fviz_contrib(res.pca, choice = "ind", axes = 3:4, top = 10)

### NOTES: Roughly speaking a biplot can be interpreted as follow:
#    an individual that is on the same side of a given variable has a high value for this variable;
#    an individual that is on the opposite side of a given variable has a low value for this variable.
fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969")

## Supplementary variables

res.pca$quanti.sup
res.pca$quali.sup

fviz_pca_ind(res.pca, habillage = 4, addEllipses=TRUE, ellipse.type = "confidence", palette = "jco", repel=TRUE) 
fviz_pca_ind(res.pca, habillage = 5, addEllipses =TRUE, ellipse.type = "confidence", palette = "jco", repel=TRUE)
fviz_pca_ind(res.pca, habillage = 6, addEllipses =TRUE, ellipse.type = "confidence", palette = "jco", repel=TRUE)
fviz_pca_biplot(res.pca, select.ind = list(contrib = 5), select.var = list(contrib = 5), ggtheme = theme_minimal())
fviz_pca_biplot(res.pca, col.var = "contrib", gradient.cols = c("green", "orange", "red"))

