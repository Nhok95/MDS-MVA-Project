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
bikeDataSet.NoMout <- bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDataSetClean <- select(bikeDataSet.NoMout,
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

seasons <- c("AutumnSpring", "Summer", "Winter")

for (i in 1:length(seasons)) {
  
  if (seasons[[i]] == "AutumnSpring") {
    dataset_by_season <- filter(bikeDataSet.NoMout, Season == "Autumn" | Season == "Spring")
    
  } else {
    dataset_by_season <- filter(bikeDataSet.NoMout, Season == seasons[[i]])
    
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

#####################################
##            Profiling            ##
#####################################


#bikeDataSetClean = select(bikeDataSet, -c("Id"))

#bikeDataSetClean$Hour = as.numeric(bikeDataSetClean$Hour)
rowlabels = bikeDataSet.NoMout$Season
rownames(bikeDataSetClean) = paste(rowlabels, rownames(bikeDataSetClean), sep="-")

#num_col = unlist(lapply(bikeDataSetClean, is.numeric))
#df = bikeDataSetClean[, num_col]
#df = scale(bikeDataSetClean[, num_col]) # x - mean / sd
#head(df)

## Error in rowSums(as.matrix(ok)) : 
## 'Calloc' could not allocate memory (74114881 of 16 bytes)
##matrix_BDS = data.matrix(df) # get a matrix version
##res.dist <- get_dist(matrix_BDS, method = "pearson") 
##fviz_dist(res.dist, lab_size = 8)


res.km <- eclust(bikeDataSetClean, k = 3, "kmeans", nstart = 50)

#fviz_gap_stat(res.km$gap_stat) #best num of clusters

fviz_silhouette(res.km) #negative silhouette means that is wrong assigned

###### Postprocessing - Getting Profiles

profiling.results <-data.frame(bikeDataSet.NoMout,res.km$cluster)
write.csv(profiling.results,"profiling_preliminar_results.csv")


res.km$centers

#### Real centroids

aggregate(bikeDataSetClean,by=list(res.km$cluster),FUN=mean) 
# Grup 1 seems that represents winter
# Grup 2 seems that represents autumm-spring (and transition beetween seasons)
# Grup 3 seems that represents summer

aggregate(bikeDataSetClean,by=list(res.km$cluster),FUN=median)
aggregate(bikeDataSetClean,by=list(res.km$cluster),FUN=sd)
aggregate(bikeDataSetClean,by=list(res.km$cluster),FUN=IQR) #distance between min and max values (q1 to q3)

### Checking Variables

##Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation
### where there are more than two groups. It's recommended when the assumptions of one-way ANOVA test are not met.
profiling.results$res.km.cluster<-as.factor(profiling.results$res.km.cluster)
levels(profiling.results$res.km.cluster)
profiling.results$res.km.cluster<- ordered(profiling.results$res.km.cluster,levels = c("1", "2", "3"))

boxplot(results$Temperature~results$res.km.cluster, main= "Temperature")
boxplot(results$Rented.Bike.Count~results$res.km.cluster, main= "RBC")

# No interception between medians of boxplots, that means that median are different, the feature murder is important to get
# profiles inside the cluster. We have to keep this feature in order to get conclusions.


kruskal.test(UrbanP ~ res.km.cluster, data = results) # p-vlaue ~ 0, means median for murder is diff from median to other groups
# As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between groups in relation to "Murder".

boxplot(results$Assault~results$res.km.cluster, main= "Assault")
boxplot(results$UrbanPop~results$res.km.cluster, main= "UrbanPop")

kruskal.test(UrbanPop ~ res.km.cluster, data = results) # p-value = 0.0004, close to 0.05 but still below

#From the output of the Kruskal-Wallis test, we know that there is a significant difference between groups, but we don't know which pairs of groups are different.

pairwise.wilcox.test(results$Murder, results$res.km.cluster, p.adjust.method = "BH") #M1 <> M2 <> M3
# Warning cause we can't compute one with themself

### For Normal or T-student features, please use Arturo's notes. For categorical variables, please use Chi-squared Test. R functin "catdes"

### Additional graphs could be done to interpret your profiles by using your csv file in Excel.

