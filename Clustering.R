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
#write.csv(clara.res$medoids,"medoids_from_clara.csv", row.names = TRUE)

# Visualize clusters
fviz_cluster(clara.res, ellipse.type = "t", geom = "point", pointsize = 2.5) +
  theme_bw() +
  labs(title = "Clustering results from CLARA")
  

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

res.km <- eclust(bikeDataSetClean, k = 3, "kmeans", nstart = 5)

#fviz_silhouette(res.km) 
fviz_silhouette(clara.res) + 
  scale_color_hue(l=40, c=35) + 
  scale_fill_manual(values=c("blue", "green", "red")) #negative silhouette means that is wrong assigned

###### Postprocessing - Getting Profiles

#profiling.results = data.frame(bikeDataSet.NoMout,clara.res$clustering)
#write.csv(profiling.results,"profiling_preliminar_results.csv")

plot(table(profiling.results$Season, profiling.results$clara.res.clustering), 
     col= c("blue","green", "red"),
     main = "Seasons vs Cluster")

final.results = data.frame(bikeDataSetClean,clara.res$clustering)
names(final.results)[names(final.results) == 'clara.res.clustering'] = 'cluster'

clara.res$medoids
#### Real centroids

mean.table = aggregate(bikeDataSetClean,by=list(clara.res$clustering),FUN=mean) 
# Grup 1 seems to represent winter (cold weather)
# Grup 2 seems to represent a transition between winter and summer (mild weather)
# Grup 3 seems to represent summer (warm weather)

median.table = aggregate(bikeDataSetClean,by=list(clara.res$clustering),FUN=median)
sd.table = aggregate(bikeDataSetClean,by=list(clara.res$clustering),FUN=sd)
IQR.table = aggregate(bikeDataSetClean,by=list(clara.res$clustering),FUN=IQR) #distance between min and max values (q1 to q3)

### Checking Variables

##Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation
### where there are more than two groups. It's recommended when the assumptions of one-way ANOVA test are not met.
final.results$cluster<-as.factor(final.results$cluster)
str(final.results)
levels(final.results$cluster) = c("Winter","Transition","Summer")
final.results$cluster<- ordered(final.results$cluster,levels = c("Winter", "Transition", "Summer"))



##https://en.wikipedia.org/wiki/Climate_of_Seoul
boxplot(final.results$Temperature~final.results$cluster, main= "Temperature")            # No interception (important)
boxplot(final.results$Humidity~final.results$cluster, main= "Humidity")         
boxplot(final.results$Wind.Speed~final.results$cluster, main= "Wind Speed")              # No interception (important)
boxplot(final.results$Visibility~final.results$cluster, main= "Visibility ")
boxplot(final.results$Dew.Point.Temperature~final.results$cluster, main= "DPTemp")
boxplot(final.results$Solar.Radiation~final.results$cluster, main= "Solar Radiation")
boxplot(final.results$Snowfall~final.results$cluster, main= "Snowfall")
boxplot(final.results$Rented.Bike.Count~final.results$cluster, main= "Rented Bike Count")

kruskal.test(UrbanP ~ res.km.cluster, data = results)

# No interception between medians of boxplots, that means that median are different, the feature murder is important to get
# profiles inside the cluster. We have to keep this feature in order to get conclusions.

# As the p-value is less than the significance level, we can conclude that there are significant differences between groups in relation to "Temperature".
kruskal.test(Temperature ~ res.km.cluster, data = results)

# As the p-value is less than the significance level, we can conclude that there are significant differences between groups in relation to "Humidity".
kruskal.test(Humidity ~ res.km.cluster, data = results)


boxplot(results$Assault~results$res.km.cluster, main= "Assault")
boxplot(results$UrbanPop~results$res.km.cluster, main= "UrbanPop")

kruskal.test(UrbanPop ~ res.km.cluster, data = results) # p-value = 0.0004, close to 0.05 but still below

#From the output of the Kruskal-Wallis test, we know that there is a significant difference between groups, but we don't know which pairs of groups are different.

pairwise.wilcox.test(results$Murder, results$res.km.cluster, p.adjust.method = "BH") #M1 <> M2 <> M3
# Warning cause we can't compute one with themself

### For Normal or T-student features, please use Arturo's notes. For categorical variables, please use Chi-squared Test. R functin "catdes"

### Additional graphs could be done to interpret your profiles by using your csv file in Excel.

