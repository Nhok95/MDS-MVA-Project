#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))
set.seed(123)

## IMPORTS ##

library(rstudioapi)
library(factoextra)

## SETTING WORKSPACE ##

current_path = getActiveDocumentContext()$path 
setwd(dirname(current_path))

## LOADING DATASET ##
##load("bikeDataSet.RData")
load("bikeDataSet_train.RData")

# Select observations from numerical features which are not considered outliers
bikeDataSetClean = select(bikeDataSet_train, -c("Id"))

bikeDataSetClean$Hour = as.numeric(bikeDataSetClean$Hour)

rowlabels = bikeDataSetClean$Season

rownames(bikeDataSetClean) = paste(rowlabels, rownames(bikeDataSetClean), sep="-")

num_col = unlist(lapply(bikeDataSetClean, is.numeric))
df <- scale(bikeDataSetClean[, num_col]) # x - mean / sd
head(df)

## Error in rowSums(as.matrix(ok)) : 
## 'Calloc' could not allocate memory (74114881 of 16 bytes)
##matrix_BDS = data.matrix(df) # get a matrix version
##res.dist <- get_dist(matrix_BDS, method = "pearson") 
##fviz_dist(res.dist, lab_size = 8)


res.km <- eclust(df, k = 3, "kmeans", nstart = 25)
#fviz_gap_stat(res.km$gap_stat) #best num of clusters

fviz_silhouette(res.km) #negative silhouette means that is wrong assigned

###### Postprocessing - Getting Profiles

results<-data.frame(bikeDataSetClean,res.km$cluster)

n.winter = length(which(results$Season == "Winter")) # 1499
n.clust3 = length(which(results$Season == "Winter" & results$res.km.cluster == 3)) #1123

prop.winter = n.clust3 / n.winter # 0.75%

n.summer = length(which(results$Season == "Summer")) # 1537
n.clust2 = length(which(results$Season == "Summer" & results$res.km.cluster == 2)) #653

prop.summer = n.clust2 / n.summer # 0.42%


n.summer = length(which(results$Season == "")) # 1537
n.clust2 = length(which(results$Season == "Summer" & results$res.km.cluster == 2)) #653

prop.summer = n.clust2 / n.summer # 0.42%






data("USArrests")
df2 <- scale(USArrests)  # x - mean / sd
head(df2)
res.dist <- get_dist(df2, method = "pearson")
fviz_dist(res.dist, lab_size = 8)
res.km2 <- eclust(df2, "kmeans", nstart = 25) #clustering with factominer, nstart values represents the number of configurations generated
fviz_gap_stat(res.km2$gap_stat) #best num of clusters
fviz_silhouette(res.km2) #negative silhouette means that is wrong assigned
res.km2$nbclust # the same (3 clusters)
res.km2
fviz_cluster(res.km2)

###### Postprocessing - Getting Profiles

results<-data.frame(USArrests,res.km$cluster)
write.csv(results,"results.csv")
##### Check these centroids
res.km$centers

#### Real centroids

aggregate(USArrests,by=list(res.km$cluster),FUN=mean) 
# Grup 2 is the most dangerous
# Grup 1 is the most safe
# Grup 3 wins in urbanPop

aggregate(USArrests,by=list(res.km$cluster),FUN=median)
aggregate(USArrests,by=list(res.km$cluster),FUN=sd)
aggregate(USArrests,by=list(res.km$cluster),FUN=IQR) #distance between min and max values (q1 to q3)

### Checking Variables

##Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation
### where there are more than two groups. It's recommended when the assumptions of one-way ANOVA test are not met.
results$res.km.cluster<-as.factor(results$res.km.cluster)
levels(results$res.km.cluster)
results$res.km.cluster<- ordered(results$res.km.cluster,levels = c("1", "2", "3"))

boxplot(results$Murder~results$res.km.cluster, main= "Murder")
boxplot(results$Rape~results$res.km.cluster, main= "Rape")

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