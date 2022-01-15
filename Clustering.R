#####################################
##            Clustering           ##
#####################################

library(cluster)
library(factoextra)
library(FactoMineR)
library(rstudioapi)
library(dplyr)
library(stringr)
library(gridExtra)
library(grid)

rm(list=ls(all=TRUE))
set.seed(123)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read the dataset which has the outliers indicated
load("bikeDataSet.RData")

# Select observations from numerical features which are not considered outliers
bikeDataSet.NoMout <- bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDataSet.NoMout = select(bikeDataSet.NoMout, -c("mout"))
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
dfClean = bikeDataSetClean
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

rowlabels = bikeDataSet.NoMout$Season
rownames(bikeDataSetClean) = paste(rowlabels, rownames(bikeDataSetClean), sep="-")

fviz_silhouette(clara.res) + 
  scale_color_hue(l=40, c=35) + 
  scale_fill_manual(values=c("blue", "green", "red")) #negative silhouette means that is wrong assigned

###### Postprocessing - Getting Profiles

results = data.frame(bikeDataSet.NoMout,clara.res$clustering)
#write.csv(profiling.results,"profiling_preliminar_results.csv")

plot(table(results$Season, results$clara.res.clustering), 
     col= c("blue","green", "red"),
     main = "Seasons vs Cluster")

names(results)[names(results) == 'clara.res.clustering'] = 'cluster'

clara.res$medoids

#### Real centroids

mean.table = aggregate(dfClean,by=list(clara.res$clustering),FUN= function(x) round(mean(x),1)); 
mean.table = mean.table[2:10]; mean.table
grid.newpage()
grid.table(mean.table)
# Grup 1 seems to represent winter (cold weather)
# Grup 2 seems to represent a transition between winter and summer (mild weather)
# Grup 3 seems to represent summer (warm weather)

median.table = aggregate(dfClean,by=list(clara.res$clustering),FUN=median); 
median.table = median.table[2:10]; median.table
grid.newpage()
grid.table(median.table)

sd.table = aggregate(dfClean,by=list(clara.res$clustering),FUN=function(x) round(sd(x),1)); 
sd.table = sd.table[2:10]; sd.table
grid.newpage()
grid.table(sd.table)

#distance between min and max values (q1 to q3)
IQR.table = aggregate(dfClean,by=list(clara.res$clustering),FUN=IQR); 
IQR.table = IQR.table[2:10]; IQR.table
grid.newpage()
grid.table(IQR.table)

### Checking Variables

##Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation
### where there are more than two groups. It's recommended when the assumptions of one-way ANOVA test are not met.
results$cluster<-as.factor(results$cluster)
str(results)
levels(results$cluster) = c("Cold","Mild","Warm")
results$cluster<- ordered(results$cluster,levels = c("Cold","Mild","Warm"))


##https://en.wikipedia.org/wiki/Climate_of_Seoul
boxplot(results$Temperature~results$cluster, main= "Temperature")
boxplot(results$Humidity~results$cluster, main= "Humidity")         
boxplot(results$Wind.Speed~results$cluster, main= "Wind Speed")
boxplot(results$Visibility~results$cluster, main= "Visibility")
boxplot(results$Dew.Point.Temperature~results$cluster, main= "DPTemp")
boxplot(results$Solar.Radiation~results$cluster, main= "Solar Radiation")
boxplot(results$Rainfall~results$cluster, main= "Rainfall")
boxplot(results$Snowfall~results$cluster, main= "Snowfall")


# As the p-value is less than the significance level in all test, 
# we can conclude that there are significant differences between groups in all our numerical variables
kruskal.test(Temperature ~ cluster, data = results) 
kruskal.test(Humidity ~ cluster, data = results) 
kruskal.test(Wind.Speed ~ cluster, data = results)
kruskal.test(Visibility ~ cluster, data = results)
kruskal.test(Dew.Point.Temperature ~ cluster, data = results)
kruskal.test(Solar.Radiation ~ cluster, data = results)
kruskal.test(Rainfall ~ cluster, data = results)
kruskal.test(Snowfall ~ cluster, data = results)

#From the output of the Kruskal-Wallis test, we know that there is a significant difference between groups, but we don't know which pairs of groups are different.
#Cold <> Mild <> Warm in all test
pairwise.wilcox.test(results$Temperature, results$cluster, p.adjust.method = "BH") 
pairwise.wilcox.test(results$Humidity, results$cluster, p.adjust.method = "BH")
pairwise.wilcox.test(results$Wind.Speed, results$cluster, p.adjust.method = "BH")
pairwise.wilcox.test(results$Visibility, results$cluster, p.adjust.method = "BH")
pairwise.wilcox.test(results$Dew.Point.Temperature, results$cluster, p.adjust.method = "BH")
pairwise.wilcox.test(results$Solar.Radiation, results$cluster, p.adjust.method = "BH")
pairwise.wilcox.test(results$Rainfall, results$cluster, p.adjust.method = "BH")
pairwise.wilcox.test(results$Snowfall, results$cluster, p.adjust.method = "BH")

results.sub = select(results, -c("Id","Year", "Date"))

length(results.sub)
catdes.res = catdes(results.sub, length(results.sub))
catdes.res$test.chi2
grid.newpage()
grid.table(catdes.res$test.chi2)

Cold.Cat = as.data.frame(catdes.res$category$Cold)
Cold.Cat[which(Cold.Cat$v.test > 0),]


Mild.Cat = as.data.frame(catdes.res$category$Mild)
Mild.Cat[which(Mild.Cat$v.test > 0),]


Warm.Cat = as.data.frame(catdes.res$category$Warm)
Warm.Cat[which(Warm.Cat$v.test > 0),]

catdes.res$quanti.var
catdes.res$quanti.var[,1] = round(catdes.res$quanti.var[,1], 3)
grid.newpage()
grid.table(catdes.res$quanti.var)

Cold.Quanti = catdes.res$quanti$Cold
Cold.Quanti[,c(2,3,4,5)] = round(Cold.Quanti[,c(2,3,4,5)],2)

Mild.Quanti = catdes.res$quanti$Mild
Mild.Quanti[,c(2,3,4,5)] = round(Mild.Quanti[,c(2,3,4,5)],2)

Warm.Quanti = catdes.res$quanti$Warm
Warm.Quanti[,c(2,3,4,5)] = round(Warm.Quanti[,c(2,3,4,5)],2)

Cold.Quanti
Mild.Quanti
Warm.Quanti

grid.newpage()
grid.table(catdes.res$category)

catdes.res$quanti.var
catdes.res$quanti
#For categorical variables, please use Chi-squared Test. R functin "catdes"

#####################################
##        Target Profiling         ##
#####################################

index = which(names(results.sub) == "Rented.Bike.Count")
condes.res = condes(results.sub, index)
condes.res$quanti
condes.res$quanti[,1] = round(condes.res$quanti[,1], 3)
grid.newpage()
grid.table(condes.res$quanti)

condes.res$quali 
condes.res$quali[,1] = round(condes.res$quali[,1], 3)
grid.newpage()
grid.table(condes.res$quali)


#Estimate of linear regression (target~category)
condes.res$category
condes.res$category[,1] = round(condes.res$category[,1], 3)

index.list = c(1,2,5,12,19,29,37,41,42,43)
condes.res.cat.1 = condes.res$category[index.list,]
grid.newpage()
grid.table(condes.res.cat.1)

condes.res.cat.2 = condes.res$category[-index.list,]
condes.res.cat.2 =condes.res.cat.2[which(condes.res.cat.2[,1] > 250 | 
                                         condes.res.cat.2[,1] < -250),]
grid.newpage()
grid.table(condes.res.cat.2)
