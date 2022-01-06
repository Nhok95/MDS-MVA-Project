#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(mice)
library(tidyverse)
library(VIM)
library(DMwR2)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

## LOADING DATASET ##

bikeDataSet <- read.csv("SeoulBikeData.csv",header=T, sep=",")

bikeDataSet$Day = as.factor(bikeDataSet$Day)
bikeDataSet$Month = as.factor(bikeDataSet$Month)
bikeDataSet$Year = as.factor(bikeDataSet$Year)
bikeDataSet$Date = as.factor(bikeDataSet$Date)
bikeDataSet$Hour = as.factor(bikeDataSet$Hour)
bikeDataSet$Season <- as.factor(bikeDataSet$Season)
bikeDataSet$Holiday <- as.factor(bikeDataSet$Holiday)
bikeDataSet$Functioning.Day <- as.factor(bikeDataSet$Functioning.Day)

str(bikeDataSet)


### Outliers List ### 

spr_aut_outliers <- read.csv("outliers/Spring_Autumn_outliers_filtered.csv",header=T, sep=",")
summer_outliers  <- read.csv("outliers/Summer_outliers_filtered.csv",header=T, sep=",")
winter_outliers  <- read.csv("outliers/Winter_outliers_filtered.csv",header=T, sep=",")

### Removing all the outliers

get_DS_Without_Outliers = function(ds, outlierList) {
  
  NaCount = 0
  
  for (j in c(1:ncol(outlierList))) {
    
    column = colnames(outlierList)[j]
    print(sprintf("For column: %s", column))
    
    
    for (i in c(1:nrow(outlierList))) {
      id = outlierList[i,j]
      print(sprintf("--ID: %s", id))
      if (!is.na(id)) {
        
        NaCount = NaCount + 1
        print(sprintf("----bikeDataSet[%s,%s] = %s now is NA", id, column, ds[id,column]))
        ds[id,column] <- NA
        
      }
    }
  }
  
  print(sprintf(" #NAs: %s", NaCount))
  
  return(ds)
  
}

bikeDataSet = get_DS_Without_Outliers(bikeDataSet, winter_outliers)  #128 NAs
bikeDataSet = get_DS_Without_Outliers(bikeDataSet, summer_outliers)  #34 NAs
bikeDataSet = get_DS_Without_Outliers(bikeDataSet, spr_aut_outliers) #86 NAs

length(which(is.na(bikeDataSet))) # Total: 248 NAs

str(bikeDataSet)


### Data imputation with MICE ###

## CORRELATIONS
library(corrplot)
DS <- bikeDataSet[,c(6,8:15)]
#plot(DS)

cr = cor(DS, use="complete.obs")
corrplot(cr, method="number", type="lower", bg='lightslategray')
################

## SKEWNESS
library(e1071)
skewness(DS$Rented.Bike.Count, na.rm = TRUE)     # 1.150
skewness(DS$Temperature, na.rm = TRUE)           #-0.197
skewness(DS$Humidity, na.rm = TRUE)              # 0.093
skewness(DS$Wind.Speed, na.rm = TRUE)            # 0.781
skewness(DS$Visibility, na.rm = TRUE)            #-0.703
skewness(DS$Dew.Point.Temperature, na.rm = TRUE) #-0.367
skewness(DS$Solar.Radiation, na.rm = TRUE)       # 1.503
skewness(DS$Rainfall, na.rm = TRUE)              # 16.35
skewness(DS$Snowfall, na.rm = TRUE)              # 8.438
################


## PLOTS
DS <- bikeDataSet[,c(6,8:14)]

aggr(DS,
     numbers=TRUE,
     sortVars=TRUE,
     cex.axis=1.1,
     labels=c('RBikes','Temp','Humid.',
              'WindSp','Visib.','DPTem',
              'Sol.Rad','Rainf'),
     gap=1,
     ylab=c('Histogram of Missing data', 'Pattern'))

# We realize the % of missing data are really low

################


summary(bikeDataSet)
str(bikeDataSet)

###
impDF = bikeDataSet[,c(6:16)]
str(impDF)


imp=mice(impDF,m=1, maxit = 50)
densityplot(imp)
bikeDS_imp2=complete(imp)
length(which(is.na(bikeDS_imp)))




imp=mice(bikeDataSet ,m=1, maxit = 15)
bikeDS_imp=complete(imp)
length(which(is.na(bikeDS_imp)))

densityplot(imp)
#densityplot(imp, ~ Rented.Bike.Count | .imp)


# Temperature,Dew.Point.Temperature, Visilibity and Humidity seem to are not correctly imputed
# If we compare the 2 complete distributions, we realize that are practically the same
# As the proportion of NAs in this columns is really slow is hard to get a good distribution. 

par(mfrow=c(2,4))
indRBC = which(is.na(impDF$Rented.Bike.Count)); length(indRBC)       
plot(density(impDF$Rented.Bike.Count, na.rm = T), lwd=4, col="blue", main="Rented Bike Count")
lines(density(bikeDS_imp$Rented.Bike.Count), lwd=1, col="red")
#lines(density(bikeDS_imp$Rented.Bike.Count[indRBC]), lwd=1, col="orange")

indTemp = which(is.na(impDF$Temperature)); length(indTemp)       
plot(density(impDF$Temperature, na.rm = T), lwd=4, col="blue", main="Temperature")
lines(density(bikeDS_imp$Temperature), lwd=1, col="red")
#lines(density(bikeDS_imp$Temperature[indTemp]), lwd=1, col="orange")

indHum = which(is.na(impDF$Humidity)); length(indHum)      
plot(density(impDF$Humidity, na.rm = T), lwd=4, col="blue", main="Humidity")
lines(density(bikeDS_imp$Humidity), lwd=1, col="red")
#lines(density(bikeDS_imp$Humidity[indHum]), lwd=1, col="orange")

indWind = which(is.na(impDF$Wind.Speed)); length(indWind)  
plot(density(impDF$Wind.Speed, na.rm = T), lwd=4, col="blue", main="Wind Speed")
lines(density(bikeDS_imp$Wind.Speed), lwd=1, col="red")
#lines(density(bikeDS_imp$Wind.Speed[indWind]), lwd=1, col="orange")

indVis = which(is.na(impDF$Visibility)); length(indVis)      
plot(density(impDF$Visibility, na.rm = T), lwd=4, col="blue", main="Visibility")
lines(density(bikeDS_imp$Visibility), lwd=1, col="red")
#lines(density(bikeDS_imp$Visibility[indVis]), lwd=1, col="orange")

indDPT = which(is.na(impDF$Dew.Point.Temperature)); length(indDPT)      
plot(density(impDF$Dew.Point.Temperature, na.rm = T), lwd=4, col="blue", main="DPT")
lines(density(bikeDS_imp$Dew.Point.Temperature), lwd=1, col="red")
#lines(density(bikeDS_imp$Dew.Point.Temperature[indDPT]), lwd=1, col="orange")

indRain = which(is.na(impDF$Rainfall)); length(indRain)     
plot(density(impDF$Rainfall, na.rm = T), lwd=4, col="blue", main="Rainfall")
lines(density(bikeDS_imp$Rainfall), lwd=1, col="red")
#lines(density(bikeDS_imp$Rainfall[indRain]), lwd=1, col="orange")


par(mfrow=c(1,1))


#bikeDataSet$Rented.Bike.Count <- bikeDS_imp$Rented.Bike.Count
#bikeDataSet$Wind.Speed <- bikeDS_imp$Wind.Speed
#bikeDataSet$Rainfall <- bikeDS_imp$Rainfall

#length(which(is.na(bikeDataSet))) #61


# knn approach
#impDF = bikeDataSet[,c(6:16)]

#impknn = knnImputation(impDF, k=5) # k-nearest neightbors
#length(which(is.na(impknn)))

#summary(impknn)
#summary(impDF)



write.table(bikeDS_imp2, file = "SeoulBikeData_FirstImp.csv", quote = FALSE, 
            sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
