#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
library(mice)
#library(tibble)
#library(dplyr)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

## LOADING DATASET ##

bikeDataSet <- read.csv("SeoulBikeData.csv",header=T, sep=",")

bikeDataSet$Date <- as.factor(bikeDataSet$Date)
bikeDataSet$Season <- as.factor(bikeDataSet$Season)
bikeDataSet$Holiday <- as.factor(bikeDataSet$Holiday)
bikeDataSet$Functioning.Day <- as.factor(bikeDataSet$Functioning.Day)

str(bikeDataSet)

original <- bikeDataSet
#bikeDataSet <- original
### Outliers List ### 

#outliers <- list(c(1,8), c(2,9))

spr_aut_outliers <- read.csv("outliers/Spring_Autumn_outliers_filtered.csv",header=T, sep=",")
summer_outliers  <- read.csv("outliers/Summer_outliers_filtered.csv",header=T, sep=",")
winter_outliers  <- read.csv("outliers/Winter_outliers_filtered.csv",header=T, sep=",")

#outliers <- list(c(1,"Temperature"), c(2,"Humidity"))

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



### First data imputation with MICE ###
init = mice(bikeDataSet, maxit=0)
meth = init$method
predM = init$predictorMatrix

predM[c("Id"),]=0
predM[,c("Id")]=0

predM[c("Day"),]=0
predM[,c("Day")]=0

predM[c("Month"),]=0
predM[,c("Month")]=0

predM[c("Year"),]=0
predM[,c("Year")]=0

predM[c("Date"),]=0
predM[,c("Date")]=0

meth[c("Temperature")]="norm"
meth[c("Humidity")]="norm"

predM; meth

k = 5
imp=mice(bikeDataSet,m=k, method=meth, predictorMatrix = predM)



bikeDS_imp=complete(imp)
original[c(1,2),]
bikeDataSet[c(1,2),]
bikeDS_imp[c(1,2),]



#########################################################
for (j in c(1:ncol(winter_outliers))) {
  
  column = colnames(winter_outliers)[j]
  print(sprintf("For column: %s", column))
  
  
  for (i in c(1:nrow(winter_outliers))) {
    id = winter_outliers[i,j]
    print(sprintf("--ID: %s", id))
    if (!is.na(id)) {
      print(sprintf("----bikeDataSet[%s,%s] = %s now is NA", id, column, bikeDataSet[id,column]))
      #bikeDataSet[id,column] <- NA
    }
  }
  
}


