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
checkDiff = function(original, bikeDS_imp, bikeDataSet, meanBool="T") {
  diff = c()
  cnames = colnames(original[c(6,8,9,10,11,12,13,14)])
  
  for (i in c(1:length(cnames))) {
    actual = original[,cnames[i]][is.na(bikeDataSet[,cnames[i]])]
    predicted = bikeDS_imp[,cnames[i]][is.na(bikeDataSet[,cnames[i]])]
    print("###")
    print(sprintf("--column: %s", cnames[i]))
    
    if (meanBool) {
      print(mean(actual))
      print(mean(predicted))
      
      diff = append(diff,mean(actual) - mean(predicted))  
    } else {
      print(var(actual))
      print(var(predicted))
      
      diff = append(diff,var(actual) - var(predicted)) 
    }
    
    
    print(sprintf("----diff: %s ", diff[i]))
  }
  
  return(diff)
}

checkDiff2 = function(original, bikeDS_imp, meanBool="T") {
  diff = c()
  cnames = colnames(original[c(6,8,9,10,11,12,13,14)])
  
  for (i in c(1:length(cnames))) {
    actual = original[,cnames[i]]
    predicted = bikeDS_imp[,cnames[i]]
    print("###")
    print(sprintf("--column: %s", cnames[i]))
    
    if (meanBool) {
      print("Mean")
      print(mean(actual))
      print(mean(predicted))
      
      diff = append(diff, abs(mean(actual, na.rm=T) - mean(predicted, na.rm=T)))
    } else {
      print("Var")
      print(var(actual))
      print(var(predicted))
      
      diff = append(diff, abs(var(actual, na.rm=T) - var(predicted, na.rm=T)))
    }
    
    
    
    print(sprintf("----diff: %s ", diff[i]))
  }
  
  return(diff)
}

cnames = colnames(original[c(6,8,9,10,11,12,13,14)])

imp1=mice(bikeDataSet,m=25, maxit = 100)
bikeDS_imp=complete(imp1)
length(which(is.na(bikeDS_imp)))

plot(imp1)
densityplot(imp1)
#stripplot(imp, pch = 20, cex = 2)

#diff1 = checkMeanDiff(original, bikeDS_imp, bikeDataSet, TRUE)
diff1.mean = checkDiff2(original, bikeDS_imp, TRUE)
diff1.var = checkDiff2(original, bikeDS_imp, FALSE)




init = mice(bikeDataSet, maxit=0)
meth = init$method
predM = init$predictorMatrix

predM[c("Id"),]=0
predM[,c("Id")]=0

#predM[c("Day"),]=0
#predM[,c("Day")]=0

#predM[c("Month"),]=0
#predM[,c("Month")]=0

#predM[c("Year"),]=0
#predM[,c("Year")]=0

predM[c("Date"),]=0
predM[,c("Date")]=0

#meth[c("Rented.Bike.Count")]="norm"
#meth[c("Temperature")]="norm"
#meth[c("Humidity")]="norm"
#meth[c("Wind.Speed")]="norm"
#meth[c("Visibility")]="norm"
#meth[c("Dew.Point.Temperature")]="norm"
#meth[c("Solar.Radiation")]="norm"
#meth[c("Rainfall")]="norm"


#predM; meth

imp2=mice(bikeDataSet,m=25, maxit = 100, method=meth, predictorMatrix = predM)
bikeDS_imp2=complete(imp2)
length(which(is.na(bikeDS_imp2)))

plot(imp2)
densityplot(imp2)


diff2.mean = checkDiff2(original, bikeDS_imp2, TRUE)
diff2.var = checkDiff2(original, bikeDS_imp2, FALSE)

diff.df.mean = data.frame(diff1.mean,diff2.mean, row.names=cnames)
diff.df.mean = as.data.frame(t(diff.df.mean))

diff.df.var = data.frame(diff1.var,diff2.var, row.names=cnames)
diff.df.var = as.data.frame(t(diff.df.var))


write.table(bikeDS_imp2, file = "SeoulBikeData_FirstImp.csv", quote = FALSE, 
            sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)



