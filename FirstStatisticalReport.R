#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##

library(rstudioapi)
#library(tibble)
#library(dplyr)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

## LOADING DATASET ##

bikeDataSet <- read.csv("SeoulBikeData.csv",header=T, sep=",")

## DATE CONVERSION TO INTEGER[OPTIONAL] ##
myDay <- substr(bikeDataSet$Date[1], start=1, stop=2)
#myDay <- sub("^0+", "", myDay)

myMonth <- substr(bikeDataSet$Date[1], start=4, stop=5)
#myMonth <- sub("^0+", "", myMonth)

myYear <- substr(bikeDataSet$Date[1], start=7, stop=10)

myDay;myMonth;myYear

newDateValue <- paste(myYear, myMonth, myDay, sep="")
newDateValue # Format YYYYMMDD

### MISSING DATA ###

#str(bikeDataSet)
summary(bikeDataSet)
mis_ind = rowSums(is.na(bikeDataSet))
table(mis_ind) 
m1 = which(mis_ind>0)
bikeDataSet[m1,] # 0 rows


#mis_col = colSums(is.na(bikeDataSet));mis_col
#m2<-which(mis_ind>0)
#bikeDataSet[m1,]
#table(mis_ind)




# No NAs 

## Categorical data treatment [Date, Hour, Seasons]

## Simple ##
day_levels = levels(as.factor(bikeDataSet$Date)); day_levels
length( day_levels ) #levels length == 365 days ~ No NAs

hour_levels = levels(as.factor(bikeDataSet$Hour)); hour_levels
length( hour_levels ) #levels length == 24 hours ~ No NAs

## Complex ##
date_table <- table(bikeDataSet$Date); date_table
length(date_table) #365 days
date_table[date_table != 24] #less or more than 24 hours per day -> 0 results

table(bikeDataSet$Hour)





### Outliers ###


#ds = filter(bikeDataSet, Seasons == "Winter")

ds_original = bikeDataSet[which(bikeDataSet$Seasons == "Winter"),]
ds = ds_original[,c(3,5:12)]


X_Summer = filter(new_bikeDataSet, Seasons == "Summer")
X_Spring = filter(new_bikeDataSet, Seasons == "Spring")
X_Autumn = filter(new_bikeDataSet, Seasons == "Autumn")
X = X_Summer[,c(3,5:12)]
X = X_Spring[,c(3,5:12)]
X = X_Autumn[,c(3,5:12)]

names(ds)

#boxplot(X, ylim=c(0,40))
#boxplot(X$Rented.Bike.Count ~ X$Seasons) # Autumn ~ Spring; Summer; Winter

boxplot(ds)
#Example
boxplot(ds$Temperature..C., 
        main="Temperature in Celsius (Winter)", 
        xlab="Degrees",
        ylab="Temperature",
        col="lightblue",
        border="black",
        horizontal=T, 
        notch=T)




boxplot(ds$Humidity...)
boxplot(ds$Wind.speed..m.s.)
boxplot(ds$Visibility..10m.)
boxplot(ds$Dew.point.temperature..C.)
boxplot(ds$Solar.Radiation..MJ.m2.)
boxplot(ds$Rainfall.mm.)
boxplot(ds$Snowfall..cm.)

# outliers indexs example
outliers_values = boxplot.stats(ds$Temperature..C.)$out
ds_outliers_index = ds_original$Id[ds$Temperature..C. == outliers_values]

hist(ds$Temperature..C., 
     xlim=c(-20,15),
     breaks = 100)

# No outlier?




