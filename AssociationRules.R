#####################################
##            Ass. Rules           ##
#####################################
install.packages("arules")

library(rstudioapi)
library(stringr)
library(arules)
library(arulesViz)
library(dplyr)
library(datasets)

library(FactoMineR)
library(factoextra)

rm(list=ls(all=TRUE))
set.seed(123)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read the dataset which has the outliers indicated
load("bikeDataSet.RData")

bikeDataSetClean = bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDataSetClean = select(bikeDataSetClean, -c("Id", "Day", "Month", "Year", "mout"))

rdata = transactions(bikeDataSetClean)
itemFrequencyPlot(rdata, topN = 10)

rules = apriori(data = rdata, parameter = list(support = 0.05, confidence = 0.9))

summary(rules)
inspect(head(rules))

strongrules = subset(rules, subset = lift > 1.2)

inspect(head(strongrules,15))

## Convert features into items using R

## We need to get trans_id

## Factor have levels (each level is an item); no problem
## Numerical; we need to transform it to factor 
##  * We can use median, quantiles, business decision (if >= k then high, else then low)

## We can use all features, or using only the most important ones.

## Output -> as we have an output, the idea is out case is try to find info by using ass. rules in order to know 
## what happen to my target (output); | LFH (most important levels) => RHS (target) |
## consequent = "output"

## For our dataset, out output is rented bike count (numerical), we need to transform it into factor (ej. high, medium, low demand).
## We should use the most important features
## We should use date as a transaction id

####

### FIRST STEP
## support => user condition (it's a treshold), in small datasets we could use [0.25 - 0.25]
##                                              in large datasets we might use [0.005-0.1]

### SECOND STEP
## Apriori -> (Confidence) -> measures that prob of LHS, then RHS, should be higher to 0.8
##         -> (Lift) -> check if confidence is true or not (true positive or false positive);
##            Lift == 1, we should forget the rule
##            Lift >>>>> 1. Good rule

##### Do it in R ######


## MCA/PCA to remove features that are not important
