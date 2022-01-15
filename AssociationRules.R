#####################################
##            Ass. Rules           ##
#####################################

library(rstudioapi)
library(stringr)
library(arules)
library(arulesViz)
library(dplyr)
library(datasets)

library(corrplot)

library(FactoMineR)
library(factoextra)

rm(list=ls(all=TRUE))
set.seed(123)

## Function
calcQ <- function(x) {
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3], 
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr ) 
}

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Read the dataset which has the outliers indicated
load("bikeDataSet.RData")

bikeDataSetNoMout = bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]

bikeDataSetClean.num = select(bikeDataSetNoMout, c("Temperature",
                                               "Humidity",
                                               #"Wind.Speed",
                                               #"Visibility",
                                               #"Dew.Point.Temperature",
                                               "Solar.Radiation",
                                               "Rainfall",
                                               "Snowfall",
                                               "Rented.Bike.Count"))

# More or less independent variables
corrplot(cor(bikeDataSetClean.num), method="number", type="lower", bg='lightslategray') 

bikeDataSetClean = select(bikeDataSetNoMout, c("Temperature",
                                               "Humidity",
                                               #"Rainfall",
                                               #"Snowfall",
                                               "Month",
                                               "Hour",
                                               #"Season",
                                               "Rented.Bike.Count"))

# Convert first 5 numeric vars to Factor

str(bikeDataSetClean)

for (i in 1:3) {
  if (i < 3) { #Temperature, Humidity
    quartiles = calcQ(bikeDataSetClean[,i])
    bikeDataSetClean[,i] <- cut(bikeDataSetClean[,i],
                                breaks = c(quartiles$min, quartiles$q1, quartiles$q2, quartiles$q3, quartiles$max),
                                include.lowest = TRUE)
    #levels(bikeDataSetClean[,i]) = c("low", "mid", "high", "v-high")
    # table(bikeDataSetClean[,i])
  }
  # } else { # Rainfall, Snowfall
  #   quartiles = calcQ(bikeDataSetClean[,i]);
  #   bikeDataSetClean[,i] <- factor(as.logical(bikeDataSetClean[,i]))
  #   levels(bikeDataSetClean[,i]) = c("No", "Yes")
  # }
  
}

# Factor target
RBC.q = calcQ(bikeDataSetClean$Rented.Bike.Count)
bikeDataSetClean$Rented.Bike.Count <- cut(bikeDataSetClean$Rented.Bike.Count,
                                      breaks = c(RBC.q$min, RBC.q$q1, RBC.q$q2, RBC.q$q3, RBC.q$max),
                                      include.lowest = TRUE)
levels(bikeDataSetClean$Rented.Bike.Count) = c("low", "mid", "high", "v-high")
table(bikeDataSetClean$Rented.Bike.Count)

names(bikeDataSetClean)[names(bikeDataSetClean) == "Rented.Bike.Count"] <- "RB"

str(bikeDataSetClean)


############## Association Rules Analysis ##############


rdata = transactions(bikeDataSetClean)
inspect(head(rdata))

itemFrequencyPlot(rdata, support = 0.01, cex.names=0.8, topN = 5)

rules = apriori(data = rdata, parameter = list(support = 0.005, confidence = 0.85))
summary(rules)
inspect(head(rules,by = "confidence"))

rules.target.low <- subset(rules, subset = rhs %in% "RB=low" & lift > 1.5)
rules.target.mid <- subset(rules, subset = rhs %in% "RB=mid" & lift > 1.5)
rules.target.high <- subset(rules, subset = rhs %in% "RB=high" & lift > 1.5)
rules.target.vhigh <- subset(rules, subset = rhs %in% "RB=v-high" & lift > 1.5)

inspect(rules.target.low,by = "lift")
inspect(rules.target.mid,by = "lift")
inspect(rules.target.high,by = "lift")
inspect(rules.target.vhigh,by = "lift")

# inspect(head(rules.target.low,by = "lift",10))
# inspect(head(rules.target.mid,by = "lift",10))
# inspect(head(rules.target.high,by = "lift",10))
# inspect(head(rules.target.vhigh,by = "lift",10))
