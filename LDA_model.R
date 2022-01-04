#####################################
##            LDA Model            ##
#####################################

library(rstudioapi)
library(tidyverse)
library(caret)
library(reshape2)
library(knitr)
library(dplyr)
library(MVN)
library(biotools)
library(MASS)
library(mda)
library(klaR)

rm(list=ls(all=TRUE))
set.seed(123)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

## Function
calcQ <- function(x) {
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3], 
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr ) 
}

# Read the dataset which has the outliers indicated
load("bikeDataSet.RData")

bikeDS.Clean = bikeDataSet[which(bikeDataSet$mout == "NoMOut"),]
bikeDS.Clean = dplyr::select(bikeDS.Clean, -c("Id","Day","Month","Year","mout"))

# Factor target
RBC.q = calcQ(bikeDS.Clean$Rented.Bike.Count)
bikeDS.Clean$Rented.Bike.Count <- cut(bikeDS.Clean$Rented.Bike.Count,
                                      breaks = c(RBC.q$min, RBC.q$q1, RBC.q$q2, RBC.q$q3, RBC.q$max),
                                      include.lowest = TRUE)
levels(bikeDS.Clean$Rented.Bike.Count) = c("low", "mid", "high", "very high")


# Testing Gaussian conditions with Shapiro-Wilk test for each variable
tidyBDS <- melt(bikeDS.Clean, value.name = "value")
#kable(head(bikeDS.Clean, n = 3))
kable(tidyBDS %>% group_by(Rented.Bike.Count, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(value)$p.value,5)))


## COND 1: Features need to have Gaussian Density [NOT OK]

# Checking for Multivariate Gaussian Conditions (Royston test and/or Henze-Zirkler test)

num_col = unlist(lapply(bikeDS.Clean, is.numeric))

sample.values = sample(1:nrow(bikeDS.Clean),2000)
DS.sample = bikeDS.Clean[sample.values,]


royston_test <- mvn(data = DS.sample[num_col], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality # MVN NO 
hz_test <- mvn(data = bikeDS.Clean[num_col], mvnTest = "hz")
hz_test$multivariateNormality # MVN NO

## COND 2: NOT OK

# Covariance Conditions

boxM(data = bikeDS.Clean[num_col], grouping = bikeDS.Clean[,2])

## COND 3: [NOT OK]

##### MODEL ######
##Step1: Split Data
load("bikeDataSet_train.RData")
load("bikeDataSet_test.RData")

#Train.Clean = dplyr::select(bikeDataSet_train, 
#                            -c("Id","Day","Month","Year","Date","Hour","Season","Holiday","Functioning.Day"))
Train.Clean = dplyr::select(bikeDataSet_train, 
                            -c("Id","Day","Month","Year"))
Test.Clean = dplyr::select(bikeDataSet_test, 
                           -c("Id","Day","Month","Year"))

#Train.Clean = dplyr::select(bikeDataSet_train, -c("Id","Day","Month","Year", "Dew.Point.Temperature"))
#Test.Clean = dplyr::select(bikeDataSet_test, -c("Id","Day","Month","Year", "Dew.Point.Temperature"))


Train.Clean$Rented.Bike.Count = cut(Train.Clean$Rented.Bike.Count,
                              breaks = c(RBC.q$min, RBC.q$q1, RBC.q$q2, RBC.q$q3, RBC.q$max),
                              include.lowest = TRUE)
levels(Train.Clean$Rented.Bike.Count) = c("low", "mid", "high", "very high")

Test.Clean$Rented.Bike.Count = cut(Test.Clean$Rented.Bike.Count,
                              breaks = c(RBC.q$min, RBC.q$q1, RBC.q$q2, RBC.q$q3, RBC.q$max),
                              include.lowest = TRUE)
levels(Test.Clean$Rented.Bike.Count) = c("low", "mid", "high", "very high")


##Step 2. Calculating LDA function

# Estimate preprocessing parameters
preproc.param = Train.Clean %>%preProcess(method = c("center", "scale")) #scale data 
# Transform the data using the estimated parameters
train.transformed = preproc.param %>% predict(Train.Clean)
test.transformed = preproc.param %>% predict(Test.Clean)

model <- lda(Rented.Bike.Count~., data = train.transformed)
model

#plot(model)

#p = predict(model, train.transformed)
#ldahist(data= p$x[,1], g = train.transformed$Rented.Bike.Count)
#ldahist(data= p$x[,2], g = train.transformed$Rented.Bike.Count)
#ldahist(data= p$x[,3], g = train.transformed$Rented.Bike.Count)

#partimat(Rented.Bike.Count~., data= test.transformed, method="lda")

##Step 3. Make predictions

predictions <- model %>% predict(test.transformed)

head(predictions$posterior, 6)  
tail(predictions$posterior, 6)

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = Rented.Bike.Count))

#library("plot3D")
#scatter3D(lda.data$LD1,lda.data$LD2,lda.data$LD3, clab= lda.data$Rented.Bike.Count)

# Confusion matrix is the most important tool to evaluate
mean(predictions$class==test.transformed$Rented.Bike.Count) # Evaluation ()
table(test.transformed$Rented.Bike.Count, predictions$class, dnn = c("Actual Class", "Predicted Class"))
Error <- mean(test.transformed$Rented.Bike.Count != predictions$class) * 100; Error

###########EXTRA QDA 
#### QDA is useful when Covariance condition (LDA) is not possible. It is more flexible than LDA (high bias if conditions are not met), but risky in terms of variance (higher than LDA)
#### LDA for small datasets and QDA for large datasets.

# Fit the model
model.rda = rda(Rented.Bike.Count~., data = train.transformed)
model.rda
# Make predictions
predictions.rda = model.rda %>% predict(test.transformed)
# Model accuracy
mean(predictions.rda$class == test.transformed$Rented.Bike.Count)
table(test.transformed$Rented.Bike.Count, predictions.rda$class, dnn = c("Actual Class", "Predicted Class"))
