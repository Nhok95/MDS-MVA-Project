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

library(plotly) #3d plot
library(moments) #skewness
library(bestNormalize) #BestNormalize function

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
table(bikeDS.Clean$Rented.Bike.Count)


# Testing Gaussian conditions with Shapiro-Wilk test for each variable
num_col = unlist(lapply(bikeDS.Clean, is.numeric))

tidyBDS <- melt(bikeDS.Clean,
                value.name = "value")
#kable(head(bikeDS.Clean, n = 3))
kable(tidyBDS %>% group_by(Rented.Bike.Count, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(value)$p.value,5)))


## COND 1: Features need to have Gaussian Density [NOT OK]

# Checking for Multivariate Gaussian Conditions (Royston test and/or Henze-Zirkler test)

sample.values = sample(1:nrow(bikeDS.Clean),2000)
DS.sample = bikeDS.Clean[sample.values,]


royston_test <- mvn(data = DS.sample[num_col], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality # MVN NO 
hz_test <- mvn(data = bikeDS.Clean[num_col], mvnTest = "hz")
hz_test$multivariateNormality # MVN NO

## COND 2: Multidimensional Gaussian Density [NOT OK]

# Covariance Conditions

boxM(data = bikeDS.Clean[num_col], grouping = bikeDS.Clean[,2])

## COND 3: Covariance/Variance [NOT OK]


### Normalizing

normalizedDF = bikeDS.Clean

plot(density(normalizedDF$Temperature))
skewness(bikeDS.Clean$Temperature) #-0.191

temp.norm = bestNormalize(normalizedDF$Temperature)
plot(density(temp.norm$x.t))
skewness(temp.norm$x.t) # 3.07e-05


df.low = normalizedDF[which(normalizedDF$Rented.Bike.Count == "low"),]
df.mid = normalizedDF[which(normalizedDF$Rented.Bike.Count == "mid"),]
df.high = normalizedDF[which(normalizedDF$Rented.Bike.Count == "high"),]
df.vhigh = normalizedDF[which(normalizedDF$Rented.Bike.Count == "very high"),]

df.low[num_col] = unlist(lapply(df.low[num_col], function(x) bestNormalize(x)$x.t))
df.mid[num_col] = unlist(lapply(df.mid[num_col], function(x) bestNormalize(x)$x.t))
df.high[num_col] = unlist(lapply(df.high[num_col], function(x) bestNormalize(x)$x.t))

num_col2 = num_col
num_col2[11] = F
df.vhigh[num_col2] = unlist(lapply(df.vhigh[num_col2], function(x) bestNormalize(x)$x.t))

#df.vhigh$Snowfall = bestNormalize(df.vhigh$Snowfall)$x.t #Error (all values minus 1 are 0)

normalizedDF = rbind(df.low,df.mid,df.high,df.vhigh)

tidyBDS.norm <- melt(normalizedDF, value.name = "value") 
kable(tidyBDS.norm %>% group_by(Rented.Bike.Count, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(value)$p.value,5)))

# Still we can't reach normality



##### MODEL ######
##Step1: Split Data
load("bikeDataSet_train.RData")
load("bikeDataSet_test.RData")

Train.Clean = dplyr::select(bikeDataSet_train, 
                            -c("Id","Day","Month","Year"))
Test.Clean = dplyr::select(bikeDataSet_test, 
                           -c("Id","Day","Month","Year"))


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

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = Rented.Bike.Count))

predictions.train <- model %>% predict(train.transformed)
dataset = data.frame(Y = train.transformed$Rented.Bike.Count, lda = predictions.train$x)

plot_ly(dataset, x = ~lda.LD1, y = ~lda.LD2, z = ~lda.LD3, color = ~Y, colors = c('red', 'green', 'blue', 'purple')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'LD1'),
                      yaxis = list(title = 'LD2'),
                      zaxis = list(title = 'LD3')))

##Step 3. Make predictions
predictions <- model %>% predict(test.transformed)

#head(predictions$posterior, 6)  
#tail(predictions$posterior, 6)

# Confusion matrix is the most important tool to evaluate
mean(predictions$class==test.transformed$Rented.Bike.Count) #73.51%
res.table = table(test.transformed$Rented.Bike.Count, predictions$class, 
                  dnn = c("Actual Class", "Predicted Class")); res.table
Error <- mean(test.transformed$Rented.Bike.Count != predictions$class) * 100; Error

neighbours.errors = 147+90+100+83+144+95
total.errors = sum(res.table) - sum(diag(res.table))
round(neighbours.errors/total.errors*100,2) #96.35% of errors comes from neighbouring clusters


###########EXTRA 

# Fit the model
Train.Clean2 = dplyr::select(Train.Clean, -c("Date"))
Test.Clean2 = dplyr::select(Test.Clean, -c("Date"))
preproc.param2 = Train.Clean2 %>%preProcess(method = c("center", "scale")) #scale data 
# Transform the data using the estimated parameters
train.transformed2 = preproc.param2 %>% predict(Train.Clean2)
test.transformed2 = preproc.param2 %>% predict(Test.Clean2)


##### Mixture discriminant analysis - MDA
model.mda = mda(Rented.Bike.Count~., data = train.transformed2)
model.mda
# Make predictions
predictions.mda = model.mda %>% predict(test.transformed2)
# Model accuracy
mean(predictions.mda == test.transformed2$Rented.Bike.Count)*100 #72.78%
table(test.transformed$Rented.Bike.Count, predictions.mda, dnn = c("Actual Class", "Predicted Class"))


##### Flexible discriminant analysis - FDA
model.fda = mda(Rented.Bike.Count~., data = train.transformed2)
model.fda
# Make predictions
predictions.fda = model.fda %>% predict(test.transformed2)
# Model accuracy
mean(predictions.fda == test.transformed2$Rented.Bike.Count)*100 #72.97%
table(test.transformed$Rented.Bike.Count, predictions.fda, dnn = c("Actual Class", "Predicted Class"))

##### Regularized discriminant analysis - RDA
model.rda = mda(Rented.Bike.Count~., data = train.transformed2)
model.rda
# Make predictions
predictions.rda = model.rda %>% predict(test.transformed2)
# Model accuracy
mean(predictions.rda == test.transformed2$Rented.Bike.Count)*100 #72.94%
table(test.transformed$Rented.Bike.Count, predictions.fda, dnn = c("Actual Class", "Predicted Class"))
