#####################################
##            MVA LAB              ##
#####################################

rm(list=ls(all=TRUE))

## IMPORTS ##
library(rstudioapi)
library(rpart)
library(rpart.plot)
library(caret)
library(printr)
library(dplyr)
library(randomForest)

## SETTING WORKSPACE ##

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

load("bikeDataSet_train.RData")
load("bikeDataSet_test.RData")

# We do not consider 1st feature/column: Id, nor Date column (redundant info)
bikeDataSet_train <- bikeDataSet_train[, -c(1, 5)]
bikeDataSet_test <- bikeDataSet_test[, -c(1, 5)]
undesired <- c("Rented.Bike.Count")
x_train <- bikeDataSet_train %>% select(-one_of(undesired))
y_train <- bikeDataSet_train[, "Rented.Bike.Count"]
x_test <- bikeDataSet_test %>% select(-one_of(undesired))
y_test <- bikeDataSet_test[, "Rented.Bike.Count"]
# str(bikeDataSet_train)
# str(bikeDataSet_test)

### Model1: Regression Trees

# We first train a regression tree with more splits than optimal
tree <- rpart(Rented.Bike.Count ~ ., data=bikeDataSet_train, cp=0, method="anova")
rpart.plot(tree)
printcp(tree)
plotcp(tree)

# After obtaining the complex tree, we prune it
# Each row of cptable represents a different height of the tree
xerror <- tree$cptable[,"xerror"]
imin.xerror <- which.min(xerror)
tree$cptable[imin.xerror, ] # The tree with the minimum cross-validated relative error
upper.xerror <- xerror[imin.xerror] + tree$cptable[imin.xerror, "xstd"] # 1sd rule calculation
icp <- min(which(xerror <= upper.xerror))
cp_sel <- tree$cptable[icp, "CP"]
tree <- prune(tree, cp = cp_sel)
rpart.plot(tree)

# Checking feature importance
importance <- tree$variable.importance
importance <- round(100*importance/sum(importance), 1)
importance[importance >= 1]

# Checking quality of the tree's prediction
p <- unname(predict(tree, bikeDataSet_test))

### Complete Function for Accuracy KPIs
accuracy <- function(pred, obs, na.rm = FALSE, 
                     tol = sqrt(.Machine$double.eps)) {
  err <- obs - pred     # Errors
  if(na.rm) {
    is.a <- !is.na(err)
    err <- err[is.a]
    obs <- obs[is.a]
  }  
  perr <- 100*err/pmax(obs, tol)  # % errors
  return(c(
    me = mean(err),           # mean error
    rmse = sqrt(mean(err^2)), # sqrt mean squared error
    mae = mean(abs(err)),     # mean absolute error
    mpe = mean(perr),         # mean percentage error
    mape = mean(abs(perr)),   # mean absolute percentage error
    r.squared = 1 - sum(err^2)/sum((obs - mean(obs))^2)
  ))
}

accuracy(p, bikeDataSet_test$Rented.Bike.Count)
# Mean Square Error (MSE) is an absolute measure of the goodness for the fit.
# MSE gives larger penalization to big prediction error by square it.
# RMSE is the square root of MSE.

### Model2: Random Forest (heavy search for tunning parameters)

seed <- 7
metric <- 'RMSE'

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), class = rep("numeric", 2), label = c("maxnodes", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
predict(modelFit, newdata)

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Set grid search parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid', verboseIter=TRUE)

# Outline the grid of parameters
# tunegrid <- expand.grid(.maxnodes=c(70, 80, 90, 100), .ntree=c(900, 1000, 1100))
tunegrid <- expand.grid(.maxnodes=c(70, 80), .ntree=c(1000))
set.seed(seed)

# Train the model
rf_gridsearch <- train(x=x_train, y=y_train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control, verbose = TRUE)
print(rf_gridsearch)
print(rf_gridsearch$bestTune)
plot(rf_gridsearch)
print(rf_gridsearch$finalModel)
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')

# Model maxnodes = 80 & ntree = 1000 is better, lower RMSE

### Model chosen: Model2 since 279.0382 < 312.1432 (RMSE), now we use the test dataset

pred <- round(unname(predict(rf_gridsearch$finalModel, newdata=x_test)))
RMSE(pred, y_test) # RMSE: 290.2148
