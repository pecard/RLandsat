#'#################################################################################################
#' Adapted from Sesnie  code 4/24/13
#' Used primarily to Compare random forests (RF) with support vector machines (SVM)
#' Split out 2/3 of the pixel data from taining and 
#' use 1/3 for test set to compare RF and SVM
#' R version 3.0.2 rgdal_0.8-16  raster_2.2-16 sp_1.0-14

#'### TO READ
#' http://gis.stackexchange.com/questions/39021/how-to-perform-random-forest-land-classification
#' This may be interesting to add: a spatial component as a new band.
#' http://www.tandfonline.com/doi/full/10.1080/01431160903252327#tabModule
#' http://horicky.blogspot.de/2012/06/predictive-analytics-decision-tree-and.html
#' http://menugget.blogspot.pt/2014/04/decision-making-trees-and-machine.html
#'#################################################################################################

#'# Load/Install Packages #########################################################################
kpacks <- c("raster", "sp", "rgdal", 'randomForest', 'ggplot2', 'e1071',
            'reshape2')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Select a training and validation data matrix already prepared from image ROI ################### 
#' Will use sigs or qsigs as imput data
dataset <- reshape2::dcast(qsigs, class + newcl ~ band, value.var = 'value')
dataset <- select(dataset, -newcl)
head(dataset, 10)

#' Split up validation and training data ##########################################################
index <- 1:nrow(dataset)
testindex <- sample(index, trunc(length(index)/3))
testset <- dataset[testindex, ]
trainset <- dataset[-testindex, ]

#' Calibrate Models ###############################################################################
#' Support Vector Machine
svm.model <- svm(class~., data=trainset, cost = 100, gamma =1)
svm.pred <- predict(svm.model, testset[, -1])
#' Random Forest
rf.model <- randomForest(factor(class)~ ., data = trainset, importance=TRUE, ntree=2000)
print(rf.model)
#' Predict with test dataset
rf.pred <- predict(rf.model, testset[, -1], type = "class")
#' Confusion matrix
#table(pred = svm.pred, true =testset[, 1])
table(pred = rf.pred, true =testset[, 1])

#' Check variable importance
print(round(rf.model$importance,2))
varImpPlot(rf.model)

#' Use a simple fixed sample to tune SVM (alternatively "boot" can be used but takes too long)
obj <- tune(svm, Cover~., data=class_data, ranges=list(gamma = 2^(-1:1), cost = 2^(2:7)),
            tunecontrol=tune.control(sampling = "fix"))
summary(obj)

#' Identify the best-performing parameter values
gammaParam<-obj$performances$gamma[which(obj$performances$error==min(obj$performances$error))]
costParam<-obj$performances$cost[which(obj$performances$error==min(obj$performances$error))]

#' Run the svm model using the best-performing parameter values
svm.model <- svm(Cover~., data = trainset, cost=costParam, gamma=gammaParam)
svm.pred <- predict(svm.model, testset[,-1])
table(pred <- svm.pred, true = testset[,1])

#' Compare model agreement
plot(obj)
tab <- table(rf.pred, svm.pred)
classAgreement(tab)



#' Tune RF model with training data & validate with testset for tuned SVM comparison
set.seed(51)
mtry <- tuneRF(trainset[ ,-1], factor(trainset[ ,1]), mtryStart = 2, stepFactor=2, ntreeTry=2000,
               improve=0.01, do.trace=250, plot=TRUE)

print(mtry)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1];

print(best.m)

#' Fit an RF model with tuning parameters and compare with SVM

set.seed(79)
rf.model <- randomForest(factor(class)~ ., data = trainset, mtry = best.m, ntree = 3000,
                         do.trace = 250, importance = TRUE)
rf.pred <- predict(rf.model, testset[, -1], type = "class" )

table(pred = rf.pred, true =testset[, 1])

#table(pred = svm.pred, true = testset[,1])

tab <- table(rf.pred, svm.pred)
classAgreement(tab)

print(rf.model)

#' Check variable importance 
varImpPlot(rf.model)
print(round(rf.model$importance,2))



#' Use all the data to develope an SVM model to run

svm.model2 <- svm(Cover~., data=class_data, cost=costParam, gamma=gammaParam)

print(svm.model2)


#' Develop a tunded RF model with ALL  data for prediction #######################################
#' Add a tuning step to improve RF performance with all of the data
set.seed(51)
mtry <- tuneRF(dataset[ ,2:7], factor(dataset[ ,1]), mtryStart = 2, stepFactor=2,
               ntreeTry=2000, improve=0.01, do.trace=250, plot=TRUE)
print(mtry)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1];

#' Fit an RF model with tuning parameters #########################################################
set.seed(79)
rf.model2 <- randomForest(factor(class)~ ., data = dataset, mtry = best.m,
                          ntree = 3000, do.trace = 250, importance = TRUE)

ls(class_data)
str(class_data)

print(rf.model2)

varImpPlot(rf.model)
print(round(rf.model2$importance,2))

#' Run predictions with RF models #################################################################
#' See This: Sort the class names to keep track
rfkumbira <- predict(etm_stk, rf.model2, progress="text")
plot(rfkumbira)
writeRaster(rfkumbira, filename=("Outputs/rf_kumbira.tif"),
            format="GTiff", datatype="INT1U")

#' Code not needed with doSNOW
stopCluster(cluster) # to free up the cpus
