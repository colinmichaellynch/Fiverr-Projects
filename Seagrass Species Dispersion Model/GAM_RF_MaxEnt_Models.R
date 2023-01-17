### --- DATA PREPARATION --- ### 


#Ensure that these files are in the same folder as this script: wave_kinetic.tif, resample_sst.tif, resample_phyto.tif, resample_detritus.tif, resample_par.tif, resample_bathy.tif, zostera.marina.occurrence.csv, future_sst.tif, future_phyto1.tif, future_PAR.tif, future_detritus.tif

#Make sure that you change the directory on lines 24 and 44 

#clear workspace 
rm(list = ls())

#Set random seed for reproducibility 
set.seed(5)

#Set sample size for training data 
n = 1000

#set proportion of data to be split into test and training sets. This is proportion in test set 
p = .2 

#load packages 
library(raster)
library(dismo)
library(gam)
library(dplyr)
library(rJava)
library(randomForest)
library(readr)
library(rfUtilities)
library(caret)
library(e1071)
library(pROC)
library(SDMtune)
library(tiff)
library(ggplot2)

#Set working directory
setwd("C:/Users/user/Documents/Fiverr projects/Seagrass Locations/MarineData")

#load occurance data, remove rows with NA 
zostera= list.files(pattern = '*.csv')
tbl = sapply(zostera, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")
tbl = na.omit(tbl)
rm(zostera)

#As there are only 3 occurances of another species, and marina is written in a couple different ways, we just call the whole column 'marina' 
tbl$SPECIES = 'Zostera marina'

#After line 55, the coordinates for lat and long get switched, so this code switches those two columns after 55
tbl2 = tbl
tbl2[56:nrow(tbl2), 5] = tbl[56:nrow(tbl), 4]
tbl2[56:nrow(tbl2), 4] = tbl[56:nrow(tbl), 5]
tbl = tbl2
rm(tbl2)

#loading area of interest for random sampling. This is here to help with sampling random points later
b_rast= raster("C:/Users/user/Documents/Fiverr projects/Seagrass Locations/MarineData/Area of interest.tif")

#Get the extent of the area of interest
ext= extent(b_rast) 
ext
res(b_rast)
plot(b_rast) #beautiful plot! raster data is so cool 

#we have occurances beyond the limits of the area of interest, so we reset the extent so that it encompasses all occurances 
ext@xmin = min(tbl[4])
ext@xmax = max(tbl[4])
ext@ymin = min(tbl[5])
ext@ymax = max(tbl[5])
b_rast = setExtent(b_rast, ext, keepres=FALSE, snap=FALSE)

#loading data we use to scale other variabl
sst= raster("C:/Users/user/Documents/Fiverr projects/Seagrass Locations/MarineData/resample_sst.tif")

#Get the extent of the area of interest
ext= extent(sst) 
ext
res(sst) #dim(sst), plot(sst)

#we have occurances beyond the limits of the area of interest, so we reset the extent so that it encompasses all occurances 
ext@xmin = min(tbl[4])
ext@xmax = max(tbl[4])
ext@ymin = min(tbl[5])
ext@ymax = max(tbl[5])
sst = setExtent(sst, ext, keepres=FALSE, snap=FALSE)

#In order to ensure that all raster layers have the same extent, we crop them to the new extent and resample them to make sure they have the same resolution as well. As I don't have the original data (except for wave_kinetic) I have to resample from the resampled data   
wave_kinetic = crop(raster("wave_kinetic.tif"), ext)
wave_kinetic = resample(wave_kinetic, sst$resample_sst,method='bilinear')

phyto = crop(raster("resample_phyto.tif"), ext)
phyto = resample(phyto, sst$resample_sst,method='bilinear')

file.nc1 = "detritus.nc"
file.tiff1 = 'detrius.tif'
detritusFile = raster(file.nc1)
writeRaster(detritusFile, filename = file.tiff1, format = 'GTiff', overwrite = T)

drititus = crop(raster("detrius.tif"), ext)
drititus = resample(drititus,sst$resample_sst,method='bilinear')

par = crop(raster("resample_par.tif"), ext)
par = resample(par,sst$resample_sst,method='bilinear')

bathy = crop(raster("resample_bathy.tif"), ext)
bathy = resample(bathy, sst$resample_sst,method='bilinear')

sstData = crop(raster("resample_sst.tif"), ext)
sstData = resample(sstData, sst$resample_sst,method='bilinear')

#We stack only the marine variables, ignoring global climate variables 

#variables = stack(wave_kinetic, sst, phyto, drititus, par, bathy)
variables = stack(wave_kinetic, sstData, phyto, bathy, drititus) 

# drititus and par end up with a ton of NAs later on in the code, so for now I'm removing them from the predictor variable set. We can keep them though by commenting out this line of code and uncommenting the line above. Also need changes on other lines as well 

#Get the coordinates of where species are located 
species = tbl[,c(4,5)]
names(species) = c("lat", "lon")
m = data.matrix(species, rownames.force = NA)

#Create training and test sets of data 

set.seed(5)
group_pres = kfold(m, 5) #create 5 groups, 1 of which will be the test set. This code ensures that final test set won't have NAs and will have equal numbers of occurances of presence and absense 
pres_train = m[group_pres != 2, ] 
pres_test = m[group_pres == 2, ]
pres_test2 = extract(variables, pres_test)
naRows = rowSums(is.na(pres_test2)) > 0
pres_test = pres_test[!naRows,]
pres_test = pres_test[sample(1:nrow(pres_test), p*n, replace = TRUE), ]

#Get coordinates where species aren't present. Our binomial response variable, then, is 0 = no species and 1 = species. In order to ensure that the model isn't biased, we want just as many cases without a species as there are cases with a species, which is why we sample n times here. 
backg = randomPoints(b_rast, n=10000, ext=ext, extf = 1.25)
colnames(backg) = c('lat', 'lon')
group_backg = kfold(backg, 5)
backg_train = backg[group_backg != 2, ]
backg_train = backg_train[sample(1:nrow(backg_train), n-(p*n), replace = TRUE), ]

backg_test = backg[group_backg == 2, ]
backg_test2 = extract(variables, backg_test)
naRows = rowSums(is.na(backg_test2)) > 0
backg_test = backg_test[!naRows,]
backg_test = backg_test[sample(1:nrow(backg_test), p*n, replace = TRUE), ]

#Combine datasets into the one large training set 
train = rbind(pres_train, backg_train)

#name the binomial variable as either 0 or 1 
pb_train = c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))

#final training dataset 
envtrain = extract(variables, train)
envtrain = data.frame(cbind(pa=pb_train, envtrain))
envtrain = na.omit(envtrain)

#Removing rows with NAs means that our sample size is lower than what we set it to. The following while loop corrects this. This is the opposite of an elegant solution (I'm sure that there are easier ways of doing this), but this worked so that's all that really matters. 
numberOfOccurances = length(envtrain$pa[envtrain$pa==1])
numberOfNoOccurances = length(envtrain$pa[envtrain$pa==0])
counter = 0
while(numberOfNoOccurances < n){
  
  pres_train = m[group_pres != 2, ] 
  pres_train = pres_train[sample(1:nrow(pres_train), n-(p*n), replace = TRUE), ] 
  backg_train = backg[group_backg != 2, ]
  backg_train = backg_train[sample(1:nrow(backg_train), n-(p*n), replace = TRUE), ]
  train = rbind(pres_train, backg_train)
  pb_train = c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
  envtrain2 = extract(variables, train)
  envtrain2 = data.frame(cbind(pa=pb_train, envtrain2))
  envtrain2 = na.omit(envtrain2)
  envtrain = rbind(envtrain, envtrain2)
  numberOfOccurances = length(envtrain$pa[envtrain$pa==1])
  numberOfNoOccurances = length(envtrain$pa[envtrain$pa==0])
  counter = counter + 1
  if (counter > 25) {
    break
    stop("Too many NAs, use different datasets")
  }
  
}

#I'm clearing out some unnecessary variables here to keep the environment a bit less cluttered 
rm(envtrain2, counter, train, pb_train, backg)

envtrainOccurances = subset(envtrain, pa == 1)
envtrainNoOccurances = subset(envtrain, pa == 0)
envtrainOccurances =  envtrainOccurances[sample(1:nrow(envtrainOccurances), n-(p*n), replace = TRUE), ]
envtrainNoOccurances =  envtrainNoOccurances[sample(1:nrow(envtrainNoOccurances), n-(p*n), replace = TRUE), ]

#final dataset has equal numbers of rows with and without occurances 
envtrain = rbind(envtrainOccurances, envtrainNoOccurances)

### --- Random Forest --- ###


#create the random forest. pa is our response variable, and everything else is a predictor. ntree gives the number of decision trees in the forst, it essentially sets the number of voters in this democracy as every tree predicts whether a point will be 0 or 1 given the data and the final answer is the majority vote 

set.seed(5)
rf.mdl = randomForest(as.factor(pa) ~ ., data = envtrain, ntree=501) 
rf.cv = rf.crossValidation(rf.mdl, envtrain[,2:5], p=0.10, n=99, ntree=501)

# Plot cross validation versus model producers accuracy, producers accuracy = [Number of correct / total number of correct and omission errors]. omission errors = observations being erroneously excluded from a given class. We had 99 iterations of cross validation so parameters were getting tuned. Successive iterations are plotted on x-axis 
par(mfrow=c(1,2)) 
plot(rf.cv, type = "cv", main = "CV producers accuracy")
plot(rf.cv, type = "model", main = "Model producers accuracy")

# Plot cross validation versus model oob. oob = out of bag error. for cv we use bagging. Bagging uses subsampling with replacement to create training samples for the model to learn from. OOB error is the mean prediction error on each training sample xi, using only the trees that did not have xi in their bootstrap sample
par(mfrow=c(1,2)) 
plot(rf.cv, type = "cv", stat = "oob", main = "CV oob error")
plot(rf.cv, type = "model", stat = "oob", main = "Model oob error")	

# Now we will evaluate the accuracy on the test dataset 
rf.mdl = randomForest(pa ~ ., data = envtrain, ntree=501) 
e = evaluate(pres_test, backg_test, rf.mdl, variables)
e #I couldn't find literature to confirm this, but I believe that 'cor' is the correlation (Pearson's?) between the predicted value and the actual value 
plot(e,'ROC')

thresh = threshold(e)[2]
truePosplustrueNeg = which.max(e@confusion[,1]/(n*p) + e@confusion[,4]/(n*p)) 
confusionMatrix = e@confusion[truePosplustrueNeg,]
accuracyRF = (confusionMatrix[1]+confusionMatrix[4])/(2*n*p)

# Visualization of prediction for random forest 
pbrf = predict(variables, rf.mdl, ext=ext, progress='')
plot(pbrf, main='RF, raw values', xlim = c(-6, -4.5), ylim = c(49.9, 50.6))

rfPrediction = c(as.numeric(e@presence > as.numeric(thresh)), as.numeric(e@absence > as.numeric(thresh)))

actual = c(rep(1, 200), rep(0, 200))

BrierScore(rfPrediction, actual)

### --- MAXENT --- ###


maxentModel = maxent(envtrain[, 2:ncol(envtrain)], envtrain[, 1])
e = evaluate(pres_test, backg_test, maxentModel, variables)
e
#thresh = e[6]
plot(e,'ROC')
plot(e, 'TPR')
boxplot(e)
#The value that the model outputs is the probability that a species will occur at a particular location given the marine data. The 'threshold' decides at what probability the model says a species is actually at that spot (as we are treating this as a classifciation problem, they are either at a spot or they aren't). If the threshold is at .2 for instance, then anything above .2 is considered 'present'. We evaluate things like true positive rate at many thresholds (each dot gives a different threshold)

# Visualization of prediction for maxent 
pbmax = predict(variables, maxentModel, ext=ext, progress='')
plot(pbmax, main='MAXENT, raw values', xlim = c(-6, -4.5), ylim = c(49.9, 50.6))

thresh = threshold(e)[2]
truePosplustrueNeg = which.max(e@confusion[,1]/(n*p) + e@confusion[,4]/(n*p)) 
confusionMatrix = e@confusion[truePosplustrueNeg,]
accuracyMaxEnt = (confusionMatrix[1]+confusionMatrix[4])/(2*n*p)

maxEntPrediction = c(as.numeric(e@presence > as.numeric(thresh)), as.numeric(e@absence > as.numeric(thresh)))

BrierScore(maxEntPrediction, actual)

#Using this package, I couldn't find a way of doing cross validation, or getting the confusion matrix. If you need more than just the AUC and correlation between prediction and actual (cor from e), I would expand on this code. I would also see if you can find AIC/BIC for each model and compare those as well 


### --- GAM --- ### 


#If you need par and detritus, then use this gam 
#gm1 = gam(pa ~ wave_kinetic+resample_bathy+resample_par+resample_sst+resample_phyto+resample_detritus, family = binomial(link = "logit"), data=envtrain)

gm1 = gam(pa ~ wave_kinetic+resample_bathy+resample_sst+resample_phyto, family = binomial(link = "logit"), data=envtrain)
#See what predictor variables are significant 
summary(gm1)

#Evaluate the model 
testpres = data.frame(extract(variables, pres_test))
testbackg = data.frame(extract(variables, backg_test))
e = evaluate(testpres, testbackg, gm1)
e
plot(e,'ROC')
plot(e, 'TPR')
boxplot(e)

# Visualization of prediction for gam 
options(warn=-1) 
pg = predict(variables, gm1, ext=ext)
par(mfrow=c(1,2))

plot(pg, main='GAM modeling,predicted', xlim = c(-6, -4.5), ylim = c(49.9, 50.6))
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)

thresh = threshold(e)[2]
truePosplustrueNeg = which.max(e@confusion[,1]/(n*p) + e@confusion[,4]/(n*p)) 
confusionMatrix = e@confusion[truePosplustrueNeg,]
accuracyGAM = (confusionMatrix[1]+confusionMatrix[4])/(2*n*p)

gamPrediction = c(as.numeric(e@presence > as.numeric(thresh)), as.numeric(e@absence > as.numeric(thresh)))

actual = c(rep(1, 200), rep(0, 200))

BrierScore(gamPrediction, actual)

### --- Plots --- ###

library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(ggpubr)

b_rast_graph_spdf <- as(b_rast, "SpatialPixelsDataFrame")
b_rast_graph_df <- as.data.frame(b_rast_graph_spdf)
colnames(b_rast_graph_df) <- c("value", "x", "y")

ggplot() + geom_tile(data=b_rast_graph_df, aes(x=x, y=y, fill=value))  +theme_map() + labs(fill = "Elevation (m)") + theme(text = element_text(size = 14)) + scale_fill_viridis() + coord_equal() + theme(legend.position="bottom") + theme(legend.key.width=unit(2, "cm"))

b_rast_graph_spdf <- as(pg, "SpatialPixelsDataFrame")
b_rast_graph_df <- as.data.frame(b_rast_graph_spdf)
colnames(b_rast_graph_df) <- c("value", "x", "y")

p1 = ggplot() + geom_tile(data=b_rast_graph_df, aes(x=x, y=y, fill=value))  +theme_map() + labs(fill = "Prediction") + theme(text = element_text(size = 14)) + scale_fill_viridis(option = "magma") + coord_equal() + theme(legend.position="bottom") + theme(legend.key.width=unit(2, "cm")) + ggtitle("GAM") +
  theme(plot.title = element_text(hjust = 0.5))

b_rast_graph_spdf <- as(pbmax, "SpatialPixelsDataFrame")
b_rast_graph_df <- as.data.frame(b_rast_graph_spdf)
colnames(b_rast_graph_df) <- c("value", "x", "y")

p2 = ggplot() + geom_tile(data=b_rast_graph_df, aes(x=x, y=y, fill=value))  +theme_map() + labs(fill = "Prediction") + theme(text = element_text(size = 14)) + scale_fill_viridis(option = "magma") + coord_equal() + theme(legend.position="bottom") + theme(legend.key.width=unit(2, "cm")) + ggtitle("Max Entropy") +
  theme(plot.title = element_text(hjust = 0.5))

b_rast_graph_spdf <- as(pbrf, "SpatialPixelsDataFrame")
b_rast_graph_df <- as.data.frame(b_rast_graph_spdf)
colnames(b_rast_graph_df) <- c("value", "x", "y")

p3 = ggplot() + geom_tile(data=b_rast_graph_df, aes(x=x, y=y, fill=value))  +theme_map() + labs(fill = "Prediction") + theme(text = element_text(size = 14)) + scale_fill_viridis(option = "magma") + coord_equal() + theme(legend.position="bottom") + theme(legend.key.width=unit(2, "cm")) + ggtitle("Random Forest") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p3, p2, p1, nrow=1, ncol=3, common.legend = TRUE)

par(mfrow=c(1,3))
plot(pg, main='GAM modeling,predicted', xlim = c(-6, -4.5), ylim = c(49.9, 50.6))
plot(pbmax, main='MAXENT, raw values', xlim = c(-6, -4.5), ylim = c(49.9, 50.6))
plot(pbrf, main='RF, raw values', xlim = c(-6, -4.5), ylim = c(49.9, 50.6))

### --- PREDICTIONS --- ###

#Remake data with future values. I'm making these future datasets the same way I made previous datasets 
#I don't have future kinetic and bathy, so I'm using old files for predictions

sstData = crop(raster("future_sst.tif"), ext)
sstData = resample(sstData, sst$resample_sst,method='bilinear')

phyto = crop(raster("future_phyto1.tif"), ext)
phyto = resample(phyto, sst$resample_sst,method='bilinear')

drititus = crop(raster("future_detritus.tif"), ext)
drititus = resample(drititus, sst$resample_sst,method='bilinear')

#par = crop(raster("future_PAR.tif"), ext)
#par = resample(par, sst$resample_sst,method='bilinear')

#variables = stack(wave_kinetic, sst, phyto, drititus, par, bathy)
variables = stack(wave_kinetic, sstData, phyto, bathy, drititus)

#rename each stack so the model will recognize variable names 
names(variables) = c("wave_kinetic", "resample_sst", "resample_phyto", "resample_bathy", "detrius")

#Predictions for each model 

random_forest_prediction = predict(variables, rf.mdl, ext=ext, progress='')
writeRaster(random_forest_prediction,filename ='zostera_predicted_rf.tif',format="GTiff", overwrite=TRUE)
random_forest_prediction = readTIFF('zostera_predicted_rf.tif') 

maxent_prediction = predict(variables, maxentModel, ext=ext, progress='')
writeRaster(maxent_prediction,filename ='zostera_predicted_maxent.tif',format="GTiff", overwrite=TRUE)
maxent_prediction = readTIFF('zostera_predicted_maxent.tif') 

gam_prediction = predict(variables, gm1, ext=ext)
writeRaster(gam_prediction,filename ='zostera_predicted_gam.tif',format="GTiff", overwrite=TRUE)
gam_prediction = readTIFF('zostera_predicted_gam.tif') 



### --- Are model predictions different from one another? --- ###

#For the test dataset, I count the number of times each model predicted either a positive or negative outcome. 
rfPos = sum(rfPrediction)
mePos = sum(maxEntPrediction)
gamPos = sum(gamPrediction)

rfNeg = sum(!rfPrediction)
meNeg = sum(!maxEntPrediction)
gamNeg = sum(!gamPrediction)

tab <- matrix(c(rfPos, mePos, gamPos, rfNeg, meNeg, gamNeg), ncol=3, byrow=TRUE)
colnames(tab) <- c('Random Forest','MaxEnt','GAM')
rownames(tab) <- c('Presence','Absence')
tab <- as.table(tab)

#I then perform a chi-squared test on this count data 
chisq.test(tab) 

#the maxent model is clearly different from the other two, which is why we get a significant value. However, its less clear whether the gam output is different from the random forest output, so we do a 2x2 version of a chi-squared test (fisher exact test) to compare these two models 
tab2 =  matrix(c(rfPos, gamPos, rfNeg, gamNeg), ncol=2, byrow=TRUE)
colnames(tab2) <- c('Random Forest','GAM')
rownames(tab2) <- c('Presence','Absence')
tab2 <- as.table(tab2)
fisher.test(tab2)

#This is insigificant. This, coupled with the similar accuracies of the models, tells me that the gam and random forest models are almost equivalent. I'm not sure which is better for your purposes then, maybe the one that takes less processing time? 