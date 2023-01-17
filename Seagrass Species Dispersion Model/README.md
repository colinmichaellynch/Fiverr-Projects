# Seagrass Species Dispersion Model

Developing models to predict the location of seagrass off the coast of England. 

## Table of Contents

* [Data](https://github.com/colinmichaellynch/Fiverr-Projects/blob/main/Where%20to%20Buy%20Rental%20Properties/CountyInfo.csv)

* [Script](https://github.com/colinmichaellynch/Fiverr-Projects/blob/main/Where%20to%20Buy%20Rental%20Properties/paretoOptimization.R)

## Background

Climate change can have complicated and sometimes unexpected effects on the distribution and abundance of different species. To understand how an ecosystem is going to respond to these changes, one must understand how the keystone species of that ecosystem is going to respond first, as all other species depend strongly on these keystones. An important producer for the coastal ecosystem of England is zorestra, commonly known as eelgrass. To develop models for how the distribution of eelgrass will change over time, one must first develop a model of how ecological factors presently affect eelgrass. If these models have high predictive power, then climate models can be used to change the environmental factors plugged into the species dispersion model to generate predictions for the future state of eelgrass. Here, we compare the capabilities of 3 different dispersal models to maximize common machine learning metrics such as accuracy and ROC AUC: a random forest model, a maximum entropy model, and a generalized additive model (GAM). 

## Methods 

* Load in environmental data as a raster file
  - Rasters are data structures which represent the surface of the planet in a grid.
  - The grids - or matrices - for different environmental features (water depth, kinetic energy of waves, average temperature, etc.) are layered on top of one another
  - Eelgrass can either be present inside a particular grid square or it is not. 

* Here is an example layer of the raster which shows elevation: 

<p align="center">
  <img src = https://user-images.githubusercontent.com/61156429/212992686-b55ac949-dd8c-4a59-966c-3ea1595f6ea5.png>
</p>


* To create training and test sets of data, we randomly select grid squares which have eelgrass (so the response variable is 1) and other grid squares which do not have eelgrass (response variable is 0). 
  - As this dataset is imbalanced (there are many more grid squares which do not have eelgrass), we sample an equal number of grid squares with and without eelgrass. 
  - We remove rows of data that do not have entries (shown in white in previous plot)

* We fit each model to the training set and then test predictions on the test set
  - We measure test accuracy, ROC AUC, and the true positive rate 

## Results 

* The following graph shows the predicition probabilities for each of the models: 

![predictionPlots](https://user-images.githubusercontent.com/61156429/212992694-f44daf54-e93b-4fed-b691-a29dc1b0ea2a.png)

## Acknowledgements

I would like to thank Ahmed Koptan and Carlos Orellana for their help in implementing this algorithm in Python. I would also like to thank [Cole Busby](https://github.com/ColeBusbyMedTech) for testing this algorithm on different datasets. 
