# Seagrass Species Dispersion Model

Developing models to predict the location of seagrass off the coast of England. 

## Table of Contents

* Data could not be included due to size constraints within Github. 

* [Script](https://github.com/colinmichaellynch/Fiverr-Projects/blob/main/Seagrass%20Species%20Dispersion%20Model/GAM_RF_MaxEnt_Models.R)

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
  - We measure test accuracy, ROC AUC, and the Brier score  

## Results 

* The following graph shows the predicition probabilities for each of the models: 

![predictionPlots](https://user-images.githubusercontent.com/61156429/212992694-f44daf54-e93b-4fed-b691-a29dc1b0ea2a.png)

* The following is a table comparing the evaluation metrics of each model 

| Model | Accuracy | ROC AUC | Brier Score | 
| --- | --- | --- | --- |
| Random Forest | 0.995 | 0.98 | 0.005 |
| Max Entropy | 0.985 | 0.971 | 0.015 |
| GAM | 0.973 | 0.946 | 0.035 |

* We can see that random forest performs the best, but the other models perform extremely well 
  - GAM predictions seem off at the southern end of the graph though. This combined with worse metrics indicates that this model should not be used. 

* This is an indication that eelgrass is extremely sensitive to environmental conditions, as these factors strongly predict where the eelgrass will be. 

* Given the high verification accuracy of these models, they can be incorporated into a larger data and model pipeline where ecologists will try to predict where these species will distribute themselves with changes in the climate. The end goal of this pipeline will be to estimate future carbon dioxide absorption rates around England, temperature in this same region, and other factors which will influence the English countryside.  

## Acknowledgements

I would like to thank the Fiverr user leafygreens27 for the project idea, for general guidance as to the purpose of these models, and for providing some reference code. 
