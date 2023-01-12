# Where to Buy Rental Properties
Found US counties ideal for buying rental properties for Fiverr client  

## Table of Contents

* [Data]()

* [Script]()

## Background

My Fiverr client wanted to buy 100 homes in a single US county for the purpose of renting them out. He therefore wanted me to identify US counties where there would be a high demand for rent and where he could quickly make his money back from his realestate investment. Large cities tend to have more rental properties than rural towns, so I used county population as a proxy for demand. He also wanted the rental rate to be high relative to the price of the home, so I also considered the fair market rent (FMR) / average home price ratio as a metric for potential profit. This problem can be construed as a two-objective optimization problem, where we want to find counties that maximize both demand as well as the FMR/price ratio. 

## Methods 

* I first created a database by scraping realestate data from realor.com and population data from census.gov. 

* I then identified counties where the population is greater than 20,000 people that also have a ratio greater than 0.01 (green points). 

* Next I found those points that are Pareto optimal. These are counties where improvements in one criterion (population, ratio) do not lead to losses in the other criterion. The set of optimal solutions is the Pareto front, which is given by the red line. 

* Finally, my client had success in buying property in Cuyahoga County (orange diamond), so he wanted me to identify counties that were similar to it. To find these counties, I found those counties that had the smallest euclidean distance from Cuyahoga County in the following plot (orange points). As the 2 axes have different units, I rescaled them so that they were weighed equally. 

<p align="center">
  <img src=https://user-images.githubusercontent.com/61156429/212129404-4fb64ff6-58b3-4d64-9e9f-202628315aac.png>
</p>

## Results

* There are 14 candidate counties for buying property. 
   - There are 4 counties that are both Pareto Optimal and meet the client's thresholds. 
   - There are an additional 10 counties in the same neighborhood as Cuyahoga County. 
   - The first 4 likely have the greatest potential for profit. The latter 10 may be the safer bet. 
