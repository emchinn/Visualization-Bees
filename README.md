# Visualization-Bees
This is a visualization of US agricultural dependence on honey bees and their declining health status. As part of a 2 person team, I used various R libraries (network3D, igraph, ggplot, plotly, maps*) with R Shiny to make an interactive web application to eplore some of the data.

The data were all obtained from the United States Department of Agriculture (USDA). The USDA collects data on stressors to bee colonies, changes in numbers of bee colonies, honey production, and pollination costs in order to help track risk factors and mortality for our nation’s main pollinators. Specifically, 3 csv files spanning 2015-2016 were analyzed. The files consisted of numerical, categorical, as well as temporal data. Analysis focuses attention on the current decline of honey bees as well as their importance in the production of the crops we consume daily. 

Bar plot and map detailing the decline of bees in various states.
<p align="center">
<img src="images/lost_colonies.png" width="800"/> 
</p>


Sankey plot detailing the dependence of certain crops on bee pollination across agricultural regions.
<p align="center">
<img src="images/region_map.png" width="300"/> <img src="images/sankey.png" width="400"/> 
</p>


\*The libraries used include: 
library(shiny)  
library(maps)  
library(mapproj)  
library(cluster)  
library(shiny)  
library(tidyr)  
library(scales)  
library(plyr)  
library(ggplot2)  
library(gplots)  
library(reshape)  
library(dplyr)  
library(grid)  
library(networkD3)  
library(igraph)  

To run the web app, from the R console type:

shiny::runGitHub('Visualization-Bees', 'emchinn')

