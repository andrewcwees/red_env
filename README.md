### project: redenv
Use R to evaluate how historically 'redlined' neighborhoods across >200 cities in the U.S. are associated with higher average levels of exposure to noise and air pollution across transportation sectors (railroads, roadways, and aviation) in the present day.

### SETUP

## required packages
library(sf)
library(httr)
library(skimr)
library(ggpubr)
library(data.table)
library(jsonlite)
library(mapview)
library(terra)
library(exactextractr)
library(stars)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(tigris)

## scripts
# compiling
1. clean_redmaps.R
2. comp_noise.R
3. comp_air.R
4. redxp.R
# analysis
- plots.R
- stats.R

## data
- original HOLC redlining maps
- HOLC redlining maps 2010 crosswalk file
- noise pollution estimates
- air pollution estimates


### USAGE

Start by installing and loading all required packages. Run compiling scripts in the order listed and modify directory paths as needed.














