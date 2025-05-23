# project: redenv
Using R and public data, evaluate how historically 'redlined' neighborhoods across >200 cities in the U.S. are associated with higher average levels of exposure to noise and air pollution across transportation sectors (railroads, roadways, and aviation) in the present day.

## SETUP

### required packages
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

### scripts
1. clean_maps.R
2. comp_noise.R
3. comp_air.R
4. redxp.R
5. plots.R
6. stats.R

### data

All data used in this project is publicly available (download links stored in scripts)

## USAGE

Start by installing and loading all required packages. Run compiling scripts in the order listed and modify directory paths as needed.

Step-by-step guide for compiling/analysis:
1.  run clean_maps.R (need original & 2010 crosswalk files from Mapping Inequality Project)
2.  run comp_noise.R
3.  run comp_air.R
4.  run redxp.R to synthesize noisedisp.csv with airdisp.csv
5.  run analysis scripts plots.R and stats.R to explore the compiled dataset 'redxp.csv'
















