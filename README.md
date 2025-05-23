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
1. clean_redmaps.R
2. comp_noise.R
3. comp_air.R
4. redxp.R
5. plots.R
6. stats.R

### data

All data used in this project is publicly available. These are links to download the required files:

- [Redlining Maps](https://dsl.richmond.edu/panorama/redlining/data) (need original maps for noise data & 2010 crosswalk file for air data)
- [Noise Emissions Estimates for 2018 & 2020](https://www.bts.gov/geospatial/national-transportation-noise-map)
- [Air Emissions Estimates for 2017 & 2019](http://air.csiss.gmu.edu/aq/NEMO/)


## USAGE

Start by installing and loading all required packages. Run compiling scripts in the order listed and modify directory paths as needed.

Step-by-step guide for compiling:
1.  run clean_redmaps.R (need 2010crosswalk.gpkg and og_maps.gpkg)
2.  run comp_noise.R and comp_air.R to organize emission sets
3.  run redxp.R to synthesize noisedisp.csv with airdisp.csv
4.  run analysis scripts plots.R and stats.R to explore the compiled dataset 'redxp.csv'


Step-by-step guide for analysis:
1. 















