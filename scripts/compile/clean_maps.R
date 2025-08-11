# libs ####
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
library(raster)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# clean base and 2010 crosswalk files (available at: https://dsl.richmond.edu/panorama/redlining/data) ####

# clear environment
rm(list = ls())
gc()

# define keep variables for crosswalk file
vars <- c(
  'state',
  'city',
  'label',
  'grade', 
  'GEOID10', 
  'calc_area', 
  'pct_tract', 
  'geom')

# load crosswalk -> filter & convert to data frame
cwalk <- st_read("D:/research/redenv/data/holc/cwalk.gpkg") %>% 
  dplyr::select(all_of(vars)) %>% 
  st_drop_geometry() %>%
  rename('TRACTID' = 'GEOID10') %>%
  dplyr::filter(grade %in% c('A','B','C','D')) %>% na.omit() %>%
  mutate(holc_area = pct_tract * calc_area)
cwalk <- cwalk[!(trimws(cwalk$label) == ""), ]
  
# create map id variable 'HOLCID' = label_state_city_grade
cwalk$HOLCID <- paste(
  cwalk$label,
  cwalk$state, 
  cwalk$city, 
  cwalk$grade, sep = "_")

# calculate total area and weights using HOLCID
total_area <- cwalk %>%
  group_by(HOLCID) %>%
  summarize(total_area = sum(holc_area, na.rm = T))

cwalk <- left_join(cwalk, total_area, by = 'HOLCID') %>%
  mutate(weight = holc_area / total_area) %>%
  dplyr::select(-state, -label, -calc_area, -pct_tract, -holc_area, -total_area)

# get ID counts for crosswalk
length(unique(cwalk$HOLCID))
length(unique(cwalk$TRACTID))

# define keep variables for base map file
vars <- c(
  'state',
  'city',
  'label',
  'grade', 
  'geom')

# load -> filter base map file
base_maps <- st_read("D:/research/redenv/data/holc/base_maps.gpkg") %>% 
  dplyr::select(all_of(vars)) %>% 
  dplyr::filter(grade %in% c('A','B','C','D')) %>% na.omit() 
base_maps <- base_maps[!(trimws(base_maps$label) == ""), ]

# create map id variable for base map
base_maps$HOLCID <- paste(
  base_maps$label,
  base_maps$state, 
  base_maps$city, 
  base_maps$grade, sep = "_")

# match observation sets between files
base_maps <- base_maps %>% dplyr::filter(HOLCID %in% cwalk$HOLCID)
cwalk <- cwalk %>% dplyr::filter(HOLCID %in% base_maps$HOLCID)
length(unique(base_maps$HOLCID))
length(unique(cwalk$HOLCID))

# save
fwrite(cwalk, 'D:/research/redenv/data/output/cw.csv')
st_write(base_maps, 'D:/research/redenv/data/output/base_maps.gpkg')



