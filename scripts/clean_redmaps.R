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

# clean 2010 crosswalk file ####

# clear environment
rm(list = ls())
gc()

# load & filter crosswalk file
vars <- c(
  'state',
  'city',
  'label',
  'grade', 
  'GEOID10', 
  'calc_area', 
  'pct_tract', 
  'geom')
cw <- st_read("D:/research/redenv/data/holc/2010crosswalk.gpkg") %>% 
  dplyr::select(all_of(vars)) %>% 
  st_drop_geometry() %>%
  rename('TRACTID' = 'GEOID10') %>%
  dplyr::filter(grade %in% c('A','B','C','D')) %>% na.omit() %>%
  mutate(holc_area = pct_tract * calc_area)
cw <- cw[!(trimws(cw$label) == ""), ]
  
# create map id variable 'HOLCID' = label_state_city_grade
cw$HOLCID <- paste(
  cw$label,
  cw$state, 
  cw$city, 
  cw$grade, sep = "_")

# calc total area and weights using HOLCID
total_area <- cw %>%
  group_by(HOLCID) %>%
  summarize(total_area = sum(holc_area, na.rm = T))

cw <- left_join(cw, total_area, by = 'HOLCID') %>%
  mutate(weight = holc_area / total_area) %>%
  dplyr::select(-state, -label, -calc_area, -pct_tract, -holc_area, -total_area)

# check obervation count for HOLC neighborhoods & census tracts
length(unique(cw$HOLCID))
length(unique(cw$TRACTID))


# clean map file ####

# clear environment
rm(list = ls())
gc()

# load & filter map file
vars <- c(
  'state',
  'city',
  'label',
  'grade', 
  'geom')
og_maps <- st_read("D:/research/redenv/data/holc/original_maps.gpkg") %>% 
  dplyr::select(all_of(vars)) %>% 
  dplyr::filter(grade %in% c('A','B','C','D')) %>% na.omit() 
og_maps <- og_maps[!(trimws(og_maps$label) == ""), ]

# create map id variable 'HOLCID' = label_state_city_grade
og_maps$HOLCID <- paste(
  og_maps$label,
  og_maps$state, 
  og_maps$city, 
  og_maps$grade, sep = "_")

# match observation sets between crosswalk and map files --> save
og_maps_filt <- og_maps %>% dplyr::filter(HOLCID %in% cw$HOLCID)
cw_filt <- cw %>% dplyr::filter(HOLCID %in% og_maps_filt$HOLCID)
length(unique(og_maps_filt$HOLCID))
length(unique(cw_filt$HOLCID))

fwrite(cw_filt, 'D:/research/redenv/data/output/cw.csv')
st_write(og_maps_filt, 'D:/research/redenv/data/output/og_maps.gpkg')



