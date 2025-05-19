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

#####

rm(list = ls())
gc()

holc <- st_read('D:/research/redenv/data/output/og_maps.gpkg')

state <- unique(holc$state)
sector <- c("rail", "road", "aviation", "rail_road_and_aviation")
year <- c("2018", "2020")

noiseR <- function(sector, year) {
  
  empty3 <- list()
  empty2 <- list()
  empty1 <- list()
  
  for (k in 1:length(sector)) {
    
    for (j in 1:length(year)) {
      pattern <- paste0("*_", sector[k], "_noise_", year[j], ".tif$")
      path <- paste0("D:/research/redenv/data/ntnm/CONUS_", sector[k], "_noise_", year[j], "/State_rasters/")
      r <- list.files(path = path, pattern = pattern, full.names = T)
      r <- lapply(r, rast)
      
      rr <- sprc(r)
      
      for (i in 1:length(state)) {
        
        print(i)
        print(pattern)
        print(path)
        st <- state[i]
        test  <- holc %>% filter(state==st) 
        medi  <- exact_extract(rr[i], test, 'median')
        count <- exact_extract(rr[i], test, 'count')
        tot   <- exact_extract(rr[i], test, function(values, coverage_fraction) sum(coverage_fraction))
        df    <- cbind(test, tot, count, medi) %>% 
          data.table() %>% mutate_if(is.numeric, ~replace_na(., 0)) %>% mutate(excess=medi*count/tot)
        df$state <- st
        empty1[[i]] <- df
        
      }
      noise <- plyr::ldply(empty1, data.frame)
      noise$year <- year[j]
      empty2[[j]] <- noise
      
    }
    noise2 <- plyr::ldply(empty2, data.frame)
    noise2$sector <- sector[k]
    empty3[[k]] <- noise2
  }
  
  noise.df <- plyr::ldply(empty3, data.frame)
  return(noise.df)
}

noise <- noiseR(sector, year) %>% dplyr::select(-geom) %>%
  mutate(sector = case_when(
    sector == "aviation" ~ "Aviation",
    sector == "rail" ~ "Rail",
    sector == "road" ~ "Road",
    sector == "rail_road_and_aviation" ~ "Total")) %>% 
  mutate_all(~ ifelse(is.nan(.), 0, .)) %>% # convert NaN values to 0
  mutate_if(is.numeric, ~ round(., 3)) %>%
  dplyr::select(-state, -label, -tot, -count, -medi)

percentage <- mean(noise$excess > 0) * 100
print(percentage)

length(unique(noise$HOLCID))

fwrite(noise, 'D:/research/redenv/data/output/noisedisp.csv')




