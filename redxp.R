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

# join noise & air
noisedisp <- fread('D:/research/redenv/data/output/noisedisp.csv')
noisedisp$pollutant <- 'NOISE'
noisedisp$xp <- noisedisp$excess
noisedisp$excess <- NULL

airdisp <- fread('D:/research/redenv/data/output/airdisp.csv')
airdisp$xp <- airdisp$w_xp
airdisp$w_xp <- NULL

data <- rbind(noisedisp, airdisp) %>%
  mutate(logxp = log1p(xp)) %>%
  rename(period = year) %>%
  mutate(period = case_when(
    period %in% c(2017, 2018) ~ 1,
    period %in% c(2019, 2020) ~ 2,
    TRUE ~ as.integer(period)))

# count variables
length(unique(data$sector))
length(unique(data$period))
length(unique(data$pollutant))
length(unique(data$grade))
length(unique(data$city))
length(unique(data$HOLCID))

# eval 0 count
percent_zero_overall <- mean(data$xp == 0) * 100
print(percent_zero_overall)

noise <- data %>% dplyr::filter(pollutant == 'NOISE')
percent_zero_noise <- mean(noise$xp == 0) * 100
print(percent_zero_noise)

# add z scores
data_zscores <- data %>% 
  group_by(sector, pollutant) %>% 
  mutate(
    z_xp = (xp - mean(xp, na.rm = T)) / sd(xp, na.rm = T),
    z_logxp = (logxp - mean(logxp, na.rm = T)) / sd(logxp, na.rm = T)) %>% 
  ungroup()

# add CEI
cei <- data_zscores %>%
  pivot_wider(
    id_cols = HOLCID,
    names_from = c(sector, pollutant),
    values_from = z_logxp,
    values_fn = list(z_logxp = mean),  
    names_sep = "_" ) %>% mutate(
    cei_Rail = rowSums(across(starts_with("Rail")), na.rm = T),
    cei_Road = rowSums(across(starts_with("Road")), na.rm = T),
    cei_Aviation = rowSums(across(starts_with("Aviation")), na.rm = T),
    cei_Total = rowSums(across(starts_with("Total")), na.rm = T)) %>%
  dplyr::select(HOLCID, all_of(starts_with('cei_'))) %>%
  pivot_longer(
    values_to = 'cei',
    names_to = 'sector',
    cols = starts_with('cei_')) %>%
  mutate(sector = str_remove(sector, "^cei_")) 

cei <- data_zscores %>%
  pivot_wider(
    id_cols = HOLCID,
    names_from = c(sector, pollutant),
    values_from = z_xp,
    values_fn = list(z_xp = mean),  
    names_sep = "_" ) %>% mutate(
      cei_Rail = rowSums(across(starts_with("Rail")), na.rm = T),
      cei_Road = rowSums(across(starts_with("Road")), na.rm = T),
      cei_Aviation = rowSums(across(starts_with("Aviation")), na.rm = T),
      cei_Total = rowSums(across(starts_with("Total")), na.rm = T)) %>%
  dplyr::select(HOLCID, all_of(starts_with('cei_'))) %>%
  pivot_longer(
    values_to = 'cei',
    names_to = 'sector',
    cols = starts_with('cei_')) %>%
  mutate(sector = str_remove(sector, "^cei_")) 

# save
save <- left_join(data_zscores, cei, by = c('HOLCID', 'sector'))
save$pollutant <- factor(save$pollutant, levels = c("NO", "NO2", "PM25", "NOISE"))
fwrite(save, 'D:/research/redenv/data/output/redxp.csv')


# outlier filter ####
rm(list = ls())
gc()

data <- fread('D:/research/redenv/data/output/redxp.csv')

print(mean(data$z_xp > 3) * 100)
print(mean(data$z_xp < -3) * 100)

outlier_data <- data %>% filter(z_xp > 3)

outlier_summary <- outlier_data %>%
  group_by(grade, sector, pollutant) %>%
  summarise(Count = n(), Percentage = (n() / nrow(outlier_data)) * 100, .groups = 'drop') %>%
  arrange(desc(Count))
print(outlier_summary)

filtered_data <- data %>%
  filter(!HOLCID %in% outlier_data$HOLCID)

fwrite(filtered_data, 'D:/research/redenv/data/output/redxp_filt.csv')



