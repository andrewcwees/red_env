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

# process source files ####
# DATA FROM http://air.csiss.gmu.edu/aq/NEMO/ #### 
rm(list = ls())
gc()

input_dir <- "D:/research/redenv/data/nemo_transport/"
output_dir <- "D:/research/redenv/data/output/nemo/"
sectors <- c("airports", "onroad_ff10", "rail")

process_and_save_files <- function(folder_path, output_folder) {
  files <- list.files(
    folder_path, pattern = "^USTRACT_.*_emis_.*_\\d+\\.csv$", full.names = TRUE)
  
  for (file in files) {
    filename <- basename(file)
    
    matches <- regmatches(
      filename, regexec("USTRACT_(.+?)_emis_(.+?)_(\\d+)\\.csv", filename))
    
    if (length(matches[[1]]) > 0) {
      file_type <- matches[[1]][2]
      source <- matches[[1]][3]
      date <- matches[[1]][4]
      
      data <- fread(file)
      data$pollutant <- file_type
      data$date <- date
      new_filename <- paste0(date, "_", file_type, "_", source, ".csv")
      output_file_path <- file.path(output_folder, new_filename)
      fwrite(data, output_file_path, row.names = F)
      cat("Processed and saved", new_filename, "to", output_folder, "\n")
    }
  }
}

for (sector in sectors) {
  sector_path <- file.path(input_dir, sector)
  process_and_save_files(sector_path, output_dir)
}

cat("Processing completed.\n")

# agg road/air by year & rbind rail, calc total sector -> save nemo.csv ####

# rbind air & road
rm(list = ls())
gc()
setwd('D:/research/redenv/data/output/nemo/')

list_airports <- list.files(pattern = "airports\\.csv$")
airports <- do.call(rbind, lapply(list_airports, function(file) {
  fread(file)
}))

list_road <- list.files(pattern = "onroad_ff10\\.csv$")
roads <- do.call(rbind, lapply(list_road, function(file) {
  fread(file)
}))

airports$sector <- 'Aviation'
roads$sector <- 'Road'

join <- rbind(airports, roads)
join$date <- join$date %>% as.character()
join <- join %>% mutate(year = substr(join$date, 1, 4)) %>% dplyr::select(-date)

# sum air/road by year
group_vars <- c('TRACTID','sector','pollutant','year')
year_totals <- join %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = T)), .groups = 'drop') %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  rename_with(~ ifelse(grepl("^[0-9]", .), paste0("X", .), .)) %>%
  rename('X23' = 'X23 (g/hr)') %>%
  mutate(
    emis_total = rowSums(across(starts_with("X")), na.rm = T)) %>%
  dplyr::select(-all_of(starts_with('X')))

# load rail by year
setwd('D:/research/redenv/data/nemo_transport/rail/')

rail_list <- list.files(pattern = "\\.csv$")

load_data <- lapply(rail_list, function(file) {
  
  data <- fread(file)
  
  data[, filename := file]
  data[, pollutant := sub(".*_(NO|NO2|PM25|.*)_emis.*", "\\1", filename)] 
  data[, year := sub(".*_(\\d{4})\\.csv", "\\1", filename)]     
  data$sector <- 'Rail'
  
  data <- data %>% 
    mutate_if(is.numeric, round, 2) %>%
    rename_with(~ ifelse(grepl("^[0-9]", .), paste0("X", .), .)) %>% 
    rename('X23' = 'X23 (g/hr)') %>%
    mutate(
      emis_total = rowSums(across(starts_with("X"), ~ ., .names = "X"), 
                           na.rm = T)) %>% 
    dplyr::select(-filename,-all_of(starts_with('X')))
  return(data)
})

rail <- rbindlist(load_data, use.names = T, fill = T)
rail$year <- rail$year %>% as.numeric()

# rbind rail by year to air/road by year
join2 <- rbind(year_totals, rail) 

total_sector <- join2 %>% group_by(TRACTID, year, pollutant) %>%
  summarize(emis_total = sum(emis_total, na.rm = T), .groups = "drop") %>%
  mutate(sector = "Total")

result <- bind_rows(join2, total_sector) %>% filter(!if_any(everything(), ~ . == ""))

#fwrite(result, 'D:/research/redenv/data/output/nemo.csv')

result_wide <- result %>% pivot_wider(
  id_cols = 'TRACTID',
  names_from = c('year', 'sector', 'pollutant'),
  values_from = 'emis_total')



# join to cw.csv and multiply xp * weight = w_xp

cw <- fread('D:/research/redenv/data/output/cw.csv')

data_save <- left_join(cw, result_wide, by = 'TRACTID') %>%
  mutate(across(starts_with(c("2017", "2019")), ~ . * weight)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))


fwrite(data_save, 'D:/research/redenv/data/output/nemo_x_cw.csv')



# pivot long -> sum w_xp by groups of HOLCID/year/sector/pollutant #####
rm(list = ls())
gc()

data <- fread('D:/research/redenv/data/output/nemo_x_cw.csv') %>%
  dplyr::select(-TRACTID, -weight) %>%
  pivot_longer(
    cols = starts_with("2017") | starts_with("2019"),  
    names_to = c("year", "sector", "pollutant"), 
    names_sep = "_" , 
    values_to = "w_xp") %>%
  group_by(city, grade, HOLCID, year, sector, pollutant) %>%
  summarize(w_xp = sum(w_xp, na.rm = T), .groups = "drop")

length(unique(data$HOLCID))
fwrite(data, 'D:/research/redenv/data/output/airdisp.csv')














