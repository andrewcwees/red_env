### libs #######################################################################
library(sf)
library(httr)
library(dplyr)
library(FSA)
library(skimr)
library(ggpubr)
library(data.table)
library(terra)
library(jsonlite)
library(mapview)
library(tidyr)
library(terra)
library(exactextractr)
library(stars)
library(raster)
library(tidyverse)
library(ggspatial)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# sumstats xp ####
rm(list = ls())
gc()

sector_order <- c("Rail", "Road", "Aviation", "Total")
grade_order <- c("A", "B", "C", "D")
pollutant_order <- c("NO", "NO2", "PM25", "NOISE")

data <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  mutate(
    sector = factor(sector, levels = sector_order),
    grade = factor(grade, levels = grade_order),
    pollutant = factor(pollutant, levels = pollutant_order))

sumstats <- data %>% 
  group_by(grade, sector, pollutant) %>%
  summarise(
    mean = paste0(round(mean(xp), 2)),
    sd = paste0(round(sd(xp), 2)),
    minmax = paste0(round(min(xp), 2), " - ", round(max(xp), 2)),
    iqr = round(IQR(xp), 2),
    count = n(),
    .groups = 'drop') %>%
  arrange(sector, grade) 

# save full table and by pollutant
fwrite(sumstats, "D:/research/redenv/data/output/stats_tables/sumstats/sumstats.csv")

output_dir <- "D:/research/redenv/data/output/stats_tables/sumstats/"

sumstats %>% group_split(pollutant) %>%
  walk(function(df) {
    pol <- unique(df$pollutant)
    file_name <- paste0(output_dir, "/", pol, "_sumstats.csv")
    fwrite(df, file_name, row.names = FALSE)})

# sumstats cei ####
rm(list = ls())
gc()

sector_order <- c("Rail", "Road", "Aviation", "Total")
grade_order <- c("A", "B", "C", "D")

data <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  mutate(
    grade = factor(grade, levels = grade_order),
    sector = factor(sector, levels = sector_order))

cei_by_grade <- data %>% 
  group_by(grade, sector) %>%
  summarise(
    mean = paste0(round(mean(cei), 2)),
    sd = paste0(round(sd(cei), 2)),
    min_max = paste0(round(min(cei), 2), " - ", round(max(cei), 2)),
    iqr = round(IQR(cei), 2),
    count = n(),
    .groups = 'drop') %>%
  arrange(sector, grade) 

# save
fwrite(cei_by_grade, 'D:/research/redenv/data/output/stats_tables/sumstats/sumstats_cei.csv')

# tests ####

rm(list = ls())
gc()

data <- fread('D:/research/redenv/data/output/redxp.csv')

# KW for CEI by sector
kw_cei <- data %>%
  group_by(sector) %>%
  summarise(
    p_value = kruskal.test(cei ~ grade)$p.value,
    .groups = 'drop')
print(kw_cei)

# dunns for CEI
dunn_cei <- data %>%
  group_by(sector) %>%
  summarise(
    test_result = list(dunnTest(cei ~ grade, method = 'bh')),
    .groups = 'drop')
dunn_cei$test_result

# KW for pollutants by sector
kw_results_grouped <- data %>%
  group_by(pollutant, sector) %>%
  summarise(
    p_value = kruskal.test(xp ~ grade)$p.value,
    .groups = 'drop')
print(kw_results_grouped)

dunn_results_grouped <- data %>%
  group_by(pollutant, sector) %>%
  summarise(
    test_result = list(dunnTest(xp ~ grade, method = 'bh')),
    .groups = 'drop')
dunn_results_grouped$test_result




# city analysis ####
rm(list = ls())
gc()

data <- fread('D:/research/redenv/data/output/redxp.csv')

# 10 most polluted cities by pollutant for 'total' sector
top10_xp <- data %>% 
  dplyr::filter(sector == 'Total') %>%
  group_by(city, pollutant) %>%
  summarise(xp = mean(xp, na.rm = TRUE), .groups = "drop") %>%
  group_by(pollutant) %>%
  slice_max(xp, n = 10)
print(top10_xp)

# 10 most disparate cities by pollutant for 'total' sector
top10_disp <- data %>% 
  dplyr::filter(sector == 'Total') %>%
  group_by(city, grade) %>%
  summarise(cei = mean(cei, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(id_cols = city, names_from = grade, values_from = cei) %>% 
  dplyr::select(city, A, D) %>% 
  mutate(dif = D - A) %>% 
  slice_max(dif, n = 10)

ggplot(top10_disp, aes(x = dif, y = reorder(city, dif))) +
  geom_col(fill = 'black') + 
  labs(
    x = "D ~ A Scale of Difference in Total Weighted Exposure",
    y = "City") +
  theme_minimal() +  
  theme(
    strip.text = element_text(size = 10, face = "bold"), 
    panel.spacing = unit(2, "lines"),
    axis.line = element_line(color = "black", linewidth = 0.8)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

















# scale of difference ####
rm(list = ls())
gc()

rates_xp <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  group_by(sector, pollutant) %>%
  summarize(
    mean_A = mean(xp[grade == "A"], na.rm = TRUE),
    mean_D = mean(xp[grade == "D"], na.rm = TRUE),
    disp_rate = ((mean_D - mean_A) / mean_A) * 100,
    .groups = "drop")

rates_cei <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  group_by(sector) %>%
  summarize(
    mean_A = mean(cei[grade == "A"], na.rm = TRUE),
    mean_D = mean(cei[grade == "D"], na.rm = TRUE),
    disp_rate = ((mean_D - mean_A) / mean_A) * 100,
    .groups = "drop")












# mean dif and % change between periods ####
rm(list = ls())
gc()

sector_order <- c("Rail", "Road", "Aviation", "Total")
grade_order <- c("A", "B", "C", "D")
pollutant_order <- c('NO', 'NO2', 'PM25', 'NOISE')

data <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  mutate(
    sector = factor(sector, levels = sector_order),
    grade = factor(grade, levels = grade_order),
    pollutant = factor(pollutant, levels = pollutant_order))

sumstats_period <- data %>% 
  group_by(grade, sector, period, pollutant) %>%
  summarise(
    mean = paste0(round(mean(xp), 2)),
    .groups = 'drop') %>%
  arrange(sector, grade) %>% pivot_wider(
    names_from = period,
    names_glue = "mean_{period}", 
    values_from = mean,
    id_cols = c('grade', 'sector', 'pollutant')) 

sumstats_period$mean_1 <- as.numeric(sumstats_period$mean_1)
sumstats_period$mean_2 <- as.numeric(sumstats_period$mean_2)
sumstats_period <- sumstats_period %>% 
  mutate(dif = ((mean_2 - mean_1) / mean_1) * 100)

fwrite(sumstats_period, 'D:/research/redenv/data/output/percent_change.csv')

export <- sumstats_period %>% dplyr::filter(sector == 'Total') %>%
  arrange(pollutant, grade)
export$sector <- NULL

fwrite(export, 'D:/research/redenv/data/output/stats_tables/sumstats/period_change.csv')


















