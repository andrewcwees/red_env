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

# map showing city locations across US for obs set ####
rm(list = ls())
gc()

holc <- st_read("D:/research/redenv/data/holc/original_maps.gpkg") %>% 
  dplyr::filter(grade %in% c('A','B','C','D')) %>% na.omit() %>%
  dplyr::select('city', 'geom') %>%
  group_by(city) %>%
  slice(1) %>%
  ungroup()

holc <- holc[st_is_valid(holc$geom), ]

holc$geom <- st_centroid(holc$geom)

usa <- states(cb = TRUE) %>%
  filter(!NAME %in% c('United States Virgin Islands', 
                      'Commonwealth of the Northern Mariana Islands', 
                      'American Samoa', 
                      'Puerto Rico', 
                      'Guam', 
                      'Hawaii', 
                      'Alaska'))

ggplot() +
  geom_sf(data = usa, fill='white', color='grey') + 
  geom_sf(data = holc, color = 'black') +
  coord_sf(xlim = c(-125, -67), ylim = c(25, 50)) +
  theme(
    axis.text = element_text(size = 8),
    panel.grid.major = element_line(color = 'gray', linetype = 'dotted'),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, 'cm'),
    axis.line = element_line(color = 'black', linewidth = 0.7)) +
  labs(
    size = 'Population') +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)
















# histogram showing tract counts by grade for obs set ####

rm(list = ls())
gc()

data <- fread('D:/research/redenv/data/output/redxp.csv')

ggplot(data, aes(x = grade, fill = grade)) +
  geom_bar(color = "black") +  
  labs(
    x = "Grade", 
    y = "Count") +
  theme_minimal() +
  scale_fill_manual(
    name = 'Grade',
    values = c(
      "olivedrab3", 
      "skyblue2", 
      "lightgoldenrod2", 
      "lightpink2")) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5)  


# box plots for cei by grade across sectors ####
rm(list = ls())
gc()

sector_order <- c("Rail", "Road", "Aviation", "Total")
grade_order <- c("A", "B", "C", "D")
pollutant_order <- c("NO", "NO2", "PM25", "NOISE")
comparisons_list <- list(c("A", "B"), c("B", "C"), c("C", "D"), c("A", "D"))
y_positions <- c(20, 22, 24, 26) 

data <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  mutate(
    sector = factor(sector, levels = sector_order),
    grade = factor(grade, levels = grade_order),
    pollutant = factor(pollutant, levels = pollutant_order))

ggplot(data, aes(x = grade, y = cei, fill = grade)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
  geom_jitter(
    data = data %>% group_by(sector) %>% sample_frac(0.1), 
    aes(color = grade),  
    width = 0.2, alpha = 0.2, size = 0.8 ) +
  stat_compare_means(
    comparisons = comparisons_list,
    label = "p.signif",
    method = "wilcox.test",
    step.increase = 0.15,
    y.position = y_positions,
    symnum.args = list(cutpoints = c(0, 0.10, 0.05, 1), symbols = c("*", "**", "NS"))) +
  facet_grid( ~ sector, scales = "fixed") +  
  coord_cartesian(ylim = c(-5, 15)) +   
  theme_minimal() +
  labs(
    x = "Grade",
    y = "Combined Exposure Index Score",
    fill = "Grade") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) +
  scale_color_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2"))

# box plots for logxp by grade across pollutants/sectors ####
rm(list = ls())
gc()

sector_order <- c("Rail", "Road", "Aviation", "Total")
grade_order <- c("A", "B", "C", "D")
pollutant_order <- c("NO", "NO2", "PM25", "NOISE")
comparisons_list <- list(c("A", "B"), c("B", "C"), c("C", "D"), c("A", "D"))

data <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  mutate(
    sector = factor(sector, levels = sector_order),
    grade = factor(grade, levels = grade_order),
    pollutant = factor(pollutant, levels = pollutant_order))

df <- data %>% dplyr::filter(abs(z_xp) <= 3)

ggplot(df, aes(x = grade, y = logxp, fill = grade)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(
      data = data %>% group_by(grade, sector, pollutant) %>% sample_frac(0.1), 
      aes(color = grade),  
      width = 0.2, alpha = 0.2, size = 0.8 ) + 
    facet_grid(pollutant ~ sector, scales = "free_y") +
    stat_compare_means(
      comparisons = comparisons_list,
      label = "p.signif",
      method = "wilcox.test",
      step.increase = 0.15,
      symnum.args = list(cutpoints = c(0, 0.10, 0.05, 1), symbols = c("*", "**", "NS"))) +
    labs(
      x = "Grade",
      y = "Log Exposure") +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(hjust = 1),
      panel.spacing = unit(2, "lines"),
      plot.margin = margin(20, 20, 20, 20)) +
    scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) +
    scale_color_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) 



# corr plots showing z scores for noise vs air by grade across sectors ####
rm(list = ls())
gc()

data <- fread('D:/research/redenv/data/output/redxp.csv') %>%
  dplyr::select(HOLCID, grade, sector, pollutant, z_xp) %>%
  pivot_wider(
    id_cols = c('HOLCID', 'grade'),
    names_from = k
  )

plot_data <- data %>% pivot_wider(
  id_cols = c('HOLCID', 'grade'),
  names_from = c('sector', 'pollutant'),
  values_from = z_xp,
  values_fn = list(z_xp = mean),
  names_sep = '_') %>%
  pivot_longer(
    cols = everything(),
    names_to = c('sector', 'pollutant'),
    names_sep = '_',
    values_to = 'z')

# total noise vs air polls
ggplot(plot_data, aes(x = Total_NOISE, y = Total_NO, color = grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Total Noise vs Total NO",
    x = "Z Score Noise",
    y = "Z Score NO",
    color = 'Grade') +
  scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) +
  scale_color_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2"))

ggplot(plot_data, aes(x = Total_NOISE, y = Total_NO2, color = grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Total Noise vs Total NO2",
    x = "Z Score Noise",
    y = "Z Score NO2",
    color = 'Grade') +
  scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) +
  scale_color_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2"))

ggplot(plot_data, aes(x = Total_NOISE, y = Total_PM25, color = grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Total Noise vs Total PM25",
    x = "Z Score Noise",
    y = "Z Score PM25",
    color = 'Grade') +
  scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) +
  scale_color_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2"))

# total NO vs total NO2
ggplot(plot_data, aes(x = Total_NO, y = Total_NO2, color = grade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Total Noise vs Total PM25",
    x = "Z Score NO",
    y = "Z Score NO2",
    color = 'Grade') +
  scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2")) +
  scale_color_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2"))






# line plots showing average exposure between periods by grade/sector/pollutant ####
rm(list = ls())
gc()

sector_order <- c("Rail", "Road", "Aviation", "Total")
grade_order <- c("A", "B", "C", "D")
pollutant_order <- c("NO", "NO2", "PM25", "NOISE")

data <- fread('D:/research/redenv/data/output/percent_change.csv') %>%
  mutate(
    sector = factor(sector, levels = sector_order),
    grade = factor(grade, levels = grade_order),
    pollutant = factor(pollutant, levels = pollutant_order)) 

ggplot(data, aes(x = grade, y = dif, fill = grade)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(rows = vars(pollutant), cols = vars(sector),
             labeller = labeller(pollutant = label_parsed)) +
  labs(
    x = "Grade",
    y = "% Change",
    fill = "Grade") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none") +
  scale_fill_manual(values = c("olivedrab3", "skyblue2", "lightgoldenrod2", "lightpink2"))














































