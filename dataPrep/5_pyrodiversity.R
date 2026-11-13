## This code calculates pyrodiversity using burn units shape files (Ulyshen et al. 2023)

rm(list=ls())
library(tidyverse)
library(vegan)
library(ggplot2)
library(scales)
library(terra)
library(landscapemetrics)
library(tidyverse)
library(lconnect)
library(Makurhini)
library(sf)
library(vegan)
library(viridis)
library(patchwork)

setwd('C:/')
source("lab_paths.R")
local.path

dir.bombus <- file.path(local.path, "prairie/dataPrep")
setwd(dir.bombus)
#Check spatial resolution of polygons
#plot Susan's data y my calculations x
pisgah_dir <- "../../prairie_saved/data/raw/spatial/pisgah_burnunits"
dir.create("../../prairie/data/pyrodiversity")
dir.create("../../prairie/dataPrep/figures")
dir.create("../../prairie/dataPrep/figures/pyro")
dir.create("../../prairie/dataPrep/figures/pyro/burn_units")

source("src/calcPyrodiversity.R")

pisgah_list <- readRDS("../../prairie/data/pyrodiversity/pisgahBurnData.rds")

options(scipen = 999)


## Loads in shapefiles and saves as a list

# pisgah_files <- list.files(
#   pisgah_dir,
#   pattern = "\\.shp$",
#   full.names = TRUE,
#   recursive = TRUE,
#   ignore.case = TRUE)
# 
# pisgah_names <- tools::file_path_sans_ext(basename(pisgah_files))
# 
# pisgah_list <- pisgah_files %>% 
#   set_names(pisgah_names) %>% 
#   map(st_read, quiet = TRUE)

# saveRDS(pisgah_list, file = "../../pugent-prairie/data/pyrodiversity/pisgahBurnData.rds")

# names(pisgah_list)

## Project to same CRS (UTM 10N)
pisgah_list <- map(pisgah_list, st_transform, crs = 26910)

## Create burn histories and calculate pyrodiversity for Pisgah prairies
pisgah_window <- 2014:2023
pisgah_sites  <- c(Meadowlark = "^ML", 
  #MeadowlarkEast = "^MLEast", MeadowlarkSouth = "^MLSouth", 
                   SpringBox = "^SpringBox", SouthBottomland= "^SB")

pisgah_results <- imap(pisgah_sites,
                       ~ calcPyrodiversity(sf_list = pisgah_list, site = .y, pattern = .x,
                                           window = pisgah_window,
                                           buffers = c(250, 500, 1000)))
## Pull burn frequencies, buffers, and centroids to plot
pisgah_burns   <- map(pisgah_results, "burns")    %>% bind_rows() %>% 
  mutate(burn_frequency = str_count(burn_hist, "1"))
pisgah_buffers   <- map(pisgah_results, "buffers")  %>% bind_rows(.id = "site")
pisgah_centroids <- map(pisgah_results, "centroid") %>% bind_rows()

## Convert from list to data frame
pisgah_all <- map(pisgah_results, "burns") %>% 
  do.call(rbind, .) %>% 
  st_drop_geometry()

## Add data for sites with no burns in 10 year period
noburnSites <- data.frame(
  site = c(rep("North Bottomlands", times = 3), rep("Sorrenson", times = 3), rep("Turtle Flats", times = 3)),
  buffer_m = rep(c(250, 500, 1000), times = 3),
  burn_hist = rep(c("0000000000"), times = 9),
  years_burned = rep("none", times = 9),
  area_sqm = rep(c(196349.54, 785398.16, 3141592.65), times = 3),
  total_burned_sqm = rep(0, times = 9),
  shannon = rep(0, times = 9)
)

# Merge datasets
pisgah_all <- bind_rows(
  pisgah_all, noburnSites
)


pisgah_all <- pisgah_all %>% 
  mutate(site = case_match(
    site, 
    "DorrisRanch" ~ "Dorris Ranch",
    "NorthBottomland" ~ "North Bottomland",
    "SouthBottomland" ~ "South Bottomland",
    "TurtleFlats" ~ "Turtle Flats",
    .default = site
  )) %>% 
  rename(Site = site)

write.csv(pisgah_all, file = "../../prairie/data/pyrodiversity/pisgahBurnResults.csv")
saveRDS(pisgah_all, file = "../../prairie/data/pyrodiversity/pisgahBurnResults.rds")
save(pisgah_all, file = "../../prairie/data/pyrodiversity/pisgahBurnResults.Rdata")

#################################
####     Plot Burn Units     ####
#################################

pisgah_prairies <- unique(pisgah_burns$site)

pisgah_plots <- set_names(pisgah_prairies) %>% 
  map(~ plotBurnFreq(burn_data = pisgah_burns,
                     buffer_data = pisgah_buffers,
                     centroid_data = pisgah_centroids,
                     prairie = .x,
                     limit = 0:3))
pisgah_fig <- wrap_plots(pisgah_plots, ncol = 2, guides = "collect") & theme(legend.justification = "center")

ggsave("../../prairie/dataPrep/figures/pyro/burn_units/pisgah_burn_frequency.png", pisgah_fig,
       width = 8, height = 8, dpi = 300, bg = "white")

