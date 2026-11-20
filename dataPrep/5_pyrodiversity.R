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
#library(Makurhini)
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
pisgah_2024 <- 2014:2023
pisgah_2025 <- 2015:2024

pisgah_sites  <- c(Meadowlark = "^ML", 
  #MeadowlarkEast = "^MLEast", MeadowlarkSouth = "^MLSouth", 
                   SpringBox = "^SpringBox", SouthBottomland= "^SB")

pisgah_24_results <- imap(pisgah_sites,
                       ~ calcPyrodiversity(sf_list = pisgah_list, site = .y, pattern = .x,
                                           window = pisgah_2024,
                                           buffers = c(250, 500, 1000)))

pisgah_25_results <- imap(pisgah_sites,
                          ~ calcPyrodiversity(sf_list = pisgah_list, site = .y, pattern = .x,
                                              window = pisgah_2025,
                                              buffers = c(250, 500, 1000)))


#Convert from list to dataframe
results_by_year <- list(`2024` = puget_24_results,
                        `2025` = puget_25_results)

pisgah_pyro <- results_by_year |>
  map(\(yr) yr |>
        compact() |>                            # drops NULL site-years like Deschutes
        map(\(x) st_drop_geometry(x$burns)) |>  # plain data frame per site
        bind_rows()) |>
  bind_rows(.id = "year") |>
  mutate(year = as.integer(year))

pisgah_pyro <- pisgah_pyro |>
  distinct(year, site, buffer_m, burn_hist, years_burned, total_burned_sqm, shannon) |>
  complete(year = 2017:2020,
           site = names(puget_sites),
           buffer_m = c(250, 500, 1000),
           fill = list(burn_hist = c("0000000000"), years_burned = "none",
                       total_burned_sqm = 0, shannon = 0))

pisgah_all <- pisgah_pyro %>% 
  mutate(site = case_match(
    site, 
    "SpringBox" ~ "North Bottomland",
    "SouthBottomland" ~ "South Bottomland",
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

