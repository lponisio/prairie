## This code calculates habitat area, landscape metrics, and 
## graph-based connectivity indices
## within 200m, 500m, and 1000m radii
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

setwd('C:/')
source("lab_paths.R")
local.path

dir.bombus <- file.path(local.path, "prairie/dataPrep")
setwd(dir.bombus)

dir.create("../data/landcover")
source("src/calcHabitatArea.R")
# source("src/calcLandscapeMetrics.R")

## Create prairie centroids vector and list of rasters
## Rasters were updated NLCD via supervised classification
years <- c(2024, 2025)
buffer_dists <- c(200, 500, 1000)

centroids <- vect("../../prairie_saved/data/raw/spatial/pisgah_classification/HBRA_centroids_merge.shp")

rasters_dir <- "../../prairie_saved/data/raw/spatial/pisgah_classification"

raster_files <- file.path(rasters_dir, paste0("Pisgah_Final_Classification_", years, ".tif"))

rasters <- setNames(lapply(raster_files, rast), years)

## Reproject CRS
crs(centroids, describe = TRUE)
crs(rasters[["2024"]], describe = TRUE)

centroids <- project(centroids, crs(rasters[["2024"]]))

## Confirm alignment
crs(centroids) == crs(rasters[["2024"]])
plot(rasters[["2024"]])
plot(centroids, add = TRUE, col = "red", pch = 19)
# text(centroids, labels = centroids$MERGE_SRC, pos = 3, cex = 0.7, col = "white")

is.factor(rasters[["2024"]])
levels(rasters[["2024"]])
freq(rasters[["2024"]])

## Pull the 2024 table and rename the column
lv24 <- cats(rasters[["2024"]])[[1]]
names(lv24)[names(lv24) == "Classname"] <- "Class_name"

## Write it back to the raster
levels(rasters[["2024"]]) <- lv24

## Confirm
names(cats(rasters[["2024"]])[[1]])
levels(rasters[["2024"]])

## Create lookup table
lv <- cats(rasters[["2024"]])[[1]]
names(lv)   
class_lookup <- data.frame(Value = lv$Value, Class_name = lv$Class_name)

## Check every year uses same labels
for (yr in names(rasters)) {
  lv <- cats(rasters[[yr]])[[1]]
  print(yr); print(lv[, c("Value", "Class_name")])
}

#############################################
## -- Calculate weighted proximity area -- ##
#############################################
    
HabitatArea <- calcHabitatArea(
  rasters = rasters,
  centroids = centroids,
  years = years,
  buffer_dists = buffer_dists,
  site_field = "Site",
  class_lookup = class_lookup)

## Merge class type df with final results, arrange order, and update site names
HabitatArea <- merge(HabitatArea, class_lookup, by = "Value", all.x = TRUE)
HabitatArea <- HabitatArea[, c("Year", "Site", 
                                   "Buffer_m",
                                   "Value", "Class_name", 
                                   "Area_m2",
                                   "Area_ha")]

HabitatArea <- HabitatArea %>%
  arrange(Year, Site, Buffer_m, Value) %>% 
  pivot_wider(id_cols = c(Year, Site, Buffer_m),
              names_from = Class_name,
              values_from = c(Area_m2, Area_ha))

# spaces and slashes become underscores, case kept
HabitatArea <- HabitatArea %>%
  janitor::clean_names(case = "none")

HabitatArea <- HabitatArea %>% 
  mutate(Site = case_match(
    Site, 
    "DorrisRanch" ~ "Dorris Ranch",
    "NorthBottomland" ~ "North Bottomland",
    "SouthBottomland" ~ "South Bottomland",
    "TurtleFlats" ~ "Turtle Flats",
    .default = Site
  ))

#Rename to match puget prairies (for consistency)
HabitatArea <- HabitatArea |> 
  rename(Area_m2_Grassland_Herbaceous = Area_m2_Herbaceous)

HabitatArea$Year <- as.factor(HabitatArea$Year)
HabitatArea$Site <- as.factor(HabitatArea$Site)

save(HabitatArea, file = "../data/landcover/HabitatArea.RData")
write.csv(HabitatArea, "../data/landcover/HabitatArea.csv", row.names = FALSE)

########################################
## -- Calculate landscape metrics -- ###
########################################
check_landscape(rasters[["2017"]])

landscape_metrics <- c(
    "lsm_l_shdi",       # Shannon diversity (landscape)
    "lsm_c_enn_mn",     # Nearest neighbor distance (mean, per class)
    "lsm_c_enn_cv",     # Nearest neighbor distance (coefficient variation)
    "lsm_c_enn_sd",     # Nearest neighbor distance (stadard deviation)
    "lsm_c_ed",         # Edge density (per class)
    "lsm_c_contig_mn",  # Contiguity index (mean, per class)
    "lsm_l_contag",     # Connectance (landscape)
    "lsm_c_cohesion",   # Patch cohesion index (per class)
    "lsm_c_ai"        )  # Aggregation index (per class)

LSM_results <- calculateLSM(rasters, centroids, years,
                            buffer_dists, landscape_metrics)

LSM_results <- pivot_wider(LSM_results,
                        id_cols = c(Year, plot_id, Class_name, Buffer_m, level),
                        names_from = metric,
                        values_from = value)

LSM_results <- LSM_results %>% 
  rename(Site = plot_id) %>% 
  mutate(Site = case_when(
    Site == "Caveness" ~ "Cavness",
    Site == "Deschutes_Centroid_Project" ~ "Deschutes",
    TRUE ~ Site)) %>% 
  mutate(Class_name = case_when(
     Class_name == "Open Water" ~ "water",
     Class_name == "Deciduous Forest" ~ "deciduous_forest",
     Class_name == "Evergreen Forest" ~ "evergreen_forest",
     Class_name == "Shrubland" ~ "shrubland",
     Class_name == "Grassland/Herbaceous" ~ "grassland",
     Class_name == "Planted / Cultivated" ~ "planted",
     Class_name == "Pasture/Hay" ~ "pasture",
     Class_name == "Developed/Barren" ~ "developed",
     Class_name == "Wetlands" ~ "wetlands",
     TRUE ~ Site))

save(LSM_results, file = "../data/landcover/LandscapeMetricsResults.RData")
write.csv(LSM_results, "../data/landcover/LandscapeMetricsResults.csv", row.names = FALSE)

#######################################################
## -- Create resistance raster and then calculate -- ##
## -- Calculate graph-based connectivity indexes  -- ##
#######################################################
source("src/calcConnectivityIndex.R")
years <- c(2017, 2018, 2019, 2020)


PC_results <- calculateSitePC(
  rasters = rasters,
  centroids = centroids,
  years = years,
  analysis_radius = 500,
  resistance_table = resistance_table,
  habitat_classes = c(4),
  site_field = "MERGE_SRC",
  distance_thresholds = 500,
  probability = 0.5
)
save(PC_results, file = "../data/landcover/PConnectivityResults.RData")
write.csv(PC_results, "../data/landcover/PConnectivityResults.csv", row.names = FALSE)
