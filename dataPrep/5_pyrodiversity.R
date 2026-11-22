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

pisgah_files <- list.files(
   pisgah_dir,
   pattern = "\\.shp$",
   full.names = TRUE,
  recursive = TRUE,
   ignore.case = TRUE)

pisgah_names <- tools::file_path_sans_ext(basename(pisgah_files))

pisgah_list <- pisgah_files %>%
   set_names(pisgah_names) %>%
   map(st_read, quiet = TRUE)

saveRDS(pisgah_list, file = "../../pugent-prairie/data/pyrodiversity/pisgahBurnData.rds")

sort(names(pisgah_list))

#Change Springbox to North Bottomlands
names(pisgah_list) <- sub("^SpringBox", "NB", names(pisgah_list))

#Check
sort(names(pisgah_list))

## Project to same CRS (UTM 10N)
pisgah_list <- map(pisgah_list, st_transform, crs = 26910)

## Create burn histories and calculate pyrodiversity for Pisgah prairies
pisgah_2025 <- 2015:2024
pisgah_2026 <- 2014:2025

pisgah_sites  <- c(Meadowlark = "^ML", "North Bottomland" = "^NB", "Turtle Flats" = "^TF",
                   "South Bottomland" = "^SB", "Dorris Ranch" = "^DR",
                   Sorrenson = "^SO")

pisgah_25_results <- imap(pisgah_sites,
                       ~ calcPyrodiversity(sf_list = pisgah_list, site = .y, pattern = .x,
                                           window = pisgah_2025,
                                           buffers = c(250, 500, 1000)))

pisgah_26_results <- imap(pisgah_sites,
                          ~ calcPyrodiversity(sf_list = pisgah_list, site = .y, pattern = .x,
                                              window = pisgah_2026,
                                              buffers = c(250, 500, 1000)))


#Convert from list to dataframe
results_by_year <- list(`2025` = pisgah_25_results,
                        `2026` = pisgah_26_results)

pisgah_pyro <- results_by_year |>
  map(\(yr) yr |>
        compact() |>                            # drops NULL site-years like Deschutes
        map(\(x) st_drop_geometry(x$burns)) |>  # plain data frame per site
        bind_rows()) |>
  bind_rows(.id = "year") |>
  mutate(year = as.integer(year))

pisgah_pyro <- pisgah_pyro |>
  distinct(year, site, buffer_m, burn_hist, years_burned, total_burned_sqm, pyrodiversity) |>
  complete(year = 2025:2026,
           site = names(pisgah_sites),
           buffer_m = c(250, 500, 1000),
           fill = list(burn_hist = c("0000000000"), years_burned = "none",
                       total_burned_sqm = 0, pyrodiversity = 0))

write.csv(pisgah_pyro, file = "../../prairie/data/pyrodiversity/pisgahBurnResults.csv")
saveRDS(pisgah_pyro, file = "../../prairie/data/pyrodiversity/pisgahBurnResults.rds")
save(pisgah_pyro, file = "../../prairie/data/pyrodiversity/pisgahBurnResults.Rdata")

#################################
####     Plot Burn Units     ####
#################################

## Pull burn frequencies, buffers, and centroids for a single year to plot
pisgah_26_burns   <- map(pisgah_26_results, "burns")    %>% bind_rows() %>% 
  mutate(burn_frequency = str_count(burn_hist, "1"))
pisgah_26_buffers   <- map(pisgah_26_results, "buffers")  %>% bind_rows(.id = "site")
pisgah_26_centroids <- map(pisgah_26_results, "centroid") %>% bind_rows()

pisgah_prairies <- unique(pisgah_26_burns$site)

pisgah_plots <- set_names(pisgah_prairies) %>% 
  map(~ plotBurnFreq(burn_data = pisgah_26_burns,
                     buffer_data = pisgah_26_buffers,
                     centroid_data = pisgah_26_centroids,
                     prairie = .x,
                     limit = 0:5))
pisgah_fig <- wrap_plots(pisgah_plots, ncol = 3, guides = "collect") & theme(legend.justification = "center")

pisgah_fig

ggsave("../../prairie/dataPrep/figures/pyro/burn_units/pisgah_burn_frequency.png", pisgah_fig,
       width = 8, height = 8, dpi = 300, bg = "white")

## Meadowlark polygons with year parsed from the file names
ml_all <- pisgah_list[grepl("^ML", names(pisgah_list))] %>%
  map(~ st_sf(geometry = st_geometry(.x))) %>%
  bind_rows(.id = "source_file") %>%
  mutate(year = as.integer(str_extract(source_file, "\\d+$")),
         year = if_else(year < 100, 2000 + year, year)) %>%
  st_make_valid()

## One geometry per year, oldest first so the newest draws on top
ml_yr <- ml_all %>%
  filter(year %in% 2014:2025) %>%
  group_by(year) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf() %>%
  arrange(year) %>%
  mutate(year = factor(year))

## Okabe-Ito palette (colorblind safe), one color per burn year present
n_yrs  <- nlevels(droplevels(ml_yr$year))
ok_pal <- unname(palette.colors(9, "Okabe-Ito")[-1])   # drops black
ok_pal <- ok_pal[c(5, 1, 3, 6, 2, 4, 7, 8)]            # blue, orange, green, vermillion, ...
yr_cols <- setNames(ok_pal[seq_len(n_yrs)], levels(droplevels(ml_yr$year)))

## Stacked / overlaid figure
ml_stack <- ggplot(ml_yr) +
  geom_sf(aes(fill = year, color = year), alpha = 0.4, linewidth = 0.7) +
  scale_fill_manual(values = yr_cols, name = "Burn year") +
  scale_color_manual(values = yr_cols, name = "Burn year") +
  labs(title = "Meadowlark Burn Units by Year") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        legend.title = element_text(face= "bold", size = 16),
        legend.text = element_text(size = 14))

ml_stack

ggsave("../../prairie/dataPrep/figures/pyro/burn_units/MLBurnUnitsYear.png", ml_stack,
       width = 8, height = 8, dpi = 300, bg = "white")

## Isometric transform: rotate 45 degrees, then squash y
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
M   <- rot(pi / 4) %*% diag(c(1, 0.5))     # change 0.5 for a steeper/flatter tilt

## Shared extent so every layer is the same size plane
bb    <- st_bbox(ml_all)
side  <- max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
ctr   <- c((bb[["xmin"]] + bb[["xmax"]]) / 2, (bb[["ymin"]] + bb[["ymax"]]) / 2)
plane <- st_as_sfc(bb) %>% st_buffer(0.05 * side, joinStyle = "MITRE", mitreLimit = 2)

iso <- function(g, dy) {
  out <- (g - ctr) * M + c(0, dy)
  st_set_crs(out, NA)                       # layers are now a drawing, not map coordinates
}

gap <- 0.35 * side                          # vertical spacing between layers
yrs <- levels(droplevels(ml_yr$year))       # oldest first = bottom layer

burns <- map(seq_along(yrs), function(i) {
  g <- ml_yr %>% filter(year == yrs[i])
  st_sf(year = yrs[i], geometry = iso(st_geometry(g), (i - 1) * gap))
}) %>% bind_rows() %>% mutate(year = factor(year, levels = yrs))

planes <- map(seq_along(yrs), function(i) {
  st_sf(year = yrs[i], geometry = iso(plane, (i - 1) * gap))
}) %>% bind_rows()

## Year labels at the left corner of each plane
lab_df <- planes %>%
  mutate(x = map_dbl(geometry, ~ st_bbox(.x)[["xmin"]]),
         y = map_dbl(geometry, ~ mean(st_bbox(.x)[c("ymin", "ymax")]))) %>%
  st_drop_geometry()

ml_layers <- ggplot() +
  geom_sf(data = planes, fill = NA, color = "grey50", linewidth = 0.3) +
  geom_sf(data = burns, aes(fill = year, color = year),
          alpha = 0.85, linewidth = 0.3) +
  geom_text(data = lab_df, aes(x = x - 0.03 * side, y = y, label = year),
            hjust = 1, fontface = "bold", size = 8) +
  scale_fill_manual(values = yr_cols, guide = "none") +
  scale_color_manual(values = yr_cols, guide = "none") +
  coord_sf(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 40))

ml_layers

ggsave("../../prairie/dataPrep/figures/pyro/burn_units/MLBurnUnitsYearStack.png", ml_layers,
       width = 8, height = 8, dpi = 300, bg = "white")
