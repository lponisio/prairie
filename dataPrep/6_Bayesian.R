#This script fits and plots Bayesian models
rm(list=ls())
library(brms)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom.mixed)
library(posterior)
library(patchwork)
library(cmdstanr)
library(lme4)
library(tidyverse)
library(ggridges)
library(bayesplot)
library(graphics)
library(testthat)
library(loo)


setwd('C:/')
source("lab_paths.R")
local.path
dir.bombus <- file.path(local.path, "prairie/dataPrep")
setwd(dir.bombus)
load('../data/spec_traits.Rdata')
load('../data/landcover/HabitatArea.RData')
climate <- read.csv("../data/climate/climate.csv")
pyrodiversity <- read.csv('../data/pyrodiversity/pisgahBurnResults.csv')

source("src/fit_model.R")
source("src/save_models.R")
source("src/plot_models.R")
source("src/summarize_models.R")
source("src/plot_flexible.R")
source("src/fit_MLModel.R")

pyrodiversity <- pyrodiversity |> 
  rename(Site = site, Year = year, 
         Pyrodiversity = pyrodiversity) |> 
  mutate(Site = as.factor(Site),
         Year = as.factor(Year)) |> 
  select(-X) |> 
  summarise(
    Pyrodiversity = first(Pyrodiversity),
    .by = c(Year, Site, buffer_m))

spec.net <- spec.net |> 
  mutate(Site = case_when(
    Site =="DR" ~ "Dorris Ranch",
    Site == "ML" ~ "Meadowlark",
    Site == "NB" ~ "North Bottomland",
    Site == "SB" ~ "South Bottomland",
    Site == "SO" ~ "Sorrenson",
    Site == "TF" ~ "Turtle Flats",
    .default = as.character(Site))) |> 
  mutate(SampleRound = as.factor(SampleRound),
         Site = as.factor(Site),
         Year = as.factor(Year)) |> 
  filter(Site != "VP")


climate <- climate %>% 
  # mutate(Site = case_when(
  #   Site == "Glacial" ~ "Glacial Heritage",
  #   Site == "Wolf" ~ "Wolf Haven",
  #   TRUE ~ Site)) |> 
  mutate(Site = as.factor(Site),
         SampleRound = as.factor(SampleRound),
         Year= as.factor(Year))


## Prep Pyrodiversity, Climate, and Spec Data ##
spec.net <- spec.net %>% 
  left_join(climate, by = c("Site", "Year", "SampleRound")) %>% 
  left_join(pyrodiversity, by = c("Site", "Year"), relationship = "many-to-many") %>% 
  summarise(
    BeeFDivSiteYearSR = first(BeeFDivSiteYearSR),
    BeeFDisSiteYearSR = first(BeeFDisSiteYearSR),
    BeeFEveSiteYearSR = first(BeeFEveSiteYearSR),
    BeeRichness = first(BeeRichness),
    BeeDiversity = first(BeeDiversity),
    BeeAbundance = first(BeeAbundance),
    IntRichnessSiteYearSR = first(IntRichnessSiteYearSR), 
    IntDiversitySiteYearSR = first(IntDiversitySiteYearSR),
    UniqueIntSiteYearSR = first(UniqueIntSiteYearSR),
    NicheOverlap = first(niche.overlap.HL),
    VegDiversity = first(VegDiversity),
    Pyrodiversity = first(Pyrodiversity),
    GDD = first(GDD),
    .by = c(Site, Year, SampleRound, buffer_m))
# APi = first(APi),
# logBeeFDivSiteYearSR = log(BeeFDivSiteYearSR),
# logBeeFDisSiteYearSR = log(BeeFDisSiteYearSR), 
# logBeeFEveSiteYearSR = log(BeeFEveSiteYearSR)) 


pyro_250m <- spec.net |> 
  filter(buffer_m == 250)

pyro_500m <- spec.net |> 
  filter(buffer_m == 500)

pyro_1000m <- spec.net |> 
  filter(buffer_m == 1000)

####### Visualize #######
pairs(pyro_500m[, c("BeeFDisSiteYearSR", "BeeFDivSiteYearSR",
                    "BeeDiversity", "BeeRichness", "BeeAbundance", 
                    "VegDiversity", "Pyrodiversity", "GDD")])

### Prep source habitat data ####
HabitatArea <- HabitatArea |>
  mutate(Year = factor(as.numeric(as.character(Year)) + 1))

SourceHabitat <- left_join(HabitatArea, spec.net, by = c("Site", "Year"),
                           relationship = "many-to-many") 
SourceHabitat <- SourceHabitat |> 
  summarise(
    deciduous_forest = first(Area_ha_Deciduous_Forest),
    evergreen_forest = first(Area_ha_Evergreen_Forest),
    grassland = first(Area_ha_Herbaceous),
    BeeFDivSiteYearSR = first(BeeFDivSiteYearSR),
    BeeFDisSiteYearSR = first(BeeFDisSiteYearSR),
    BeeFEveSiteYearSR = first(BeeFEveSiteYearSR),
    BeeRichness = first(BeeRichness),
    BeeDiversity = first(BeeDiversity),
    BeeAbundance = first(BeeAbundance),
    IntRichnessSiteYearSR = first(IntRichnessSiteYearSR), 
    IntDiversitySiteYearSR = first(IntDiversitySiteYearSR),
    UniqueIntSiteYearSR = first(UniqueIntSiteYearSR),
    NicheOverlap = first(NicheOverlap),
    .by = c(Site, Year, SampleRound, Buffer_m)
  )

SourceHabitat_250m <- SourceHabitat %>% 
  filter(Buffer_m == 250)

SourceHabitat_500m <- SourceHabitat %>% 
  filter(Buffer_m == 500)

SourceHabitat_1000m <- SourceHabitat %>% 
  filter(Buffer_m == 1000)

### Visualize ####
pairs(SourceHabitat_500m[, c("BeeFDisSiteYearSR", "BeeFDivSiteYearSR",
                             "BeeDiversity", "BeeRichness", "BeeAbundance", 
                             "deciduous_forest", "evergreen_forest", "grassland")])

### Prep combine Data ###
### 500 m is where we see effects of landscape and pyrodiversity ###
pyroHabitat <- SourceHabitat_500m |> 
  select(Site, Year, SampleRound, deciduous_forest, evergreen_forest, grassland) |> 
  left_join(pyro_500m, by = c("Site", "Year", "SampleRound"))

#####################################################################
##  Multilevel Models
# Does more diverse fire history lead to more diverse flowers?
# Does more diverse vegetation lead to more diverse bees?
# Does pyrodiversity affect bees only through vegetation (indirect), or also directly?

#####################################################################
## Questions for Lauren ##
## 1. Can I include buffer radius in the model or do I need to model them separately? 
## 2. Include GDD in veg model in hierarchical model
## 3. Do I need to subset the data to exclude rows with NAs? (e.g., divergence + dispersion)
## 4. Do I need to put source habitat in the hierarchical model too or keep separate?
## 5. Susan had a strong direct effect of pyrodiversity on bee diversity, whats up with that?

## Floral Model
flowers <- bf(VegDiversity ~ scale(Pyrodiversity) + scale(GDD) + (1 | Site), family = gaussian())

######Dispersion#######
bees   <- bf(BeeFDisSiteYearSR  ~ scale(Pyrodiversity) + VegDiversity + 
               scale(grassland) + scale(deciduous_forest) + 
               (1 | Site), family = skew_normal())

ml_vegPyroBeeFDis <- fit_ml(
  flowers + bees, 
  data = pyroHabitat)

mcmc_plot(ml_vegPyroBeeFDis)
mcmc_trace(ml_vegPyroBeeFDis)

## To test for indirect association
indirect_effect(ml_vegPyroBeeFDis, "b_VegDiversity_scalePyrodiversity", "b_BeeFDisSiteYearSR_VegDiversity")

######Divergence#######
bees   <- bf(BeeFDivSiteYearSR  ~ scale(Pyrodiversity) + VegDiversity + 
               scale(grassland) + scale(deciduous_forest) + 
               (1 | Site), family = skew_normal())

ml_vegPyroBeeFDiv <- fit_ml(
  flowers + bees, 
  data = pyroHabitat)

mcmc_plot(ml_vegPyroBeeFDiv)
mcmc_trace(ml_vegPyroBeeFDiv)

## To test for indirect association
indirect_effect(ml_vegPyroBeeFDiv, "b_VegDiversity_scalePyrodiversity", "b_BeeFDivSiteYearSR_VegDiversity")

#######Diversity#########
bees   <- bf(BeeDiversity ~ scale(Pyrodiversity) + VegDiversity + 
               scale(grassland) + scale(deciduous_forest) + 
               (1 | Site), family = skew_normal())

ml_vegPyroBeeDiversity <- fit_ml(
  flowers + bees, 
  data = pyroHabitat)

mcmc_plot(ml_vegPyroBeeDiversity)
mcmc_trace(ml_vegPyroBeeDiversity)

## To test for indirect association
indirect_effect(ml_vegPyroBeeDiversity, "b_VegDiversity_scalePyrodiversity", "b_BeeDiversity_VegDiversity")

#######Interaction Diversity###########
bees   <- bf(IntDiversitySiteYearSR ~ scale(Pyrodiversity) + VegDiversity +
               scale(grassland) + scale(deciduous_forest) +
               (1 | Site), family = skew_normal())

ml_vegPyroIntDiversity <- fit_ml(
  flowers + bees, 
  data = pyroHabitat)

mcmc_plot(ml_vegPyroIntDiversity)
mcmc_trace(ml_vegPyroIntDiversity)

## To test for indirect association
indirect_effect(ml_vegPyroIntDiversity, "b_VegDiversity_scalePyrodiversity", "b_IntDiversitySiteYearSR_VegDiversity")

#######Niche Overlap###########
bees   <- bf(NicheOverlap ~ scale(Pyrodiversity) + VegDiversity +
               scale(grassland) + scale(deciduous_forest) +
               (1 | Site), family = skew_normal())

ml_vegPyroNiche <- fit_ml(
  flowers + bees, 
  data = pyroHabitat)

mcmc_plot(ml_vegPyroNiche)
mcmc_trace(ml_vegPyroNiche)

## To test for indirect association
indirect_effect(ml_vegPyroNiche, "b_VegDiversity_scalePyrodiversity", "b_NicheOverlap_VegDiversity")


## To save models
ML_Models <- list(
  "Functional Dispersion"  = ml_vegPyroBeeFDis,
  "Functional Divergence"  = ml_vegPyroBeeFDiv,
  "Bee Diversity"          = ml_vegPyroBeeDiversity,
  "Interaction Diversity"  = ml_vegPyroIntDiversity,
  "Niche Overlap (HL)"     = ml_vegPyroNiche
)

dir.create("brms_models/puget", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/pp_checks/puget", recursive = TRUE, showWarnings = FALSE)
dir.create("results/puget", showWarnings = FALSE, recursive = TRUE)

save_models_list(ML_Models,  "brms_models/puget/ML_Models")

summarise_fixed_ml(ML_Models, "results/ml_fixed.csv")

savePPCheck(ML_Models,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/puget/ML_Models",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix   = "MLModel")

ML_files <- list.files("brms_models/puget/ML_Models", pattern = "\\.rds$", full.names = TRUE)


models_ML <- setNames(
  lapply(ML_files, readRDS),
  tools::file_path_sans_ext(basename(ML_files)))


####################################
######     Plot     ################
####################################
dispersionPlot <- plot_path_effects(
  ml_vegPyroBeeFDis,
  predictor = "scalePyrodiversity", 
  mediator  = "VegDiversity",
  response  = "BeeFDisSiteYearSR",
  labels = c(a = "Pyrodiversity -> Veg Diversity",
             b = "Veg diversity -> Bee Dispersion",
             direct = "Pyrodiversity (Direct) -> Bee Dispersion",
             indirect = "Pyrodiversity (Indirect) -> Bee Dispersion"), 
  return_data = TRUE)
dispersionPlot

divergencePlot <- plot_path_effects(
  ml_vegPyroBeeFDiv,
  predictor = "scalePyrodiversity", 
  mediator  = "VegDiversity",
  response  = "BeeFDivSiteYearSR",
  labels = c(a = "Pyrodiversity -> Veg Diversity",
             b = "Veg diversity -> Bee Divergence",
             direct = "Pyrodiversity (Direct) -> Bee Divergence",
             indirect = "Pyrodiversity (Indirect) -> Bee Divergence"), 
  return_data = TRUE)
divergencePlot


diversityPlot <- plot_path_effects(
  ml_vegPyroBeeDiversity,
  predictor = "scalePyrodiversity", 
  mediator  = "VegDiversity",
  response  = "BeeDiversity",
  labels = c(a = "Pyrodiversity -> Veg Diversity",
             b = "Veg diversity -> Bee Diversity",
             direct = "Pyrodiversity (Direct) -> Bee Diversity",
             indirect = "Pyrodiversity (Indirect) -> Bee Diversity"), 
  return_data = TRUE)

# get the draws 
diversityPlot$data %>% group_by(path) %>% median_qi(value, .width = 0.95)

IntDiversityPlot <- plot_path_effects(
  ml_vegPyroIntDiversity,
  predictor = "scalePyrodiversity", 
  mediator  = "VegDiversity",
  response  = "IntDiversitySiteYearSR",
  labels = c(a = "Pyrodiversity -> Veg Diversity",
             b = "Veg diversity -> Interaction Diversity",
             direct = "Pyrodiversity (Direct) -> Interaction Diversity",
             indirect = "Pyrodiversity (Indirect) -> Interaction Diversity"), 
  return_data = TRUE)

#####################################################################
##  Bee response as a function of pyrodiversity and veg diversity ###
#####################################################################


#####################################################################
##  Multilevel Models
#####################################################################
## Floral Model
flowers <- bf(VegDiversity ~ scale(Pyrodiversity) + (1 | Site), family = gaussian())


######Divergence#######
bees   <- bf(BeeFDivSiteYearSR  ~ scale(Pyrodiversity) + VegDiversity + (1 | Site), family = skew_normal())

ml_vegPyroBeeFDiv <- fit_ml(
  flowers + bees, 
  data = pyro_500m)

## To test for indirect association
indirect_effect(ml_vegPyroBeeFDiv, "b_VegDiversity_scalePyrodiversity", "b_BeeFDivSiteYearSR_VegDiversity")

#######Diversity#########
bees   <- bf(BeeDiversity ~ scale(Pyrodiversity) + VegDiversity + (1 | Site), family = skew_normal())

ml_vegPyroBeeDiversity <- fit_ml(
  flowers + bees, 
  data = pyro_500m)

## To test for indirect association
indirect_effect(ml_vegPyroBeeDiversity, "b_VegDiversity_scalePyrodiversity", "b_BeeDiversity_VegDiversity")

#######Interaction Diversity###########
bees   <- bf(IntDiversitySiteYearSR ~ scale(Pyrodiversity) + VegDiversity + (1 | Site), family = skew_normal())

ml_vegPyroIntDiversity <- fit_ml(
  flowers + bees, 
  data = pyro_500m)

## To test for indirect association
indirect_effect(ml_vegPyroIntDiversity, "b_VegDiversity_scalePyrodiversity", "b_IntDiversitySiteYearSR_VegDiversity")

## To save models
summarise_fixed_ml(list(plants_bees = fit_mv1), "output/mv_fixed.csv")

#####################################################################
##  Bee response as a function of pyrodiversity and veg diversity ###
#####################################################################

pyro <- c("scale(Pyrodiversity) + 
          scale(VegDiversity) + 
          (1|Site)")

# get_prior(BeeFDivSiteYearSR ~ scale(Pyrodiversity) + 
#             scale(VegDiversity) + 
#             (1|Site), data = pyro_500m)
# 
# pri <- c(
#   prior(normal(0, 0.15), class = sd),       
#   prior(normal(0.2, 0.3), class = Intercept)
# )

######### Dispersion ##########
m_pyroFDis_1000m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroFDis_500m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroFDis_250m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

####### Divergence #######
m_pyroFDiv_1000m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = pyro_1000m,
  rhs  = pyro,
  # prior = pri,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroFDiv_500m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = pyro_500m,
  rhs  = pyro,
  # prior = pri,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

# moderate evidence
m_pyroFDiv_250m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = pyro_250m,
  rhs  = pyro,
  # prior = pri,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

######### Diversity ##########
m_pyroPollDiversity_1000m <- fit_one(
  resp = "BeeDiversity",
  data = pyro_1000m,
  rhs  = pyro,
  family = gaussian(),
  pp_type = "dens_overlay"
)

m_pyroPollDiversity_500m <- fit_one(
  resp = "BeeDiversity",
  data = pyro_500m,
  rhs  = pyro,
  family = gaussian(),
  pp_type = "dens_overlay"
)

m_pyroPollDiversity_250m <- fit_one(
  resp = "BeeDiversity",
  data = pyro_250m,
  rhs  = pyro,
  family = gaussian(),
  pp_type = "dens_overlay"
)

######### Richness ###########
m_pyroPollRichness_1000m <- fit_one(
  resp = "BeeRichness",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroPollRichness_500m <- fit_one(
  resp = "BeeRichness",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroPollRichness_250m <- fit_one(
  resp = "BeeRichness",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

######## Abundance #########
m_pyroAbundance_1000m <- fit_one(
  resp = "BeeAbundance",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroAbundance_500m <- fit_one(
  resp = "BeeAbundance",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroAbundance_250m <- fit_one(
  resp = "BeeAbundance",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

######### Interaction Richness ###########
m_pyroIntRichness_1000m <- fit_one(
  resp = "IntRichnessSiteYearSR",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroIntRichness_500m <- fit_one(
  resp = "IntRichnessSiteYearSR",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroIntRichness_250m <- fit_one(
  resp = "IntRichnessSiteYearSR",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

########## Interactoin Diversity #########
m_pyroIntDiversity_1000m <- fit_one(
  resp = "IntDiversitySiteYearSR",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroIntDiversity_500m <- fit_one(
  resp = "IntDiversitySiteYearSR",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroIntDiversity_250m <- fit_one(
  resp = "IntDiversitySiteYearSR",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

######### Unique Interactions ##########
m_pyroUniqInt_1000m <- fit_one(
  resp = "UniqueIntSiteYearSR",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroUniqInt_500m <- fit_one(
  resp = "UniqueIntSiteYearSR",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroUniqInt_250m <- fit_one(
  resp = "UniqueIntSiteYearSR",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

######### Niche Overlap - Network Level ##########
m_pyroNicheOverlap_1000m <- fit_one(
  resp = "NicheOverlap",
  data = pyro_1000m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroNicheOverlap_500m <- fit_one(
  resp = "NicheOverlap",
  data = pyro_500m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroNicheOverlap_250m <- fit_one(
  resp = "NicheOverlap",
  data = pyro_250m,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)


##################################################################################
# -- Bee and interaction response as a function source habitat -- #
##################################################################################
pairs(SourceHabitat_500m[, c("BeeFDisSiteYearSR", "BeeFDivSiteYearSR",
                             "BeeDiversity", "BeeRichness", "BeeAbundance", 
                             "NicheOverlap", "IntRichnessSiteYearSR",
                             "IntDiversitySiteYearSR", "UniqueIntSiteYearSR",
                             "deciduous_forest", "evergreen_forest", "grassland")])

landscape <- "scale(deciduous_forest) +
              scale(evergreen_forest) +
              scale(grassland) + (1|Site)"
# 
# get_prior(BeeDiversity ~ scale(deciduous_forest) + scale(evergreen_forest) + 
#             scale(grassland) + (1 | Site), 
#           data = SourceHabitat_1000m, family = skew_normal())

###### Dispersion #######
m_FDIsHabitat1000m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIsHabitat500m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIsHabitat250m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Divergence ######
m_FDIvHabitat1000m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIvHabitat500m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIvHabitat250m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Diversity ######
## what effect sizes are plausible on the scale of my response?
## Weakly informative priors scaled to the response 
pri <- c(
  prior(normal(0, 0.5), class = b),
  prior(exponential(2), class = sd),   
  prior(exponential(1), class = sigma)
)

m_DiversityHabitat1000m <- fit_one(
  resp = "BeeDiversity",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_DiversityHabitat500m <- fit_one(
  resp = "BeeDiversity",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_DiversityHabitat250m <- fit_one(
  resp = "BeeDiversity",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Richness #######
m_RichnessHabitat1000m <- fit_one(
  resp = "BeeRichness",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)
# 96% posterior support for negative relationship with deciduous forest
# 96% posteroir support for positive relationship with grassland
m_RichnessHabitat500m <- fit_one(
  resp = "BeeRichness",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_RichnessHabitat250m <- fit_one(
  resp = "BeeRichness",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

####### Abundance ########
m_AbundHabitat1000m <- fit_one(
  resp = "BeeAbundance",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

## 96% posteroir support for positive relationship with grassland
m_AbundHabitat500m <- fit_one(
  resp = "BeeAbundance",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_AbundHabitat250m <- fit_one(
  resp = "BeeAbundance",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Interaction Richness ######

## 94% posterior support for positive relationship with grassland
m_IntRichnessHabitat1000m <- fit_one(
  resp = "IntRichnessSiteYearSR",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

## 98% posterior support for positive relationship with grassland
m_IntRichnessHabitat500m <- fit_one(
  resp = "IntRichnessSiteYearSR",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_IntRichnessHabitat250m <- fit_one(
  resp = "IntRichnessSiteYearSR",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Interaction Diversity ######
m_IntDiversityHabitat1000m <- fit_one(
  resp = "IntDiversitySiteYearSR",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

## 95% posterior support for positive relationship with grassland
m_IntDiversityHabitat500m <- fit_one(
  resp = "IntDiversitySiteYearSR",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_IntDiversityHabitat250m <- fit_one(
  resp = "IntDiversitySiteYearSR",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Unique Interactions ######

## 14 divergent transitions
## 95% posterior support for positive relationship with grassland
m_UniqIntHabitat1000m <- fit_one(
  resp = "UniqueIntSiteYearSR",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

## 2 divergent transitions
## 99% posterior support for positive relationship with grassland
m_UniqIntHabitat500m <- fit_one(
  resp = "UniqueIntSiteYearSR",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

## 96% posterior support for negative relationship with deciduous forest
## 95% posterior support for positive relationship with grassland
m_UniqIntHabitat250m <- fit_one(
  resp = "UniqueIntSiteYearSR",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

###### Niche Overlap ######
m_NicheOverlapHabitat1000m <- fit_one(
  resp = "NicheOverlap",
  data = SourceHabitat_1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_NicheOverlapHabitat500m <- fit_one(
  resp = "NicheOverlap",
  data = SourceHabitat_500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_NicheOverlapHabitat250m <- fit_one(
  resp = "NicheOverlap",
  data = SourceHabitat_250m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)
########################################################
## --       Model Selection and Power Analysis      --##
########################################################
#Change rhs to something meaningful
#BIC model selection, function gives score, summary function compares all
#Limitation - not many sites, little prairie remaining / study system
#Include power analysis in supplement
pyroHabitat <- SourceHabitat_500m |>
  left_join(
    pyro_500m |> select(Site, Year, SampleRound, VegDiversity, Pyrodiversity),
    by = c("Site", "Year", "SampleRound"))


fm <- "scale(Pyrodiversity) + 
          scale(VegDiversity) + 
          scale(grassland) +
         (1|Site)"

# m1 <- "scale(pyrodiv) + 
#           scale(VegDiversity) + 
#           scale(grassland) +
#           scale(deciduous_forest) +
#          (1|Site)"
# 
# m2 <- "scale(pyrodiv) + 
#           scale(VegDiversity) + 
#           scale(grassland) +
#          (1|Site)"
# 
# m3 <- "scale(pyrodiv) + 
#           scale(VegDiversity) + 
#          (1|Site)"

FullMod <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = pyroHabitat,
  rhs  = fm,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

# mod1 <- fit_one(
#   resp = "BeeFDivSiteYearSR",
#   data = landscapeWPA500m,
#   rhs  = m1,
#   family = skew_normal(),
#   pp_type = "dens_overlay"
# )
# 
# mod2 <- fit_one(
#   resp = "BeeFDivSiteYearSR",
#   data = landscapeWPA500m,
#   rhs  = m2,
#   family = skew_normal(),
#   pp_type = "dens_overlay"
# )
# 
# mod3 <- fit_one(
#   resp = "BeeFDivSiteYearSR",
#   data = landscapeWPA500m,
#   rhs  = m3,
#   family = skew_normal(),
#   pp_type = "dens_overlay"
# )

## Model comparison using Leave-One-Out Cross-Validation (LOO-CV)
## Model at top is best fit
## elpd_diff - The difference in expected log predictive density relative to the best model
## Higher elpd is better, so negative values mean worse than the top
## se_diff - standard error of the difference
## p_worse - The estimated probability that the model is worse than the top model, 
## based on a normal approximation to the elpd difference
## diag_diff - 
FullMod <- add_criterion(FullMod, "loo")

looFM <- loo(FullMod)
loom1 <- loo(mod1)
loom2 <- loo(mod2)
loom3 <- loo(mod3)

# the simplest model had the best predictive performance
# adding landscape covariates did not improve the predictive performance
loo_compare(loom3, loom2, loom1, looFM)

## Power analysis
library(powerbrmsINLA)
m_pyroFDiv
?brms_inla_power

powerBeeFDiv <- brms_inla_power(
  formula         = BeeFDivSiteYearSR ~ pyrodiv + VegDiversity + (1 | Site),
  family          = skew_normal(),
  effect_name     = "pyrodiv",
  effect_grid     = 0.03,   # true effect pm 1 SD of parameter
  sample_sizes    = 4,                            # number of observations
  obs_per_group   = 11,                            # observations per Site (67 / 6)
  predictor_means = list(pyrodiv = 0, VegDiversity = 0),
  predictor_sds   = list(pyrodiv = 1, VegDiversity = 1),   
  error_sd        = list(dist = "halfnormal", sd = 0.01, location = 0.11),
  group_sd        = list(dist = "halfnormal", sd = 0.02, location = 0.02),
  prob_threshold  = 0.95,      
  effect_threshold = 0,
  power_threshold = 0.80,
  nsims           = 100,
  seed            = 42
)

powerBeeFDis <- brms_inla_power(
  formula         = BeeFDisSiteYearSR ~ pyrodiv + VegDiversity + (1 | Site),
  family          = skew_normal(),
  effect_name     = "pyrodiv",
  effect_grid     = 0.01,   # true effect pm 1 SD of parameter
  sample_sizes    = 4,                            # number of observations
  obs_per_group   = 11,                            # observations per Site (67 / 6)
  predictor_means = list(pyrodiv = 0, VegDiversity = 0),
  predictor_sds   = list(pyrodiv = 1, VegDiversity = 1),   
  error_sd        = list(dist = "halfnormal", sd = 0.01, location = 0.12),
  group_sd        = list(dist = "halfnormal", sd = 0.02, location = 0.02),
  prob_threshold  = 0.95,      
  effect_threshold = 0,
  power_threshold = 0.80,
  nsims           = 100,
  seed            = 42
)

powerBeeAbund <- brms_inla_power(
  formula         = PollAbundance ~ scale(pyrodiv) + scale(VegDiversity) + (1 | Site),
  family          = skew_normal(),
  effect_name     = "pyrodiv",
  effect_grid     = 11.26,   # true effect pm 1 SD of parameter
  sample_sizes    = 4,                            # number of observations
  obs_per_group   = 11,                            # observations per Site (67 / 6)
  predictor_means = list(pyrodiv = 0, VegDiversity = 0),
  predictor_sds   = list(pyrodiv = 1, VegDiversity = 1),   
  error_sd        = list(dist = "halfnormal", sd = 4.94, location = 60.43),
  group_sd        = list(dist = "halfnormal", sd = 9.15, location = 11.14),
  prob_threshold  = 0.95,      
  effect_threshold = 0,
  power_threshold = 0.80,
  nsims           = 100,
  seed            = 42
)

powerBeeRich <- brms_inla_power(
  formula         = PollRichness ~ scale(pyrodiv) + scale(VegDiversity) + (1 | Site),
  family          = skew_normal(),
  effect_name     = "pyrodiv",
  effect_grid     = 2.66,   # true effect pm 1 SD of parameter
  sample_sizes    = 4,                            # number of observations
  obs_per_group   = 11,                            # observations per Site (67 / 6)
  predictor_means = list(pyrodiv = 0, VegDiversity = 0),
  predictor_sds   = list(pyrodiv = 1, VegDiversity = 1),   
  error_sd        = list(dist = "halfnormal", sd = 1.06, location = 12.23),
  group_sd        = list(dist = "halfnormal", sd = 2.51, location = 2.73),
  prob_threshold  = 0.95,      
  effect_threshold = 0,
  power_threshold = 0.80,
  nsims           = 100,
  seed            = 42
)

############################################################################ 
# Save Models for Later Use, Reload in
############################################################################ 

###### Pyrodiversity and Veg Diversity Models #######
models_pyro_1000m <- list(
  "Functional Dispersion"          = m_pyroFDis_1000m,
  "Functional Divergence"           = m_pyroFDiv_1000m,
  "Bee Diversity"     = m_pyroPollDiversity_1000m,
  "Bee Richness"      = m_pyroPollRichness_1000m,
  "Bee Abundance"      = m_pyroAbundance_1000m,
  "Interaction Richness" = m_pyroIntRichness_1000m,
  "Interaction Diversity" = m_pyroIntDiversity_1000m,
  "Unique Interactions" = m_pyroUniqInt_1000m,
  "Niche Overlap" = m_pyroNicheOverlap_1000m)

models_pyro_500m <- list(
  "Functional Dispersion"          = m_pyroFDis_500m,
  "Functional Divergence"           = m_pyroFDiv_500m,
  "Bee Diversity"     = m_pyroPollDiversity_500m,
  "Bee Richness"      = m_pyroPollRichness_500m,
  "Bee Abundance"      = m_pyroAbundance_500m,
  "Interaction Richness" = m_pyroIntRichness_500m,
  "Interaction Diversity" = m_pyroIntDiversity_500m,
  "Unique Interactions" = m_pyroUniqInt_500m,
  "Niche Overlap" = m_pyroNicheOverlap_500m)

models_pyro_250m <- list(
  "Functional Dispersion"          = m_pyroFDis_250m,
  "Functional Divergence"           = m_pyroFDiv_250m,
  "Bee Diversity"     = m_pyroPollDiversity_250m,
  "Bee Richness"      = m_pyroPollRichness_250m,
  "Bee Abundance"      = m_pyroAbundance_250m,
  "Interaction Richness" = m_pyroIntRichness_250m,
  "Interaction Diversity" = m_pyroIntDiversity_250m,
  "Unique Interactions" = m_pyroUniqInt_250m,
  "Niche Overlap" = m_pyroNicheOverlap_250m)

##### Source Habitat Models #######
models_habitat_1000m <- list(
  "Functional Dispersion"  = m_FDIsHabitat1000m,
  "Functional Divergence"  = m_FDIvHabitat1000m,
  "Bee Diversity"          = m_DiversityHabitat1000m,
  "Bee Richness"           = m_RichnessHabitat1000m,
  "Bee Abundance"          = m_AbundHabitat1000m,
  "Interaction Richness" = m_IntRichnessHabitat1000m,
  "Interaction Diversity" = m_IntDiversityHabitat1000m,
  "Unique Interactions" = m_UniqIntHabitat1000m,
  "Niche Overlap" = m_NicheOverlapHabitat1000m
)

models_habitat_500m <- list(
  "Functional Dispersion"  = m_FDIsHabitat500m,
  "Functional Divergence"  = m_FDIvHabitat500m,
  "Bee Diversity"          = m_DiversityHabitat500m,
  "Bee Richness"           = m_RichnessHabitat500m,
  "Bee Abundance"          = m_AbundHabitat500m,
  "Interaction Richness" = m_IntRichnessHabitat500m,
  "Interaction Diversity" = m_IntDiversityHabitat500m,
  "Unique Interactions" = m_UniqIntHabitat500m,
  "Niche Overlap" = m_NicheOverlapHabitat500m
)

##### Source Habitat Models #######
models_habitat_250m <- list(
  "Functional Dispersion"  = m_FDIsHabitat250m,
  "Functional Divergence"  = m_FDIvHabitat250m,
  "Bee Diversity"          = m_DiversityHabitat250m,
  "Bee Richness"           = m_RichnessHabitat250m,
  "Bee Abundance"          = m_AbundHabitat250m,
  "Interaction Richness" = m_IntRichnessHabitat250m,
  "Interaction Diversity" = m_IntDiversityHabitat250m,
  "Unique Interactions" = m_UniqIntHabitat250m,
  "Niche Overlap" = m_NicheOverlapHabitat250m
)

# Save pp_checks 
pp_types <- c("dens_overlay")

dir.create("figures/pp_checks/pisgah", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/pp_checks/pisgah/pyrodiversity/1000m", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/pp_checks/pisgah/pyrodiversity/500m", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/pp_checks/pisgah/pyrodiversity/250m", recursive = TRUE, showWarnings = FALSE)

dir.create("figures/pp_checks/pisgah/habitat/1000m", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/pp_checks/pisgah/habitat/500m", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/pp_checks/pisgah/habitat/250m", recursive = TRUE, showWarnings = FALSE)

savePPCheck(models_pyro_1000m,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/pisgah/pyrodiversity/1000m",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "Pyrodiversity1000m_")

savePPCheck(models_pyro_500m,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/pisgah/pyrodiversity/500m",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "Pyrodiversity500m_")

savePPCheck(models_pyro_250m,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/pisgah/pyrodiversity/250m",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "Pyrodiversity250m_")

savePPCheck(models_habitat_1000m,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/pisgah/habitat/1000m",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "habitat1000m_")

savePPCheck(models_habitat_500m,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/pisgah/habitat/500m",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "habitat500_")

savePPCheck(models_habitat_250m,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks/pisgah/habitat/250m",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "habitat250_")

## Save models to reload
dir.create("brms_models/pisgah", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/pyrodiversity", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/pyrodiversity/1000m", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/pyrodiversity/500m", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/pyrodiversity250m", recursive = TRUE, showWarnings = FALSE)

dir.create("brms_models/pisgah/habitat", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/habitat/1000m", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/habitat/500m", recursive = TRUE, showWarnings = FALSE)
dir.create("brms_models/pisgah/habitat/250m", recursive = TRUE, showWarnings = FALSE)

save_models_list(models_pyro_1000m,  "brms_models/pisgah/pyrodiversity/1000m")
save_models_list(models_pyro_500m,  "brms_models/pisgah/pyrodiversity/500m")
save_models_list(models_pyro_250m,  "brms_models/pisgah/pyrodiversity/250m")

save_models_list(models_pyro_1000m,  "brms_models/pisgah/habitat/1000m")
save_models_list(models_pyro_500m,  "brms_models/pisgah/habitat/500m")
save_models_list(models_pyro_250m,  "brms_models/pisgah/habitat/250m")


## Reload models if returning to script
pyro_files <- list.files("brms_models/pisgah/pyrodiversity/1000m", pattern = "\\.rds$", full.names = TRUE)
pyro_files <- list.files("brms_models/pisgah/pyrodiversity/500m", pattern = "\\.rds$", full.names = TRUE)
pyro_files <- list.files("brms_models/pisgah/pyrodiversity/250m", pattern = "\\.rds$", full.names = TRUE)


landscape_files <- list.files("brms_models/pisgah/habitat/1000m", pattern = "\\.rds$", full.names = TRUE)
landscape_files <- list.files("brms_models/pisgah/habitat/500m", pattern = "\\.rds$", full.names = TRUE)
landscape_files <- list.files("brms_models/pisgah/habitat/250m", pattern = "\\.rds$", full.names = TRUE)


models_pyro <- setNames(
  lapply(pyro_files, readRDS),
  tools::file_path_sans_ext(basename(pyro_files)))

models_landscape <- setNames(
  lapply(landscape_files, readRDS),
  tools::file_path_sans_ext(basename(landscape_files))
)

## Save results to CSV
dir.create("results/pisgah", showWarnings = FALSE, recursive = TRUE)

pyro_sigs <- summarise_fixed(models_pyro, "results/pisgah/fixed_effects_pyro.csv")

landscape_sigs <- summarise_fixed(models_landscape, "results/pisgah/fixed_effects_habitat.csv")

pyro_sigs <- read.csv("results/pisgah/fixed_effects_pyro.csv")

landscape_sigs <- read.csv('results/pisgah/fixed_effects_habitat.csv')

############################################################################ 
## Plotting 
############################################################################ 

dir.create("figures/pisgah/pyro/all_metrics", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/pisgah/habitat/all_metrics", showWarnings = FALSE, recursive = TRUE)

## Predictors to plot (raw scale; model applies log1p + scale internally
pyro_predictors <- list(
  pyrodiversity   = c("pyrodiv",    "Pyrodiversity"),
  VegDiversity = c("VegDiversity",  "Floral Diversity"))

landscape_predictors <- list(
  evergreen = c("evergreen_forest", "Evergreen Forest (ha)"),
  deciduous = c("deciduous_forest", "Deciduous Forest (ha)"),
  prairie   = c("grassland",        "Prairie (ha)")
)


# Loop over predictors and save one faceted plot per predictor
purrr::iwalk(pyro_predictors, function(xinfo, nm) {
  p <- plot_continuous(models_pyro, spec.net.SR, x = xinfo[[1]], xlab = xinfo[[2]])
  
  ggplot2::ggsave(
    filename = paste0("figures/pisgah/pyro/all_metrics/", nm, "_all_metrics.pdf"),
    plot     = p,
    width    = 10,
    height   = 6)})

purrr::iwalk(landscape_predictors, function(xinfo, nm) {
  p <- plot_continuous(models_landscape, landscapeWPA500m, x = xinfo[[1]], xlab = xinfo[[2]])
  
  ggplot2::ggsave(
    filename = paste0("figures/pisgah/landscape/all_metrics/", nm, "_all_metrics.pdf"),
    plot     = p,
    width    = 10,
    height   = 6)})

#Color blind safe
c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Plot pyrodiversity model
plotsPyro <- plotNiceModels(models_pyro,
                            sig_table    = pyro_sigs,
                            ncol         = 3,
                            focal_var    = "pyrodiv",
                            focal_scaled = "scale(pyrodiv)",
                            x_label      = "Pyrodiversity",
                            y_label      = c(
                              "Bee Abundance",
                              "Bee Diversity",
                              "Bee Richness",
                              "Functional Dispersion",
                              "Functional Divergence"),
                            lookup_term    = "scalepyrodiv",
                            sig_only_solid = TRUE,
                            colour         = "#E69F00")

# Plot Evergreen Forest
plotsEvergreen <- plotNiceModels(models_landscape, 
                                 sig_table    = landscape_sigs, 
                                 ncol         = 3,
                                 focal_var    = "evergreen_forest",           
                                 focal_scaled = "scale(evergreen_forest)",   
                                 x_label      = "Evergreen Forest (ha)",
                                 y_label = c(
                                   "Bee Abundance",
                                   "Bee Diversity",
                                   "Bee Richness",
                                   "Functional Dispersion",
                                   "Functional Divergence"),
                                 lookup_term  = "scaleevergreen_forest",
                                 sig_only_solid = TRUE,
                                 colour = "#009E73")

# Plot Deciduous Forest
plotsDeciduous <- plotNiceModels(models_landscape, 
                                 sig_table    = landscape_sigs, 
                                 ncol         = 3,
                                 focal_var    = "deciduous_forest",           
                                 focal_scaled = "scale(deciduous_forest)",   
                                 x_label      = "Deciduous Forest (ha)",
                                 y_label = c(
                                   "Bee Abundance",
                                   "Bee Diversity",
                                   "Bee Richness",
                                   "Functional Dispersion",
                                   "Functional Divergence"),
                                 lookup_term  = "scaledeciduous_forest",
                                 sig_only_solid = TRUE,
                                 colour = "#009E73")

# Plot Prairie
plotsPrairie <- plotNiceModels(models_landscape, 
                               sig_table    = landscape_sigs, 
                               ncol         = 3,
                               focal_var    = "grassland",           
                               focal_scaled = "scale(grassland)",   
                               x_label      = "Prairie (ha)",
                               y_label = c(
                                 "Bee Abundance",
                                 "Bee Diversity",
                                 "Bee Richness",
                                 "Functional Dispersion",
                                 "Functional Divergence"),
                               lookup_term  = "scaledeciduous_forest",
                               sig_only_solid = TRUE,
                               colour = "#009E73")



# Plot Vegetation Rarefied Richness
# plotsVeg <- plotNiceModels(models_pyro, 
#                     sig_table    = pyro_sigs, 
#                     ncol         = 2,
#                     focal_var    = "VegRarefiedRichness",           
#                     focal_scaled = "scale(VegRarefiedRichness)",   
#                     x_label      = "Floral RareFied Richness",
#                     y_label      = c(PyrodivFDis = "Functional Dispersion",
#                                      PyrodivFDiv = "Functional Divergence",
#                                      PyrodivFEve = "Functional Evenness"),
#                     lookup_term  = "scaleVegRarefiedRichness",
#                     sig_only_solid = TRUE,
#                     colour = "#009E73")


ggsave(filename = "PyroFDPlots.jpg", plot = plotsPyro, 
       path = "figures/pisgah/pyro/all_metrics/", width = 10, height = 6)

ggsave(filename = "Prairie.jpg", plot = plotsPrairie, 
       path = "figures/pisgah/landscape/all_metrics/", width = 10, height = 6)

ggsave(filename = "Evergreen.jpg", plot = plotsEvergreen, 
       path = "figures/pisgah/landscape/all_metrics/", width = 10, height = 6)

ggsave(filename = "Deciduous.jpg", plot = plotsDeciduous, 
       path = "figures/pisgah/landscape/all_metrics/", width = 10, height = 6)

# Subset
plot_models_pyrodiv(models_pyro, metrics = c("Pyrodivspec.netis", "PyrodivFEve"))

