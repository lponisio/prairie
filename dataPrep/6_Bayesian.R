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
pyrodiversity <- read.csv('../data/pyrodiversity/pisgahBurnResults.csv')

source("src/fit_model.R")
source("src/save_models.R")
source("src/plot_models.R")
source("src/summarize_models.R")
source("src/plot_flexible.R")

pyrodiversity$Year <- as.factor(pyrodiversity$Year)
pyrodiversity$Site <- as.factor(pyrodiversity$Site)

spec.net$SampleRound <- as.factor(spec.net$SampleRound)
spec.net$Site <- as.factor(spec.net$Site)
spec.net$Year <- as.factor(spec.net$Year)

## Site + Year + Sampleround level ##
spec.net.SR <- spec.net %>% 
  left_join(climate, by = c("Site", "Year", "SampleRound")) %>% 
  left_join(pyrodiversity, by = c("Site", "Year")) %>% 
  group_by(Site, Year, SampleRound) %>% 
  summarise(
    BeeFDivSiteYearSR = first(BeeFDivSiteYearSR),
    BeeFDisSiteYearSR = first(BeeFDisSiteYearSR),
    BeeRichness = first(BeeRichness),
    BeeDiversity = first(BeeDiversity),
    BeeAbundance = first(BeeAbundance),
    VegDiversity = first(VegDiversity),
    pyrodiv = first(shannon),
    logBeeFDivSiteYearSR = log(BeeFDivSiteYearSR),
    logBeeFDisSiteYearSR = log(BeeFDisSiteYearSR)) 

#####################################################################
# Pyrodiversity
######################################################
pyro <- c("scale(pyrodiv) + 
          scale(VegDiversity) + 
          (1|Site)")

m_pyroFDis <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = spec.net.SR,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

# moderate evidence
m_pyroFDiv <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = spec.net.SR,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_pyroPollDiversity <- fit_one(
  resp = "BeeDiversity",
  data = spec.net.SR,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

#moderate evidence
m_pyroPollRichness <- fit_one(
  resp = "BeeRichness",
  data = spec.net.SR,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

#moderate evidence
m_pyroAbundance <- fit_one(
  resp = "BeeAbundance",
  data = spec.net.SR,
  rhs  = pyro,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

##################################################################################
# -- Functional diversity as a function of land cover weighted proximity area -- #
##################################################################################
load('../data/landcover/LandscapeMetricsResults.RData')
landscapeWPA <- left_join(landscapeWPA, spec.net.SR, by = c("Site", "Year"),
                         relationship = "many-to-many")

landscapeWPA500m <- landscapeWPA %>% 
  filter(Decay_Rate == 500)

landscapeWPA1000m <- landscapeWPA %>% 
  filter(Decay_Rate == 1000)

landscapeWPA1500m <- landscapeWPA %>% 
  filter(Decay_Rate == 1500)

landscape <- "scale(deciduous_forest) +
              scale(evergreen_forest) +
              scale(grassland) +
              (1|Year) + (1|Site)"

 # landscapeWPA %>% 
#   ggplot(aes(x = evergreen_forest, y = BeeFDisSiteYear)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_wrap(~ Year + Decay_Rate)

# Dispersion
m_FDIsLandscape1500m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = landscapeWPA1500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIsLandscape1000m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = landscapeWPA1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIsLandscape500m <- fit_one(
  resp = "BeeFDisSiteYearSR",
  data = landscapeWPA500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)


# Divergence
m_FDIvLandscape1500m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA1500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_FDIvLandscape1000m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

#Weak posterior support
m_FDIvLandscape500m <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

# Diversity
m_DiversityLandspape1500m <- fit_one(
  resp = "BeeDiversity",
  data = landscapeWPA1500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_DiversityLandspape1000m <- fit_one(
  resp = "BeeDiversity",
  data = landscapeWPA1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_DiversityLandspape500m <- fit_one(
  resp = "BeeDiversity",
  data = landscapeWPA500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

# Richness
m_RichLandspape1500m <- fit_one(
  resp = "BeeRichness",
  data = landscapeWPA1500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_RichLandspape1000m <- fit_one(
  resp = "BeeRichness",
  data = landscapeWPA1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_RichLandspape500m <- fit_one(
  resp = "BeeRichness",
  data = landscapeWPA500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

# Abundance
m_AbundLandspape1500m <- fit_one(
  resp = "BeeAbundance",
  data = landscapeWPA1500m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_AbundLandspape1000m <- fit_one(
  resp = "BeeAbundance",
  data = landscapeWPA1000m,
  rhs  = landscape,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

m_AbundLandspape500m <- fit_one(
  resp = "BeeAbundance",
  data = landscapeWPA500m,
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

fm <- "scale(pyrodiv) + 
          scale(VegDiversity) + 
          scale(grassland) +
          scale(evergreen_forest) +
          scale(deciduous_forest) +
         (1|Site)"

m1 <- "scale(pyrodiv) + 
          scale(VegDiversity) + 
          scale(grassland) +
          scale(deciduous_forest) +
         (1|Site)"

m2 <- "scale(pyrodiv) + 
          scale(VegDiversity) + 
          scale(grassland) +
         (1|Site)"

m3 <- "scale(pyrodiv) + 
          scale(VegDiversity) + 
         (1|Site)"

FullMod <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA500m,
  rhs  = fm,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

mod1 <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA500m,
  rhs  = m1,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

mod2 <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA500m,
  rhs  = m2,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

mod3 <- fit_one(
  resp = "BeeFDivSiteYearSR",
  data = landscapeWPA500m,
  rhs  = m3,
  family = skew_normal(),
  pp_type = "dens_overlay"
)

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

models_pyro <- list(
  "Functional Dispersion"          = m_pyroFDis,
  "Functional Divergence"           = m_pyroFDiv,
  "Bee Diversity"     = m_pyroPollDiversity,
  "Bee Richness"      = m_pyroPollRichness,
  "Bee Abundance"      = m_pyroAbundance)

models_landscape <- list(
  "Functional Dispersion"  = m_FDIsLandscape500m,
  "Functional Divergence"  = m_FDIvLandscape500m,
  "Bee Diversity"          = m_DiversityLandspape500m,
  "Bee Richness"           = m_RichLandspape500m,
  "Bee Abundance"          = m_AbundLandspape500m)

# Save pp_checks 
pp_types <- c("dens_overlay")

dir.create("figures/pp_checks", recursive = TRUE, showWarnings = FALSE)

savePPCheck(models_pyro,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "Pyrodiversity")

savePPCheck(models_landscape,
            pp_types = "dens_overlay",
            out_dir  = "figures/pp_checks",
            ndraws   = 1000,
            width    = 12,
            height   = 6,
            prefix = "Landscape")

## Save models to reload
save_models_list(models_pyro,  "brms_models/pyrodiversity")
save_models_list(models_landscape, "brms_models/landscape")

## Reload models if returning to script
pyro_files <- list.files("brms_models/pyrodiversity", pattern = "\\.rds$", full.names = TRUE)
landscape_files <- list.files("brms_models/landscape", pattern = "\\.rds$", full.names = TRUE)

models_pyro <- setNames(
  lapply(pyro_files, readRDS),
  tools::file_path_sans_ext(basename(pyro_files)))

models_landscape <- setNames(
  lapply(landscape_files, readRDS),
  tools::file_path_sans_ext(basename(landscape_files))
)


## Save results to CSV
dir.create("results", showWarnings = FALSE, recursive = TRUE)

pyro_sigs <- summarise_fixed(models_pyro, "results/fixed_effects_pyro.csv")

landscape_sigs <- summarise_fixed(models_landscape, "results/fixed_effects_landscape.csv")

pyro_sigs <- read.csv("results/fixed_effects_pyro.csv")

landscape_sigs <- read.csv('results/fixed_effects_landscape.csv')

############################################################################ 
## Plotting 
############################################################################ 

dir.create("figures/pyro/all_metrics", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/landscape/all_metrics", showWarnings = FALSE, recursive = TRUE)

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
    filename = paste0("figures/pyro/all_metrics/", nm, "_all_metrics.pdf"),
    plot     = p,
    width    = 10,
    height   = 6)})

purrr::iwalk(landscape_predictors, function(xinfo, nm) {
  p <- plot_continuous(models_landscape, landscapeWPA500m, x = xinfo[[1]], xlab = xinfo[[2]])
  
  ggplot2::ggsave(
    filename = paste0("figures/landscape/all_metrics/", nm, "_all_metrics.pdf"),
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
       path = "figures/pyro/all_metrics/", width = 10, height = 6)

ggsave(filename = "Prairie.jpg", plot = plotsPrairie, 
       path = "figures/landscape/all_metrics/", width = 10, height = 6)

ggsave(filename = "Evergreen.jpg", plot = plotsEvergreen, 
       path = "figures/landscape/all_metrics/", width = 10, height = 6)

ggsave(filename = "Deciduous.jpg", plot = plotsDeciduous, 
       path = "figures/landscape/all_metrics/", width = 10, height = 6)

# Subset
plot_models_pyrodiv(models_pyro, metrics = c("Pyrodivspec.netis", "PyrodivFEve"))

