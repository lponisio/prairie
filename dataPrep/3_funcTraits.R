rm(list=ls())
library(FD)
library(vegan)
library(readxl)
library(ape)

setwd('C:/')
source("lab_paths.R")
local.path

library(tidyverse)
dir.bombus <- file.path(local.path, "prairie/dataPrep")
setwd(dir.bombus)

## calculate trait uniqueness and originality based on Coux et
## al. 2016 for each site-year-round

source("src/calcFuncUniqOrig.R")
#source("src/misc.R")
load('../data/spec_net.Rdata')
traits <-
  read_csv("../../prairie_saved/data/raw/pisgah_bee_traits.csv")

## only working with bees
bee.families <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                  "Megachilidae")

spec.net <- spec.net[spec.net$Family %in% bee.families,]

#View missing species from data
lost.species <- setdiff(spec.net$GenusSpecies, traits$GenusSpecies)
print("Missing species from observed data")
sort(lost.species)

#Subset only observed species
traits <- traits[traits$GenusSpecies %in% spec.net$GenusSpecies,]
traits <- as.data.frame(traits)

bee.traits <- c("NestLocation", 
                "NestConstruction", "NestPartition",
                "NestLining", "PrimaryNestMaterial", 
                "Lecty", "Sociality", "MeanITD", 
                "PollenCarry", "d")


#traits$Proboscis <- as.factor(traits$Proboscis)
traits$Sociality <- as.factor(traits$Sociality)
traits$NestLocation <- as.factor(traits$NestLocation)
traits$NestConstruction <- as.factor(traits$NestConstruction)
traits$NestPartition <- as.factor(traits$NestPartition)
traits$NestLining <- as.factor(traits$NestLining)
traits$PrimaryNestMaterial <- as.factor(traits$PrimaryNestMaterial)
traits$Lecty <- as.factor(traits$Lecty)
traits$PollenCarry <- as.factor(traits$PollenCarry)

traits[traits == ""] <- NA

#Species as row names
species_names <- traits$GenusSpecies
traits <- traits %>% 
  column_to_rownames("GenusSpecies")

#Check rownames
rownames(traits) 

## We grouped the traits based on what they discuss. so for example, we
## have 3 nesting traits, so weighted each one by 1/3.
bee.weights <- c(rep(1/5, 5), rep(1, 5))
length(bee.weights) == length(bee.traits)

print("\nUnique values per trait:")
lapply(traits[, bee.traits], unique)
colSums(is.na(traits[, bee.traits]))

#To find which axes capture the most variation, run PCoA
#trait distance matrix and inspect eigenvalues:
#Check unique categorical trait combos
# traits[, bee.traits] %>%
   # select(NestLocation, NestConstruction, NestLining, NestPartition, Sociality, MeanITD, d) %>%
   # group_by(NestLocation, NestConstruction, NestLining, NestPartition,  Sociality, MeanITD, d) %>%
   # summarise(n_species = n(), .groups = "drop") %>%
   # arrange(desc(n_species))

# #Build distance matrix from traits
 # trait.dist <- gowdis(traits[, bee.traits],
                                   # w = bee.weights)

 # #Run PCoA
 # pcoa.res <- pcoa(trait.dist, correction = "lingoes")
 # 
 # # Quick summary: cumulative variance for first N axes
 # eig_table <- pcoa.res$values
 # n_axes  <- min(25, nrow(eig_table))
 # cum_var <- cumsum(eig_table$Rel_corr_eig[1:n_axes])
 # message(paste0("\nCumulative variance explained (first ", n_axes, " axes):"))
 # print(data.frame(
 #   Axis     = seq_len(n_axes),
 #   Corr_eig = round(eig_table$Corr_eig[1:n_axes], 4),
 #   Pct_var  = round(eig_table$Rel_corr_eig[1:n_axes] * 100, 2),
 #   Cum_pct  = round(cum_var * 100, 2)
 # ))

#############################
# Use for Euclidean approach
#############################
 
# # Standardize both traits
# traits_scaled <- traits
# traits_scaled$MeanITD <- scale(traits$MeanITD)
# traits_scaled$d       <- scale(traits$d)
# 
# # Remove NAs first
# traits_complete <- traits_scaled[complete.cases(traits_scaled[, c("MeanITD", "d")]), ]
# 
# # Euclidean distance
# trait.dist.euc <- dist(traits_complete[, c("MeanITD", "d")], method = "euclidean")
# 
# # PCoA
# pcoa.euc <- pcoa(trait.dist.euc, correction = "none")
# eig_euc  <- pcoa.euc$values
# 
# n <- nrow(eig_euc)
# print(data.frame(
#   Axis    = seq_len(nrow(eig_euc)),
#   Pct_var = round(eig_euc$Relative_eig * 100, 2),
#   Cum_pct = round(eig_euc$Cumul_eig * 100, 2)
# ))


# make the three IDs once
spec.net <- spec.net %>%
  tidyr::unite("SiteYearSR", Site, Year, SampleRound, sep = ";", remove = FALSE) %>%
  tidyr::unite("SiteYear",   Site, Year,           sep = ";", remove = FALSE)

# run all three with one pattern
spec.net <- add_func_uniq_orig(spec.net, traits,
                               id_col = "Site",
                               traits.2.keep = bee.traits, weights = bee.weights,
                               suffix = "Site",
                               add_fd = TRUE)

spec.net <- add_func_uniq_orig(spec.net, traits,
                               id_col = "SiteYearSR",
                               traits.2.keep = bee.traits, weights = bee.weights,
                               suffix = "SiteYearSR",
                               add_fd = TRUE)

spec.net <- add_func_uniq_orig(spec.net, traits,
                               id_col = "SiteYear",
                               traits.2.keep = bee.traits, weights = bee.weights,
                               suffix = "SiteYear",
                               add_fd = TRUE)

hist(spec.net$BeeFDivSiteYearSR)
hist(spec.net$BeeFDisSiteYearSR)
hist(spec.net$BeeFEveSiteYearSR)

save(spec.net, file="../data/spec_traits.Rdata")
write.csv(spec.net, file="../data/spec_traits.csv", row.names=FALSE)

# Merges network traits by site and round to spec.net
# load('../data/sp_network_mets_sr_pretty.RData')
# 
# sp.network.metrics <- sp.network.metrics %>%
#   select(GenusSpecies, Site, Year, SampleRound, zdegree,
#          zweighted.betweenness, zweighted.closeness, zd,
#          normalised.degree)
# 
# traits$GenusSpecies <- rownames(traits)
# rownames(traits) <- NULL
# 
# dim(spec.net)
# spec.net <- merge(spec.net, sp.network.metrics, all.x=TRUE)
# dim(spec.net)
# spec.net <- merge(spec.net, traits, all.x=TRUE)
# dim(spec.net)
# 
# save(spec.net, file="../data/spec_traits.Rdata")
# write.csv(spec.net, file="../data/spec_traits.csv", row.names=FALSE)

# ## Merges network traits by year to spec.net
# load('../data/sp_network_mets_Year_pretty.RData')
#  
# sp.network.metrics <- sp.network.metrics %>%
#   select(GenusSpecies, Site, Year, SampleRound, zdegree,
#       zweighted.betweenness, zweighted.closeness, zd,
#           normalised.degree)
# 
# traits$GenusSpecies <- rownames(traits)
# rownames(traits) <- NULL
# 
# dim(spec.net)
# spec.net <- merge(spec.net, sp.network.metrics, all.x=TRUE)
# dim(spec.net)
# spec.net <- merge(spec.net, traits, all.x=TRUE)
# dim(spec.net)
# 
#   
# save(spec.net, file="../data/spec_traits_YR.Rdata")
# write.csv(spec.net, file="../data/spec_traits_YR.csv", row.names=FALSE)

#spec.bee <- spec.net %>%
#  filter(Family != "Syrphidae") %>%
#  select(UniqueID, TempID, GenusSpecies, Site, Year, SampleRound,
 #        PlantGenusSpecies, NestLocation, NestPartitions,
  #       NestConstruction, PrimaryNestMaterial, MeanITD, PollenCarry,
   #      ReproStrat, Sociality,
    #     originalitySiteYearSR, uniqSiteYearSR,
     #    originalitySiteYear, uniqSiteYear,
      #   originalitySite, uniqSite,
       #  zdegree,
        # zweighted.betweenness, zweighted.closeness, zd,
         #normalised.degree)


#write.csv(spec.bee.microbes,
#          file="../data/bee_FD_traits.csv", row.names=FALSE)
