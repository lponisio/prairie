rm(list=ls())

## relational database making
library(RSQLite)
library(tidyr)
library(readr)
library(glue)
library(lubridate)
library(stringr)


## specimen prep
library(igraph)
library(vegan)
library(fields)
library(fossil)
library(bipartite)
library(dplyr)
library(geosphere)


source("lab_paths.R")
local.path

## *****************************************************************
## create relational database, add species IDs
## *****************************************************************

setwd(local.path)
source('cascades-meadows/dataPrep/relational/1prep.R')

setwd(local.path)
source('cascades-meadows/dataPrep/relational/2make.R')

setwd(local.path)
source('cascades-meadows/dataPrep/relational/3join.R')

## *****************************************************************
## prep specimen data
## *****************************************************************
rm(list=ls()[ls() != "local.path"])

setwd(file.path(paste0(local.path,
                       "/cascades-meadows/dataPrep")))
# Create a folder for data
save.dir <- "../data"
if(!dir.exists(save.dir)) {
  dir.create(save.dir, showWarnings = FALSE)
}

spec <-
  read.csv("../../cascades-meadows_saved/data/relational/traditional/specimens-complete.csv",
           stringsAsFactors=FALSE)

## trait data

source("src/misc.R")
source("src/prepNets.R")
source("src/specialization.R")

spec$Date <- as.Date(spec$Date, format='%Y-%m-%d')
spec$Year <- format(spec$Date, "%Y")
spec$DoyStart<- as.numeric(strftime(spec$Date, format='%j'))
spec <- subset(spec, SampleRound != "SV")

## create a master sheet of conditions from the original data
surveys.master <-
  read.csv("../../cascades-meadows_saved/data/relational/original/conditions.csv")


condition.cols <-  c("Site", "SampleRound",
                                     "Plot", "Date", "StartTime",
                                     "EndTime",
                                     "TempStart", "TempEnd",
                                     "WindStart",
                                     "WindEnd")

surveys.master <- surveys.master[, condition.cols]

surveys.master$Date <- as.Date(surveys.master$Date,
                                    format='%Y-%m-%d')

surveys.master$DoyStart<-
  as.numeric(strftime(surveys.master$Date,
                      format='%j'))

surveys.master$Year <- format(surveys.master$Date, '%Y')
surveys.master <- subset(surveys.master, SampleRound != "SV")

## *****************************************************************
## Meadow proximity
## *****************************************************************

sites <- spec %>% 
  distinct(Site, Lat, Long, Area) 

# Compute the distance matrix (in meters)
dist_matrix <- distm(sites[, c("Long", "Lat")], fun = distHaversine)

# Convert meters to kilometers
dist_matrix_km <- dist_matrix / 1000

# Calculate mean distance for each point, excluding self-distance (diagonal)
mean_distances <- apply(dist_matrix_km, 1, function(x) mean(x[-which(x == 0)]))

# Add mean distances to the data frame
sites$Proximity <- mean_distances

surveys.master <- left_join(surveys.master, sites)

## *****************************************************************
## mean across Site, Sampling Round, Year
## *****************************************************************

surveys.master.round.year <- surveys.master  %>%
  group_by(Year, Site, SampleRound,
           Date, DoyStart, Area, Proximity
           ) %>%
  summarise(TempStart = mean(TempStart, na.rm = TRUE),
            TempEnd = mean(TempEnd, na.rm = TRUE),
            WindStart = mean(WindStart, na.rm = TRUE),
            WindEnd = mean(WindEnd, na.rm = TRUE))

## *****************************************************************
### prep spec data
## *****************************************************************
## fix plant names
spec$PlantGenusSpecies <- fix.white.space(paste(spec$PlantGenus,
                                                spec$PlantSpecies))
spec$PlantSpecies <- NULL

spec$SubSpecies[is.na(spec$SubSpecies)] <- ""
spec$GenusSpecies <- fix.white.space(paste(spec$Genus,
                                                spec$Species,
                                           spec$SubSpecies))
spec$SubSpecies <- NULL
spec$Species <- NULL

spec$PlantGenusSpecies[spec$PlantGenusSpecies == "NA NA"] <- ""
spec$GenusSpecies[spec$GenusSpecies == "NA NA"] <- ""

#save(spec, file="../data/spec_all.Rdata")
#write.csv(spec, file="../data/spec_all.csv", row.names=FALSE)


## *****************************************************************
## specimen-level parasite calculations
## *****************************************************************
source('parasite_positives.R')


crithidias <- c("CrithidiaExpoeki",
                "CrithidiaBombi", "CrithidiaSpp")
nosemas <- c("NosemaBombi", "NosemaCeranae")
parasites <- c( "AscosphaeraSpp",
               "ApicystisSpp", crithidias, nosemas)

blank.par <- data.frame(rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)),
                        rep(NA, nrow(spec)))

colnames(blank.par) <- c("Apidae", parasites)

spec <- cbind(spec, blank.par)

spec$Apidae[spec$UniqueID %in% Apidae] <- 1
spec[which(spec$Apidae == 1) , parasites] <- 0

spec$ApicystisSpp[spec$UniqueID %in% ApicystisSpp] <- 1
spec$AscosphaeraSpp[spec$UniqueID %in% AscosphaeraSpp] <- 1
spec$CrithidiaBombi[spec$UniqueID %in% CrithidiaBombi] <- 1
spec$CrithidiaExpoeki[spec$UniqueID %in% CrithidiaExpoeki] <- 1
spec$CrithidiaSpp[spec$UniqueID %in% CrithidiaSpp] <- 1
spec$NosemaBombi[spec$UniqueID %in% NosemaBombi] <- 1
spec$NosemaCeranae[spec$UniqueID %in% NosemaCeranae] <- 1

all.parasites <- list(ApicystisSpp, AscosphaeraSpp, CrithidiaBombi,
                      CrithidiaExpoeki, CrithidiaSpp, NosemaBombi,
                      NosemaCeranae)

apply(spec[, parasites],2, sum, na.rm=TRUE)

spec[, parasites][is.na(spec[, parasites])] <- 0
spec[, parasites][spec[, parasites] == ""] <- 0
spec[, parasites] <- apply(spec[, parasites], 2,  as.numeric)

spec[spec$Apidae != 1 | is.na(spec$Apidae), parasites] <- NA

spec$ParasiteRichness <- rowSums(spec[, parasites],
                                 na.rm=TRUE)
spec$CrithidiaRichness <- rowSums(spec[, crithidias],
                                  na.rm=TRUE)
spec$NosemaRichness <- rowSums(spec[, nosemas],
                                 na.rm=TRUE)
spec$PossibleParasite <- apply(spec[, parasites], 1,
                               function(x) sum(!is.na(x)))
spec$ParasitePresence <- (spec$ParasiteRichness >= 1)*1
spec$CrithidiaPresence <- (spec$CrithidiaRichness >= 1)*1
spec$NosemaPresence <- (spec$NosemaRichness >= 1)*1

spec[spec$Apidae != 1  | is.na(spec$Apidae), "ParasiteRichness"] <- NA
spec[spec$Apidae != 1  | is.na(spec$Apidae), "ParasitePresence"] <- NA
spec[spec$Apidae != 1  | is.na(spec$Apidae), "CrithidiaRichness"] <-
    NA
spec[spec$Apidae != 1  | is.na(spec$Apidae), "NosemaRichness"] <-
  NA
spec[spec$Apidae != 1  | is.na(spec$Apidae), "CrithidiaPresence"] <- NA
spec[spec$Apidae != 1  | is.na(spec$Apidae), "NosemaPresence"] <- NA

check.spec <- spec[!is.na(spec$Apidae),]
check.spec <- check.spec[check.spec$GenusSpecies == "",]
check.spec

## ***********************************************************************
## site/species level insect data
## ***********************************************************************

bee.families <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                  "Megachilidae")
site.sp <- spec %>%
  group_by(Site, Date, GenusSpecies) %>%
  summarise(Abundance = length(GenusSpecies),
            SpParasitismRate=mean(ParasitePresence, na.rm=TRUE))


## sum over site
site.sum <- spec[!is.na(spec$GenusSpecies),] %>%
  group_by(Site, Date, SampleRound) %>%
  summarise(PollAbundance = length(GenusSpecies),
            HBAbundance = sum(GenusSpecies == "Apis mellifera"),
            BombusAbundance = sum(Genus == "Bombus"),
            NonBombusHBAbundance =
              sum(Genus != "Bombus" & Genus != "Apis"),
            PollRichness= length(unique(GenusSpecies)),
            VisitedFloralRichness= length(unique(PlantGenusSpecies)),
            BombusRichness= length(unique(GenusSpecies[Genus ==
                                                       "Bombus"])),
            MeanParasiteRichness=mean(ParasiteRichness, na.rm=TRUE),
            PollDiversity=vegan:::diversity(table(GenusSpecies),
                                           index="shannon"),
            VisitedFloralDiversity=vegan:::diversity(
              table(PlantGenusSpecies),
              index="shannon"),
            BombusDiversity=vegan:::diversity(table(GenusSpecies[
              Genus == "Bombus"]),
              index="shannon"),
            SiteParasitismRate=mean(ParasitePresence, na.rm=TRUE),
            IndivScreened=length(!is.na(ParasitePresence)),
            SiteParasitism = sum(ParasitePresence, na.rm=TRUE),
            SiteCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
            SiteApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
            SiteNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
            SiteNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
            SiteAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
            SiteCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
            SiteCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
            SiteCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
            SiteScreened = sum(!is.na(Apidae)),
            HBSiteParasitismRate=mean(
              ParasitePresence[GenusSpecies == "Apis mellifera"],
              na.rm=TRUE),
            BombusSiteParasitismRate=mean(
              ParasitePresence[Genus == "Bombus"], na.rm=TRUE),
            BeeAbundance = length(GenusSpecies[Family %in%
                                                     bee.families]),
            
            BeeRichness= length(unique(GenusSpecies[Family %in%
                                                          bee.families])),
            
            BeeDiversity=vegan:::diversity(table(
              GenusSpecies[Family %in% bee.families]),
              index="shannon"))


col.mets <- c("PollAbundance",
            "HBAbundance",
            "BombusAbundance", 
            "NonBombusHBAbundance",
            "PollRichness",
            "VisitedFloralRichness",
            "BombusRichness",
            "MeanParasiteRichness",
            "PollDiversity", 
            "VisitedFloralDiversity", 
            "BombusDiversity", 
            "SiteParasitismRate", 
            "IndivScreened",
            "HBSiteParasitismRate", 
            "BombusSiteParasitismRate", 
            "BeeAbundance", 
            "BeeRichness",
            "BeeDiversity")

print(paste("number of surveys before summary stats merge",
            nrow(surveys.master.round.year)))
site.sum <- merge(surveys.master.round.year,
                      site.sum,
                     all.x=TRUE)

site.sum[, col.mets][is.na(site.sum[, col.mets])] <- 0

print(paste("number of surveys after", nrow(site.sum)))


net.par.sum <- spec[!is.na(spec$GenusSpecies),] %>%
  group_by(Site, Year, SampleRound, GenusSpecies) %>%
  summarise(SpSiteParasitismRate=mean(ParasitePresence, na.rm=TRUE),
    IndivScreened=sum(!is.na(ParasitePresence)),
    SpSiteCrithidiaRate=mean(CrithidiaPresence, na.rm=TRUE))

site.sp.yr <- spec[!is.na(spec$GenusSpecies),] %>%
  group_by(Site, Year, GenusSpecies) %>%
  summarise(Abundance = length(GenusSpecies))

## write species-level summary data
write.csv(site.sp.yr, file='../data/sp_year.csv', row.names=FALSE)

## write the site, year, sampling round summary data after merging
## with plant data


## ***********************************************************************
## For multi-study parasite summaries, a bit redundant but nice to
## have in a standardized format
## ***********************************************************************

bee.families <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                  "Megachilidae")


par.site.sp <- spec %>%
    group_by(Site, Year, SampleRound, GenusSpecies, Genus) %>%
    summarise(SpAbundance = length(GenusSpecies),
              SpParasitism = sum(ParasitePresence, na.rm=TRUE),
              SpCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
              SpApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
              SpNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
              SpNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
              SpAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
              SpCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
              SpCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
              SpCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
              SpScreened = sum(!is.na(Apidae))
              )

par.site.gen <- spec %>%
    group_by(Site, Year, SampleRound, Genus) %>%
    summarise(GenusAbundance = length(GenusSpecies),
              GenusParasitism = sum(ParasitePresence, na.rm=TRUE),
              GenusCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
              GenusApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
              GenusNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
              GenusNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
              GenusAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
              GenusCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
              GenusCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
              GenusCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
              GenusScreened = sum(!is.na(Apidae))
              )


par.site.sum <- spec %>%
    group_by(Site, Year, SampleRound) %>%
    summarise(SiteBeeAbundance = length(GenusSpecies[Family %in%
                                                     bee.families]),

              SiteBeeRichness= length(unique(GenusSpecies[Family %in%
                                                          bee.families])),

              SiteBeeDiversity=vegan:::diversity(table(
                                           GenusSpecies[Family %in% bee.families]),
                                           index="shannon"),

              SiteParasitism = sum(ParasitePresence, na.rm=TRUE),
              SiteCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
              SiteApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
              SiteNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
              SiteNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
              SiteAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
              SiteCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
              SiteCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
              SiteCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
              SiteScreened = sum(!is.na(Apidae))
              )

all.sums <- left_join(par.site.sp, par.site.gen)

all.sums <- left_join(all.sums, par.site.sum)
all.sums$Project <- "HJA"

save(all.sums, file="../data/HJA_parasite_sums.Rdata")

## Summary but without Sampling Round

par.year.sp <- spec %>%
  group_by(Site, Year, GenusSpecies, Genus) %>%
  summarise(SpAbundance = length(GenusSpecies),
            SpParasitism = sum(ParasitePresence, na.rm=TRUE),
            SpCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
            SpApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
            SpNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
            SpNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
            SpAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
            SpCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
            SpCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
            SpCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
            SpScreened = sum(!is.na(Apidae))
  )

par.year.gen <- spec %>%
  group_by(Site, Year, Genus) %>%
  summarise(GenusAbundance = length(GenusSpecies),
            GenusParasitism = sum(ParasitePresence, na.rm=TRUE),
            GenusCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
            GenusApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
            GenusNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
            GenusNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
            GenusAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
            GenusCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
            GenusCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
            GenusCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
            GenusScreened = sum(!is.na(Apidae))
  )

par.year.sum <- spec %>%
  group_by(Site, Year) %>%
  summarise(SiteBeeAbundance = length(GenusSpecies[Family %in%
                                                     bee.families]),
            
            SiteBeeRichness= length(unique(GenusSpecies[Family %in%
                                                          bee.families])),
            
            SiteBeeDiversity=vegan:::diversity(table(
              GenusSpecies[Family %in% bee.families]),
              index="shannon"),
            
            SiteParasitism = sum(ParasitePresence, na.rm=TRUE),
            SiteCrithidiaPresence= sum(CrithidiaPresence, na.rm=TRUE),
            SiteApicystisSpp = sum(ApicystisSpp, na.rm=TRUE),
            SiteNosemaBombi= sum(NosemaBombi, na.rm=TRUE),
            SiteNosemaCeranae = sum(NosemaCeranae, na.rm=TRUE),
            SiteAscosphaeraSpp= sum(AscosphaeraSpp, na.rm=TRUE),
            SiteCrithidiaExpoeki = sum(CrithidiaExpoeki, na.rm=TRUE),
            SiteCrithidiaBombi = sum(CrithidiaBombi, na.rm=TRUE),
            SiteCrithidiaSpp = sum(CrithidiaSpp, na.rm=TRUE),
            SiteScreened = sum(!is.na(Apidae))
  )

all.sums.siteYR <- left_join(par.year.sp, par.year.gen)

all.sums.siteYR <- left_join(all.sums.siteYR, par.year.sum)

save(all.sums.siteYR, file="../data/parasite_sums.Rdata")
## *******************************************************************
## create a giant plant-pollinator network to calculate specialization
## etc. across all sites
## *******************************************************************

## currently aggregates all of the localities, perhaps break up by

nets.all <- table(spec$PlantGenusSpecies, spec$GenusSpecies)

all.traits <- specieslevel(nets.all)
## calculate rarified plant.pol degree
rare.plants.degree <- apply(nets.all, 1, chao1)
rare.pols.degree <- apply(nets.all, 2, chao1)

traits <- data.frame(GenusSpecies= unlist(sapply(all.traits,
                                                 rownames)),
                     do.call(rbind, all.traits))

traits$r.degree <-  rare.pols.degree[match(traits$GenusSpecies,
                                           names(rare.pols.degree))]
traits$r.degree[is.na(traits$r.degree)] <-
  rare.plants.degree[match(traits$GenusSpecies[is.na(traits$r.degree)],
                           names(rare.plants.degree))]

rownames(traits) <- NULL

write.csv(traits, file='../data/network_traits.csv', row.names=FALSE)

## *******************************************************************
## create a giant pathogen-pollinator network to calculate
## specialization etc. across all sites
## *******************************************************************
agg.spec.sub <- spec[spec$Apidae == 1,]

agg.spec.para <- aggregate(agg.spec.sub[, parasites],
                           list(GenusSpecies=agg.spec.sub$GenusSpecies),
                           sum, na.rm=TRUE)

para.gensp.counts <- table(agg.spec.sub$GenusSpecies)

## proportion of individuals screened
agg.spec.para[, parasites] <- agg.spec.para[, parasites]/
  para.gensp.counts[agg.spec.para$GenusSpecies]

nets.para <- agg.spec.para
rownames(nets.para) <- nets.para$GenusSpecies
nets.para$GenusSpecies <- NULL

all.traits.para <- specieslevel(nets.para)
## calculate rarified plant.pol degree

para.traits <- data.frame(GenusSpecies= unlist(sapply(all.traits.para,
                                                      rownames)),
                          do.call(rbind, all.traits.para))

rare.pols.para.degree <- apply(nets.para, 1, chao1)
rare.para.degree <- apply(nets.para, 2, chao1)

para.traits$r.degree <-  rare.pols.para.degree[match(para.traits$GenusSpecies,
                                                     names(rare.pols.para.degree))]

para.traits$r.degree[is.na(para.traits$r.degree)] <-
  rare.para.degree[match(para.traits$GenusSpecies[is.na(para.traits$r.degree)],
                         names(rare.para.degree))]

rownames(para.traits) <- NULL

write.csv(traits, file='../data/parasite_network_traits.csv', row.names=FALSE)

## *******************************************************************
##  create site, sample round, year level networks for plant-pollinators,
##  pollinator-parasites
## *******************************************************************

dir.create("../data/networks", showWarnings = FALSE)
dir.create("../data/network_metrics", showWarnings = FALSE)
dir.create("../data/splevel_network_metrics", showWarnings = FALSE)

 
## plant-pollinator networks
nets.sr <- makeNets(spec, net.type="YearSR")
nets.yr <- makeNets(spec, net.type="Year")

## Bees only
nets.sr.bees <- makeNets(spec[spec$Family %in% bee.families,],
                         net.type="YearSR",
                         poll.groups="Bees")
nets.yr.bees <- makeNets(spec[spec$Family %in% bee.families,],
                         net.type="Year",
                          poll.groups="Bees")


dim(spec)
spec <- merge(spec, nets.yr[nets.yr$speciesType ==
                                      "higher.level",],
                  all.x=TRUE)
spec$SpSiteYear <- NULL
spec$speciesType <- NULL
dim(spec)

spec.sub <- agg.spec.sub %>%
  select(UniqueID, GenusSpecies, Site, Year, SampleRound,
         "AscosphaeraSpp",
         "ApicystisSpp", "CrithidiaExpoeki", "CrithidiaBombi",
         "NosemaBombi", "NosemaCeranae")

prep.para <- spec.sub %>%
  pivot_longer(cols=c("AscosphaeraSpp",
                      "ApicystisSpp", "CrithidiaExpoeki",
                      "CrithidiaBombi",
                      "NosemaBombi", "NosemaCeranae"),
               names_to = "Parasite", values_to = "count")
prep.para <- as.data.frame(prep.para)

par.net.sr <- makeNets(prep.para, net.type="YearSR", species=c("Pollinator",
                                                             "Parasite"),
                       lower.level="GenusSpecies",
                       higher.level="Parasite")

par.net.yr <- makeNets(prep.para, net.type="Year", species=c("Pollinator",
                                                           "Parasite"),
                       lower.level="GenusSpecies",
                       higher.level="Parasite",
                       mean.by.year=TRUE)

## *******************************************************************
##  rarefied richness
## *******************************************************************
## Note: This was for Jesse's data not necessary for HJA
## calculate and append rarefied bee abundance data
# all.species.rare <- makeRarefyComm(spec)
# bombus.species.rare <- makeRarefyComm(spec[spec$Genus == "Bombus",])
# colnames(bombus.species.rare)[4] <- "BombusRareRichness"
# 
# all.species.rare <- merge(all.species.rare, bombus.species.rare, all=TRUE)
# 
# site.sum <- merge(site.sum, all.species.rare, all.x=TRUE)
# site.sum$BombusRareRichness[is.na(site.sum$BombusRareRichness)] <- 0
# site.sum$RareRichness[is.na(site.sum$RareRichness)] <- 0


## *******************************************************************
##  Data checks
## *******************************************************************

print(paste("Pollinator species", length(unique(spec$GenusSpecies))))
print(paste("Plant species", length(unique(spec$PlantGenusSpecies))))
print(paste("Pollinator genera", length(unique(spec$Genus))))
print(paste("Specimens", nrow(spec)))

## *******************************************************************
##  merge specimen data with site summaries
## *******************************************************************

print(paste("number of specimens before parasite summary stats merge",
            nrow(spec)))
spec  <- merge(spec, net.par.sum, all.x=TRUE)
print(paste("number of specimens after parasite summary stats merge",
            nrow(spec)))

## preserves plots without any specimens caught!!
spec$SiteSampleRoundDate <- paste(spec$Site, spec$SampleRound,
                                 spec$Date)
sry.spec <- length(unique(spec$SiteSampleRoundDate))

site.sum$SiteSampleRoundDate <- paste(site.sum$Site,
                                      site.sum$SampleRound,
                                 site.sum$Date)
syr.sums <- length(unique(site.sum$SiteSampleRoundDate))

syr.sums-sry.spec

overlap.cols <- colnames(spec)[colnames(spec) %in% colnames(site.sum)]

overlap.cols <- overlap.cols[overlap.cols != "SiteSampleRoundDate"]

## this will mean weather is averaged across plots
spec[, overlap.cols] <- NULL

x <- dim(spec)
x
spec <- merge(spec, site.sum, all = TRUE, by="SiteSampleRoundDate")
z <- dim(spec)
z

spec$GenusSpecies[is.na(spec$GenusSpecies)] <- ""

print("sites with no specimens")
z[1]-x[1]
print("specimens with no ID, should be the same as above")
sum(spec$GenusSpecies == "")
print(length(unique(site.sum$SiteSampleRoundDate)))
print(length(unique(spec$SiteSampleRoundDate)))


write.csv(spec, file='../data/spec_net.csv',
          row.names=FALSE)

save(spec, file='../data/spec_net.Rdata')


## ***********************************************************************
## site/specimen level plant data
## ***********************************************************************

veg <-
  read.csv("../../cascades-meadows_saved/data/relational/traditional/veg-complete.csv")

## didn't count the exact number of flowers in 2022, use
## midpoints of bins
veg$NumBlooms <- veg$NumBlooms
veg$NumBlooms[veg$NumBlooms == "<10"] <- 5
veg$NumBlooms[veg$NumBlooms == "10-100"] <- 50
veg$NumBlooms[veg$NumBlooms == "100-1000"] <- 100

veg$NumBlooms <- as.numeric(veg$NumBlooms)

veg$PlantGenusSpecies <- fix.white.space(paste(veg$PlantGenus,
                                               veg$PlantSpecies))


print("veg missing IDs")
dim(veg[veg$PlantGenusSpecies == "",])

write.csv(veg[veg$PlantGenusSpecies == "",],
          file="../../cascades-meadows_saved/data/cleaning/veg_no_id.csv")

#veg.checked.plant.names <-
#  read.csv(file="../../cascades-meadows_saved/data/checkedfloralnames.csv")

#veg <- fixPlantNames(veg, "PlantGenusSpecies",
#                     veg.checked.plant.names)

veg$Date <- as.Date(veg$Date,  "%Y-%m-%d")
veg$Year <- format(veg$Date,  "%Y")

veg.blooming <- veg[veg$NumBlooms != "" &
                      veg$NumBlooms != "0",]


veg.sp.site <- veg[!is.na(veg$PlantGenusSpecies),] %>%
  group_by(Site, Year, SampleRound, PlantGenusSpecies) %>%
  summarise(SpAbund = sum(NumBlooms))

veg.site.sum <- veg.sp.site  %>%
  group_by(Site, SampleRound, Year) %>%
  summarise(VegAbundance =  sum(SpAbund),
            VegRichness= length(unique(PlantGenusSpecies)),
            VegDiversity=vegan:::diversity(SpAbund,
                                           index="shannon"))

## merge w master surveys 
col.mets <- c("VegAbundance",
              "VegRichness",
              "VegDiversity")

veg.site.sum <- merge(surveys.master.round.year,
                      veg.site.sum,
                      all.x=TRUE)

veg.site.sum[, col.mets][is.na(veg.site.sum[, col.mets])] <- 0

## merge mean transect and site level metrics together
# veg.site.sum <- merge(veg.site.sum.transect,
#                       veg.site.sum)



layout(matrix(1:3, nrow=3))
par(mar=c(1,1,1,1))
hist(veg.site.sum$VegAbundance)
hist(veg.site.sum$VegRichness)
hist(veg.site.sum$VegDiversity)

## *******************************************************************
##  rarefied richness veg
## *******************************************************************

## calculate and append rarefied bee abundance data
# veg.rare <- makeRarefyComm(veg,
#                                    higher.level="PlantGenusSpecies")
# 
# colnames(veg.rare)[4] <- "FlowerRareRichness"
# 
# veg.site.sum <- merge(veg.site.sum, veg.rare, all.x=TRUE)
# 
# veg.site.sum$FlowerRareRichness[is.na(veg.site.sum$FlowerRareRichness)] <- 0

## *******************************************************************

## merge to specimen data 
dim(spec)
spec <- merge(spec, 
                  veg.site.sum,
                  all.x=TRUE)
dim(spec)

spec[, col.mets][is.na(spec[, col.mets])] <- 0

write.csv(spec, file='../data/spec_net.csv',
          row.names=FALSE)

save(spec, file='../data/spec_net.Rdata')

