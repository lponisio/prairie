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


source("C:/lab_paths.R")
#source("~/lab_paths.R")
local.path

## *****************************************************************
## create relational database, add species IDs
## *****************************************************************

setwd(local.path)
source('prairie/dataPrep/relational/1prep.R')

setwd(local.path)
source('prairie/dataPrep/relational/2make.R')

setwd(local.path)
source('prairie/dataPrep/relational/3join.R')

## *****************************************************************
## prep specimen data
## *****************************************************************
rm(list=ls()[ls() != "local.path"])

setwd(file.path(paste0(local.path,
                       "/prairie/dataPrep")))
# Create a folder for data
save.dir <- "../data"
if(!dir.exists(save.dir)) {
  dir.create(save.dir, showWarnings = FALSE)
}

spec <-
  read.csv("../../prairie_saved/data/relational/traditional/specimens-complete.csv",
           stringsAsFactors=FALSE)

## trait data
source("src/misc.R")
source("src/prepNets.R")
source("src/specialization.R")

spec$Date <- as.Date(spec$Date, format='%Y-%m-%d')
spec$Year <- format(spec$Date, '%Y')
spec$DoyStart<- as.numeric(strftime(spec$Date, format='%j'))

## create a master sheet of conditions from the original data
surveys.master <-
  read.csv("../../prairie_saved/data/relational/original/conditions.csv")


condition.cols <-  c("Site", "SampleRound",
                    "Transect", "BurnUnburned", "Block",
                    "Date", "TimeStart",
                    "TimeEnd", "TempStart", "TempEnd",
                    "WindStart", "WindEnd")

surveys.master <- surveys.master[, condition.cols]

surveys.master$Date <- as.Date(surveys.master$Date,
                                    format='%Y-%m-%d')

surveys.master$DoyStart<-
  as.numeric(strftime(surveys.master$Date,
                      format='%j'))

surveys.master$Year <- format(surveys.master$Date, '%Y')

## *****************************************************************
## mean across Site, Sampling Round, Year
## *****************************************************************

surveys.master.round.year <- surveys.master  %>%
  summarise(TempStart = mean(TempStart, na.rm = TRUE),
            TempEnd = mean(TempEnd, na.rm = TRUE),
            WindStart = mean(WindStart, na.rm = TRUE),
            WindEnd = mean(WindEnd, na.rm = TRUE), 
            .by = c(Year, Site, SampleRound))

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

save(spec, file="../data/spec_all.Rdata")
write.csv(spec, file="../data/spec_all.csv", row.names=FALSE)

## ***********************************************************************
## site/species/sampleround level insect data
## ***********************************************************************

bee.families <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                  "Megachilidae")

## Site, year level
site.sp.yr <- spec[!is.na(spec$GenusSpecies),] %>%
  summarise(Abundance = length(GenusSpecies),
            .by = c(Site, Year, GenusSpecies))

## Site, year, sampleround level
site.sp.yr.sr <- spec %>%
  summarise(Abundance = length(GenusSpecies),
            .by = c(Site, Year, SampleRound, GenusSpecies))

site.sum <- spec[!is.na(spec$GenusSpecies),] %>%
  summarise(PollAbundance = length(GenusSpecies),
            HBAbundance = sum(GenusSpecies == "Apis mellifera"),
            BombusAbundance = sum(Genus == "Bombus"),
            NonBombusHBAbundance =
              sum(Genus != "Bombus" & Genus != "Apis"),
            PollRichness= length(unique(GenusSpecies)),
            VisitedFloralRichness= length(unique(PlantGenusSpecies)),
            BombusRichness= length(unique(GenusSpecies[Genus ==
                                                       "Bombus"])),
            PollDiversity=vegan:::diversity(table(GenusSpecies),
                                           index="shannon"),
            VisitedFloralDiversity=vegan:::diversity(
              table(PlantGenusSpecies),
              index="shannon"),
            BombusDiversity=vegan:::diversity(table(GenusSpecies[
              Genus == "Bombus"]),
              index="shannon"),
            BeeAbundance = length(GenusSpecies[Family %in%
                                                     bee.families]),
            
            BeeRichness= length(unique(GenusSpecies[Family %in%
                                                          bee.families])),
            
            BeeDiversity=vegan:::diversity(table(
              GenusSpecies[Family %in% bee.families]),
              index="shannon"),
            .by = c(Site, Year, SampleRound))


col.mets <- c("PollAbundance",
            "HBAbundance",
            "BombusAbundance", 
            "NonBombusHBAbundance",
            "PollRichness",
            "VisitedFloralRichness",
            "PollDiversity", 
            "VisitedFloralDiversity",
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

## write species-level summary data
write.csv(site.sp.yr, file='../data/sp_year.csv', row.names=FALSE)
write.csv(site.sp.yr.sr, file='../data/sp_year_sr.csv', row.names=FALSE)

## *******************************************************************
## create a giant plant-pollinator network to calculate specialization
## etc. across all sites
## *******************************************************************

## Calculate network properties
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
##  create site, sample round, year level networks for plant-pollinators
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
spec <- merge(spec, nets.sr.bees[nets.sr.bees$speciesType ==
                                      "higher.level",],
                  all.x=TRUE)
spec$SpSiteYear <- NULL
spec$speciesType <- NULL
dim(spec)

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

## preserves plots without any specimens caught!!
spec$SiteSRYear <- paste(spec$Site, spec$SampleRound,
                                 spec$Year)
sry.spec <- length(unique(spec$SiteSRYear))

site.sum$SiteSRYear <- paste(site.sum$Site,
                                      site.sum$SampleRound,
                                 site.sum$Year)
syr.sums <- length(unique(site.sum$SiteSRYear))

syr.sums-sry.spec

overlap.cols <- colnames(spec)[colnames(spec) %in% colnames(site.sum)]

overlap.cols <- overlap.cols[overlap.cols != "SiteSRYear"]

## this will mean weather is averaged across plots
spec[, overlap.cols] <- NULL

x <- dim(spec)
x
spec <- merge(spec, site.sum, all = TRUE, by="SiteSRYear")
z <- dim(spec)
z

spec$GenusSpecies[is.na(spec$GenusSpecies)] <- ""

print("sites with no specimens")
z[1]-x[1]
print("specimens with no ID, should be the same as above")
sum(spec$GenusSpecies == "")
print(length(unique(site.sum$SiteSRYear)))
print(length(unique(spec$SiteSRYear)))


write.csv(spec, file='../data/spec_net.csv',
          row.names=FALSE)

save(spec, file='../data/spec_net.Rdata')


## ***********************************************************************
## site/specimen level plant data
## ***********************************************************************

veg <-
  read.csv("../../prairie_saved/data/relational/traditional/veg-complete.csv")

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
          file="../../prairie_saved/data/cleaning/veg_no_id.csv")

#veg.checked.plant.names <-
#  read.csv(file="../../prairie_saved/data/checkedfloralnames.csv")

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

