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

## Fix misspellings and string inconsistencies 
spec <- spec |>
  filter(!is.na(PlantGenusSpecies), !PlantGenusSpecies %in% c("#N/A", "")) |>
  mutate(
    PlantGenusSpecies = PlantGenusSpecies |>
      stringr::str_squish() |>
      stringr::str_to_sentence() |>
      recode("Bellardia viscosa"       = "Parentucellia viscosa",
             "Rubis armeniacus"        = "Rubus armeniacus",
             "Jacobaea vulgaris"       = "Senecio jacobaea",
             "Toxicoscordion venenosm" = "Toxicoscordion venenosum",
             "Geranium pusillium"      = "Geranium pusillum"),
    GenusSpecies = GenusSpecies |>
      stringr::str_squish() |>
      # standardize morphospecies labels, keeping the number: "Sp.1" / "Sp. 2" -> "sp. 1" / "sp. 2"
      stringr::str_replace("\\s[Ss]p\\.?\\s*(\\d+)$", " sp. \\1") |>
      # lump "cf." identifications with the named species
      stringr::str_replace("\\scf\\.\\s", " ") |>
      stringr::str_replace("Utahense", "utahense")
  )

save(spec, file="../data/spec_all.Rdata")
write.csv(spec, file="../data/spec_all.csv", row.names=FALSE)

## ***********************************************************************
## site/species/sample round level bee data
## ***********************************************************************

bee.families <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                  "Megachilidae")
print("Dim with all insect data")
dim(spec)

spec <- spec %>% 
  filter(Family %in% bee.families)
print("Dim with only beet data")
dim(spec)

## Site, year level
site.sp.yr <- spec[!is.na(spec$GenusSpecies),] %>%
  summarise(Abundance = length(GenusSpecies),
            .by = c(Site, Year, GenusSpecies))

## Site, year, sampleround level
site.sp.yr.sr <- spec %>%
  summarise(Abundance = length(GenusSpecies),
            .by = c(Site, Year, SampleRound, GenusSpecies))

site.sum <- spec[!is.na(spec$GenusSpecies),] %>%
  summarise(HBAbundance = sum(GenusSpecies == "Apis mellifera"),
            BombusAbundance = sum(Genus == "Bombus"),
            NonBombusHBAbundance =
              sum(Genus != "Bombus" & Genus != "Apis"),
            VisitedFloralRichness= length(unique(PlantGenusSpecies)),
            BombusRichness= length(unique(GenusSpecies[Genus ==
                                                       "Bombus"])),
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


col.mets <- c("HBAbundance",
            "BombusAbundance", 
            "NonBombusHBAbundance",
            "VisitedFloralRichness",
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
dim(spec)
nets.sr.bees <- makeNets(spec[spec$Family %in% bee.families, ],
                         net.type = "YearSR", poll.groups = "Bees")
sp.lev  <- nets.sr.bees$sp.lev
net.lev <- nets.sr.bees$net.lev

#species-level
spec <- merge(spec,
              sp.lev[sp.lev$speciesType == "higher.level", ],
              by = c("Site", "Year", "SampleRound", "GenusSpecies"),
              all.x = TRUE)
spec$SpSiteYear <- NULL
spec$speciesType <- NULL

spec <- spec |> 
  rename(niche.overlap.SpLevel = niche.overlap)

# network-level
spec$SampleRound <- as.character(spec$SampleRound)
net.lev$SampleRound <- as.character(net.lev$SampleRound)
spec <- merge(spec, net.lev,
              by = c("Site", "Year", "SampleRound"),
              all.x = TRUE)

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


veg.sp.site <- veg[!is.na(veg$PlantGenusSpecies),] %>%
  summarise(SpAbund = length(PlantGenusSpecies),
            .by = c(Site, Year, SampleRound, PlantGenusSpecies))

veg.site..yr.sr.sum <- veg.sp.site  %>%
  summarise(VegAbundance =  sum(SpAbund),
            VegRichness= length(unique(PlantGenusSpecies)),
            VegDiversity=vegan:::diversity(SpAbund,
                                           index="shannon"),
            .by = c(Site, SampleRound, Year))

## merge w master surveys 
col.mets <- c("VegAbundance",
              "VegRichness",
              "VegDiversity")

veg.site..yr.sr.sum <- merge(surveys.master.round.year,
                             veg.site..yr.sr.sum,
                      all.x=TRUE)

veg.site..yr.sr.sum[, col.mets][is.na(veg.site..yr.sr.sum[, col.mets])] <- 0



layout(matrix(1:3, nrow=3))
par(mar=c(1,1,1,1))
hist(veg.site..yr.sr.sum$VegAbundance)
hist(veg.site..yr.sr.sum$VegRichness)
hist(veg.site..yr.sr.sum$VegDiversity)

## *******************************************************************
##  rarefied richness veg
## *******************************************************************

# calculate and append rarefied floral abundance data
veg.rare <- makeRarefyComm(veg,
                                   higher.level="PlantGenusSpecies")

colnames(veg.rare)[3] <- "FlowerRareRichness"

veg.site..yr.sr.sum <- merge(veg.site..yr.sr.sum, veg.rare, all.x=TRUE)

veg.site..yr.sr.sum$FlowerRareRichness[is.na(veg.site..yr.sr.sum$FlowerRareRichness)] <- 0

## *******************************************************************

## merge to specimen data 
dim(spec)
spec <- merge(spec, 
              veg.site..yr.sr.sum,
                  all.x=TRUE)
dim(spec)

spec[, col.mets][is.na(spec[, col.mets])] <- 0

###############################################################
##      Interaction Richness, Diversity & Uniqueness        ##
##            Year/Site/Sample Round Level                  ##


Interactions <- spec %>%
  select(Year, Site, SampleRound, GenusSpecies, PlantGenusSpecies) %>% 
  mutate(
    PlantPolInteraction = paste0(GenusSpecies, PlantGenusSpecies),
    YearSiteSampleRound = paste(Year, Site, SampleRound)
  ) 

IntAbundances <- Interactions %>% 
  summarise(
    n = n(), .by = c(Year, Site, SampleRound, PlantPolInteraction)
  )

IntTaxaMetrics <- IntAbundances %>% 
  summarise(
    IntRichnessSiteYearSR = n_distinct(PlantPolInteraction), 
    IntDiversitySiteYearSR = diversity(n, index = "shannon"),
    .by = c(Year, Site, SampleRound)
  )


## unique interactions per Year/Site combination
IntUniqueSiteYear <- Interactions %>% 
  filter(!is.na(GenusSpecies), !is.na(PlantGenusSpecies)) %>% 
  distinct(Year, Site, PlantPolInteraction) %>% 
  mutate(
    NSites = n_distinct(Site),
    UniqueToSite = NSites == 1,
    .by = c(Year, PlantPolInteraction)
  ) %>% 
  summarise(
    UniqueIntSiteYear = sum(UniqueToSite),
    .by = c(Year, Site)
  )

## unique interactions per Year/Site/Sample Round combination
IntUniqueSiteSRYear <- Interactions %>% 
  filter(!is.na(GenusSpecies), !is.na(PlantGenusSpecies)) %>% 
  distinct(Year, Site, SampleRound, PlantPolInteraction) %>% 
  mutate(
    NSites = n_distinct(Site),
    UniqueToSite = NSites == 1,
    .by = c(Year, SampleRound, PlantPolInteraction)
  ) %>% 
  summarise(
    UniqueIntSiteYearSR = sum(UniqueToSite),
    .by = c(Year, Site, SampleRound)
  )

## Merge datasets
InteractionData <- IntUniqueSiteSRYear %>% 
  left_join(IntUniqueSiteYear, by = c("Year", "Site"))

InteractionData <- IntTaxaMetrics %>% 
  left_join(InteractionData, by = c("Year", "Site", "SampleRound"))

spec.net <- spec %>% 
  left_join(InteractionData, by = c("Year", "Site", "SampleRound"),
            relationship = "many-to-one")

write.csv(spec.net, file='../data/spec_net.csv',
          row.names=FALSE)

save(spec.net, file='../data/spec_net.Rdata')
