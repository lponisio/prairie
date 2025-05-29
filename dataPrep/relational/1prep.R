## *******************************************************
## make files for use in relational database
## *******************************************************

source('prairie/dataPrep/relational/src/misc.R')

dir.create("prairie_saved/data/relational/original",
           showWarnings = FALSE, recursive = TRUE)

## load data
spec.data <- read.csv("prairie_saved/data/raw/specimens.csv",  stringsAsFactors=FALSE)

## *******************************************************
## next we will create the data structures that that we
## will use to construct our relational database
## *******************************************************

print(paste("original number of specimens", nrow(spec.data)))

## ****************************** !!!!
## BEWARE year format
## ****************************** !!!!
print("spec data date format")
print(head(spec.data$Date))
date.format.spec <-  "%y%m%d"
print(date.format.spec)

spec.data$Date <- as.Date(as.character(spec.data$Date), date.format.spec)

spec.data$Year <- format(spec.data$Date, "%Y")

spec.data$Method <- NA
spec.data$Method[spec.data$Year == "2025"] <- "Net"

## check sites
unique(spec.data$Site)

## ## add plant data
## plant.keys <- read.csv("prairie_saved/data/raw/plant.csv", stringsAsFactors=FALSE)

## plant.keys$PlantGenusSpecies <- fix.white.space(plant.keys$PlantGenusSpecies)

## ## check plant IDs
## plants <- sort(unique(plant.keys$PlantGenusSpecies))

## ## checked.plant.names <- TPL(plants)

## ## write.csv(checked.plant.names , file="checkedfloralnames.csv")


## spec.data$PlantGenusSpecies <- plant.keys$PlantGenusSpecies[
##                                               match(spec.data$FieldPlantID,
##                                                         plant.keys$Code)]

## print("field plant IDs without a match")
## unique(spec.data$FieldPlantID[is.na(spec.data$PlantGenusSpecies)])

## assign species IDs *********************************************
## source('prairie/dataPrep/speciesIDs/AssignSpecies.R')

write.csv(spec.data, file="prairie_saved/data/relational/original/specimens.csv",
          row.names=FALSE)


## *******************************************************
## create conditions file

data.weather <- read.csv("prairie_saved/data/raw/weather.csv")

unique(data.weather$Site)

print("weather data date format")
print(head(data.weather$Date))
date.format.weather <-  "%y%m%d"
print(date.format.weather)

data.weather$Date <- as.Date(as.character(data.weather$Date), date.format.weather)

check.data.weather <- aggregate(data.weather$TimeStart,
                                list(site = data.weather$Site,
                                     date = data.weather$Date,
                                     block = data.weather$Block,
                                     transect = data.weather$Transect),
                                length)  

check.data.weather

## write unique data to a table
write.csv(unique(data.weather),
          file="prairie_saved/data/relational/original/conditions.csv",
          row.names=FALSE)
## *******************************************************

data.geo <- read.csv("prairie_saved/data/raw/geography.csv")

data.geo$SiteBlockTransect <- paste(data.geo$Site,
                                    data.geo$Block,
                                    data.geo$Transect,
                                    sep="-")


table(data.geo$SiteBlockTransect)


data.geo <- data.geo[!duplicated(data.geo$SiteBlockTransect),]

table(data.geo$SiteBlockTransect)


## write unique data to a table
write.csv(data.geo, file="prairie_saved/data/relational/original/geography.csv",
          row.names=FALSE)


## *******************************************************
## veg
## *******************************************************

veg.data <- read.csv("prairie_saved/data/raw/veg.csv")

## veg.data$PlantGenusSpecies <- plant.keys$PlantGenusSpecies[
##   match(veg.data$PlantCode,
##         plant.keys$Code)]

## print("field plant IDs without a match")
## unique(veg.data$PlantCode[is.na(veg.data$PlantGenusSpecies)])


veg.data$PlantGenusSpecies[veg.data$PlantGenusSpecies == "NA"] <- ""
veg.data$PlantGenusSpecies[veg.data$PlantGenusSpecies == "#N/A"] <- ""

## CHECK DATE FORMAT
print("veg data date format")
print(head(veg.data$Date))
date.format.veg <-  "%y%m%d"
veg.data$Date <- as.Date(as.character(veg.data$Date), date.format.veg)
veg.data$Year <- format(veg.data$Date, "%Y")
print("double check date format")
print(head(veg.data$Date))

write.csv(veg.data, file="prairie_saved/data/relational/original/veg.csv",
          row.names=FALSE)
