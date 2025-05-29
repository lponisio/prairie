## *******************************************************
## create relational database
## *******************************************************

source('prairie/dataPrep/relational/src/misc.R')


dir.create("prairie_saved/data/relational/relational/tables",
           showWarnings = FALSE, recursive = TRUE)


library(RSQLite)

conditions <- read.csv("prairie_saved/data/relational/original/conditions.csv", as.is=TRUE)
specimens <- read.csv("prairie_saved/data/relational/original/specimens.csv", as.is=TRUE)
geography <- read.csv("prairie_saved/data/relational/original/geography.csv", as.is=TRUE)
veg <- read.csv("prairie_saved/data/relational/original/veg.csv", as.is=TRUE)

## *******************************************************
## Start by importing the conditions information, from the conditions
## file.
## *******************************************************
## check that if there is already a database, remove it
## initiate relational database via SQL Lite package

if(file.exists("prairie_saved/prairie.db")) file.remove("prairie_saved/prairie.db")
con <- dbConnect(dbDriver("SQLite"), dbname='prairie_saved/prairie.db')

## *******************************************************
## 1. Geographic information
## *******************************************************

## Temporarily identify site, block, transect combinations
keep <- c("Site", "BurnUnburned", "Block", "Transect")

geography$geo.code <- apply(geography[keep], 1, paste, collapse=";")
conditions$geo.code <- apply(conditions[keep], 1, paste, collapse=";")
specimens$geo.code <- apply(specimens[keep], 1, paste, collapse=";")
veg$geo.code <- apply(veg[keep], 1, paste, collapse=";")

keep <- c("geo.code", "Country",
          "State", "County",  "Lat", "Long", "CRS", "Locality")

geography <- unique(geography[keep])

geography <- cbind(GeographyPK=seq_len(nrow(geography)), geography)
rownames(geography) <- NULL
dbWriteTable(con, "tblGeography", geography, row.names=FALSE, overwrite=TRUE)

## Propagate geography key to the conditions table.
conditions$GeographyFK <-
    geography$GeographyPK[match(conditions$geo.code,
                                geography$geo.code)]

print("conditions with no site key")
sum(is.na(conditions$GeographyFK))
fix.geo <- conditions[is.na(conditions$GeographyFK),]
write.csv(fix.geo, file="prairie_saved/data/cleaning/cond_no_geo.csv")

## Propagate geography key to the specimens table.
specimens$GeographyFK <- geography$GeographyPK[match(specimens$geo.code,
                                                     geography$geo.code)]
print("specimens with no site key")
sum(is.na(specimens$GeographyFK))
fix.geo <- specimens[is.na(specimens$GeographyFK),]
write.csv(fix.geo, file="prairie_saved/data/cleaning/spec_no_geo.csv")

## Propagate geography key to the veg table.
veg$GeographyFK <- geography$GeographyPK[match(veg$geo.code,
                                                     geography$geo.code)]
print("veg surveys with no site key")
sum(is.na(veg$GeographyFK))
fix.geo <- veg[is.na(veg$GeographyFK),]
write.csv(fix.geo, file="prairie_saved/data/cleaning/spec_no_geo.csv")

## write a .csv version of this table (just for ease of viewing)
write.csv(dbReadTable(con, "tblGeography"),
          file="prairie_saved/data/relational/relational/tables/geography.csv",
          row.names=FALSE)

dbListTables(con)

## *******************************************************
## 2. Conditions
## *******************************************************

## Temporarily identify unique combinations:
keep <- c("GeographyFK", "Date")
conditions$cond.code <- apply(conditions[keep], 1, paste, collapse=";")
specimens$cond.code <- apply(specimens[keep], 1, paste, collapse=";")
veg$cond.code <- apply(veg[keep], 1, paste, collapse=";")

## make table
keep <- c("Date",
          "TimeStart", "TimeEnd", "TempStart",
          "TempEnd", "WindStart", "WindEnd",
          "WeatherStart", "WeatherEnd",
          "GeographyFK", "cond.code")

cond <- unique(conditions[keep])
rownames(cond) <- NULL
cond <- cbind(ConditionsPK=seq_len(nrow(cond)), cond)

## Don't upload the cond.code column
dbWriteTable(con, "tblConditions", cond[-ncol(cond)], row.names=FALSE)

## Propagate conditions key to the conditions table.
conditions$ConditionsFK <-
    cond$ConditionsPK[match(conditions$cond.code, cond$cond.code)]

## Propagate conditions key to the specimens table.
specimens$ConditionsFK <-
    cond$ConditionsPK[match(specimens$cond.code, cond$cond.code)]


## Propagate conditions key to the veg table.
veg$ConditionsFK <-
    cond$ConditionsPK[match(veg$cond.code, cond$cond.code)]



print("specimens without weather data")
print(specimens$UniqueID[is.na(specimens$ConditionsFK)])
print(specimens$cond.code[is.na(specimens$ConditionsFK)])

fix.cond <- specimens[is.na(specimens$ConditionsFK),]
write.csv(fix.cond, file="prairie_saved/data/cleaning/spec_no_cond.csv")

write.csv(dbReadTable(con, "tblConditions"),
          file="prairie_saved/data/relational/relational/tables/conditions.csv",
          row.names=FALSE)

## *******************************************************
## 2.1 sample dates  for veg datasets
## *******************************************************

## keep <- c("Site", "Date", "SampleRound")
## conditions_veg$samp.code <- gsub(" ", "", apply(conditions_veg[keep], 1, paste, collapse=";"))
## veg$samp.code <- apply(veg[keep], 1, paste, collapse=";")

## keep <- c("Site", "Date", "SampleRound",  "samp.code")
## samp <- unique(conditions_veg[keep])
## rownames(samp) <- NULL
## samp <- cbind(SamplePK=seq_len(nrow(samp)), samp)

## ## Don't upload the cond.code column
## dbWriteTable(con, "tblSample", samp[-ncol(samp)], row.names=FALSE,
##              overwrite=TRUE)

## ## Propagate conditions key to the veg table.
## veg$SampleFK <-
##   samp$SamplePK[match(veg$samp.code, samp$samp.code)]


## print(paste("site, round in veg data without condition key",
##             unique(paste(veg$Site,
##                          veg$SampleRound, veg$Date)[is.na(veg$SampleFK)])))


## *******************************************************
## 3. Insect species:
## *******************************************************

keep <- c("Order", "Family", "Genus", "SubGenus", "Species",
          "SubSpecies", "Determiner", "Author", "Sex")

insects <- specimens[keep]
insects <- unique(insects)

insects$gen.sp <- paste(insects$Order,
                        insects$Family,
                        insects$Genus,
                        insects$SubGenus,
                        insects$Species,
                        insects$SubSpecies,
                        insects$Sex,
                        sep=";")
insects <- cbind(InsectPK=seq_len(nrow(insects)), insects)
rownames(insects) <- NULL

dbWriteTable(con, "tblInsect", insects[-ncol(insects)],
             row.names=FALSE)

## Propagate insect key to the specimens table.
specimens$gen.sp <- paste(specimens$Order,
                          specimens$Family,
                          specimens$Genus,
                          specimens$SubGenus,
                          specimens$Species,
                          specimens$SubSpecies,
                           specimens$Sex,sep=";")
specimens$InsectFK <- insects$InsectPK[match(specimens$gen.sp,
                                             insects$gen.sp)]

print(paste("insects without insect IDs",
            specimens$UniqueID[is.na(specimens$InsectFK)]))

write.csv(dbReadTable(con, "tblInsect"),
          file="prairie_saved/data/relational/relational/tables/insect.csv",
          row.names=FALSE)

## *******************************************************
## 4. Plant species:
## *******************************************************

keep <- c('PlantGenusSpecies')
plants <- c(specimens[keep][[1]], veg[keep][[1]])
plants <- sort(unique(plants))

PlantGenus <- sapply(strsplit(plants, ' '), function(x) x[1])
PlantGenus[is.na(PlantGenus)] <- ''

PlantSpecies <- sapply(strsplit(plants, ' '), function(x) x[2])
PlantSpecies[is.na(PlantSpecies)] <- ''

plants <- data.frame(PlantPK=seq_along(PlantGenus),
                     PlantGenus, PlantSpecies)

rownames(plants) <- NULL
dbWriteTable(con, 'tblPlant', plants, row.names=FALSE)

## Propagate plant key to the specimens table.
specimens.plant.sp <- specimens[keep][[1]]

plants.plant.sp <- fix.white.space(paste(plants$PlantGenus,
                                         plants$PlantSpecies))

## propogate plant key to specimens
specimens$PlantFK <- plants$PlantPK[match(specimens.plant.sp,
                                          plants.plant.sp)]

print(paste("specimens without plants IDs",
            specimens$UniqueID[is.na(specimens$PlantFK)]))
## Propagate plant key to the veg table.
veg$PlantFK <- plants$PlantPK[match(veg$PlantGenusSpecies,
                                    plants.plant.sp)]

print(paste("veg without plants IDs",
            unique(veg$PlantGenusSpecies[is.na(veg$PlantFK)])))

write.csv(dbReadTable(con, 'tblPlant'),
          file='prairie_saved/data/relational/relational/tables/plant.csv',
          row.names=FALSE)


## *******************************************************
## 5. Specimens:
## *******************************************************

keep <- c('UniqueID', 'Collector', 'InsectFK',
          'PlantFK', 'ConditionsFK', 'GeographyFK')

specimens <- specimens[keep]

specimens <- unique(specimens)
rownames(specimens) <- NULL

dbWriteTable(con, 'tblSpecimens', specimens, row.names=FALSE, overwrite=TRUE)

write.csv(dbReadTable(con, 'tblSpecimens'),
          file='prairie_saved/data/relational/relational/tables/specimens.csv',
          row.names=FALSE)

print(paste("before traditional, dim=", nrow(specimens)))

## *******************************************************
## 6. quadrats:
## *******************************************************

keep <- c('PlantFK',
          'ConditionsFK',
          'BloomStatus',
          'PlantCount',
          'BloomCount',
          'MeanFlorets',
          'Collector')

veg <- veg[keep]
veg <- unique(veg)

dbWriteTable(con, 'tblVeg', veg, row.names=FALSE,
             overwrite=TRUE)

write.csv(dbReadTable(con, 'tblVeg'),
          file='prairie_saved/data/relational/relational/tables/veg.csv',
          row.names=FALSE)

print(paste("before veg traditional, dim=", nrow(veg)))
## *******************************************************
## close connection to database
## *******************************************************
dbDisconnect(con)

