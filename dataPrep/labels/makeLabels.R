rm(list=ls())
setwd("C:/")
source("lab_paths.R")
local.path

setwd(local.path)

setwd(file.path(local.path,
                       "prairie_saved/data/relational/traditional/"))

load('specimens-complete.Rdata')

spec <- res.complete

makeLabel <- function(x){
    paste(x["UniqueID"], x["Locality"], "\\",
          x["geo.code"], x["Date"],  "\\",
          x["PlantGenus"], x["PlantSpecies"],x["PlantEpi"], x["PlantSubSpeciesVar"], "\\",
          x["County"], x["State"], x["Country"], "\\", sep=" ")
}


spec <- spec[spec$UniqueID >= 1443 & spec$UniqueID <= 2077, ]
spec <- spec[order(spec$UniqueID),]

labels <- apply(spec, 1, makeLabel)

write.table(labels, file="../../../labels/lables_jul_19_2026.txt", sep="\n", row.names=FALSE)
