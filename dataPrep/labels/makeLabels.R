rm(list=ls())

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


spec <- spec[order(spec$UniqueID),]

labels <- apply(spec, 1, makeLabel)

write.table(labels, file="../../../labels/lables_jul_30_2025.txt", sep="\n", row.names=FALSE)
