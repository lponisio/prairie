## Common-species specimen ID template for a new project.
##
## All species blocks are commented out so this file will source even when
## no individuals have been entered for a template species. To use an
## element, remove the leading # from every line in that block and add IDs
## to temp.id. The first commented block below shows the ID-entry syntax.

## 1. Element names must be unique and should mirror
##Genus_species_subspecies_sex.

## 2. If the same taxon appears twice, merge IDs into one element, preserve
##    first-seen ID order, remove duplicate IDs, and combine Determiner names
##    separated by commas.

## 3. Morphospecies: Species = "sp." and SubSpecies = "PROJECT-a".
##    Do not put "sp.1", "sppE", or "morphoa" in Species.

## 4. cf./aff. IDs: put the qualifier in Species, e.g. Species = "cf. sodalis"
##    or Species = "aff. fulgida".

## 5. Authors: use "Authority, year" whenever known; leave blank only for
##    morphospecies, unresolved species, or names needing later lookup.

## 6. IDs: preserve original ID order.

## 7. Keep Sex values consistent within a project. This template uses
##    "female", "male", "mixed", or blank when unknown/not recorded.

sp.ids <- list()

## -------------------------------------------------------------------------
## Optional species blocks
## -------------------------------------------------------------------------

# sp.ids[["Agapostemon_subtilior_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Agapostemon",
#     SubGenus = "Agapostemon",
#     Species = "subtilior",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1872",
#     Determiner = "",  ## required; combine multiple determiners with commas when merging
#     temp.id = c(
#       ## Add IDs here. Examples: 101, 102, 103  OR  "ABBR-001", "ABBR-002"
#       ## For repeated prefixes, use: paste0("ABBR-", c("001", "002"))
#     )
# )

# sp.ids[["Agapostemon_virescens_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Agapostemon",
#     SubGenus = "Agapostemon",
#     Species = "virescens",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Fabricius, 1775",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Anthophora_urbana_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Anthophora",
#     SubGenus = "Mystacanthophora",
#     Species = "urbana",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Apis_mellifera_female"]] <- list(
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Apis",
#     SubGenus = "Apis",
#     Species = "mellifera",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Linnaeus",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_caliginosus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "caliginosus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Frison",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_caliginosus_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "caliginosus",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Frison",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_fervidus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Thoracobombus",
#     Species = "fervidus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Fabricius, 1798",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_flavifrons_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "flavifrons",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Cresson, 1863",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_flavifrons_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "flavifrons",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cresson, 1863",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_melanopygus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "melanopygus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Nylander, 1848",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_mixtus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Thoracobombus",
#     Species = "mixtus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_rufocinctus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Cullumanobombus",
#     Species = "rufocinctus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Cresson, 1863",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_rufocinctus_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Cullumanobombus",
#     Species = "rufocinctus",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cresson, 1863",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_sitkensis_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "sitkensis",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Nylander, 1848",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_vancouverensis_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "vancouverensis",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_vosnesenskii_female_2"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "vosnesenskii",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "gyne",
#     Author = "Radoszkowski, 1862",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_vosnesenskii_female"]] <- list(
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "vosnesenskii",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "worker",
#     Author = "Radoszkowski, 1862",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Bombus_vosnesenskii_male"]] <- list(
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Bombus",
#     SubGenus = "Pyrobombus",
#     Species = "vosnesenskii",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Radoszkowski, 1862",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Ceratina_acantha_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Ceratina",
#     SubGenus = "Zadontomerus",
#     Species = "acantha",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Provancher, 1895",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Ceratina_nanula_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Ceratina",
#     SubGenus = "Zadontomerus",
#     Species = "nanula",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1897",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Diadasia_bituberculata_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Diadasia",
#     SubGenus = "Coquillettapis",
#     Species = "bituberculata",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Diadasia_bituberculata_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Diadasia",
#     SubGenus = "Coquillettapis",
#     Species = "bituberculata",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Diadasia_ochracea_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Diadasia",
#     SubGenus = "dasiapis",
#     Species = "ochracea",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1906",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_confusus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "Nealictus",
#     Species = "confusus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Smith, 1853",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_farinosus_female"]] <- list(
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "",
#     Species = "farinosus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Smith, 1853",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_farinosus_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "",
#     Species = "farinosus",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Smith, 1853",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_ligatus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "Odontalictus",
#     Species = "ligatus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Say, 1837",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_rubicundus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "Protohalictus",
#     Species = "rubicundus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Christ, 1791",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_tripartitus_female"]] <- list(
#     DateDetermined = "",  ## e.g., "2026" or full date if used
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "Seladonia",
#     Species = "tripartitus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1895",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Halictus_tripartitus_mixed"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Halictus",
#     SubGenus = "Seladonia",
#     Species = "tripartitus",
#     SubSpecies = "",
#     Sex = "mixed",
#     Caste = "",
#     Author = "Cockerell, 1895",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Hoplitis_albifrons_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Megachilidae",
#     Genus = "Hoplitis",
#     SubGenus = "Alcidamea",
#     Species = "albifrons",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Kirby, 1837",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_anhypops_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "anhypops",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "McGinley, 1986",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_argemonis_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "",
#     Species = "argemonis",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1897",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_buccale_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Hemihalictus",
#     Species = "buccale",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Perez, 1903",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_cordleyi_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "cordleyi",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1906",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_diatretum_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "diatretum",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Vachal, 1904",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_glabriventre_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Hemihalictus",
#     Species = "glabriventre",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1907",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_incompletum_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Dialictus",
#     Species = "incompletum",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1907",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_marinense_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Dialictus",
#     Species = "marinense",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Michener, 1936",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_mellipes_female"]] <- list(
#     DateDetermined = "",  ## e.g., "2026" or full date if used
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "mellipes",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1907",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_nevadense_female"]] <- list(
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Dialictus",
#     Species = "nevadense",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1907",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_occultum_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "",
#     Species = "occultum",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Vachal, 1904",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_punctatoventre_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "",
#     Species = "punctatoventre",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1907",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_sandhousiellum_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "",
#     Species = "sandhousiellum",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Gibbs, 2010",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_sisymbrii_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "sisymbrii",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1895",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_sisymbrii_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "sisymbrii",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cockerell, 1895",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_titusi_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "Lasioglossum",
#     Species = "titusi",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Crawford, 1902",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Lasioglossum_villosulum_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Halictidae",
#     Genus = "Lasioglossum",
#     SubGenus = "",
#     Species = "villosulum",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Kirby, 1802",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Megachile_montivaga_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Megachilidae",
#     Genus = "Megachile",
#     SubGenus = "Megachile",
#     Species = "montivaga",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Megachile_perihirta_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Megachilidae",
#     Genus = "Megachile",
#     SubGenus = "Xanthosarus",
#     Species = "perihirta",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1898",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_communis_female"]] <- list(
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Melissodes",
#     Species = "communis",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_communis_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Melissodes",
#     Species = "communis",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_lupinus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Callimelissodes",
#     Species = "lupinus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1878",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_metenua_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Callimelissodes",
#     Species = "metenua",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1924",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_metenua_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Callimelissodes",
#     Species = "metenua",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cockerell, 1924",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_microstictus_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Eumelissodes",
#     Species = "microstictus",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cockerell, 1905",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_microstictus_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "Eumelissodes",
#     Species = "microstictus",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cockerell, 1905",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_rivalis_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "",
#     Species = "rivalis",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1872",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Melissodes_rivalis_male"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Apidae",
#     Genus = "Melissodes",
#     SubGenus = "",
#     Species = "rivalis",
#     SubSpecies = "",
#     Sex = "male",
#     Caste = "",
#     Author = "Cresson, 1872",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

# sp.ids[["Osmia_densa_female"]] <- list(
#
#     Order = "Hymenoptera",
#     Family = "Megachilidae",
#     Genus = "Osmia",
#     SubGenus = "Melanosmia",
#     Species = "densa",
#     SubSpecies = "",
#     Sex = "female",
#     Caste = "",
#     Author = "Cresson, 1864",
#     Determiner = "", 
#     temp.id = c(
#     )
# )

## -------------------------------------------------------------------------
## Optional edge-case templates
## -------------------------------------------------------------------------

# sp.ids[["Genus_sp_PROJECT_a_female"]] <- list(
#   DateDetermined = "",
#   Order = "Hymenoptera",
#   Family = "FamilyName",
#   Genus = "Genus",
#   SubGenus = "",
#   Species = "sp.",
#   SubSpecies = "PROJECT-a",
#   Sex = "female",
#   Caste = "",
#   Author = "",
#   Determiner = "",
#   temp.id = c()
# )

# sp.ids[["Genus_cf_species_female"]] <- list(
#   DateDetermined = "",
#   Order = "Hymenoptera",
#   Family = "FamilyName",
#   Genus = "Genus",
#   SubGenus = "",
#   Species = "cf. species",
#   SubSpecies = "",
#   Sex = "female",
#   Caste = "",
#   Author = "Authority, year if known",
#   Determiner = "",
#   temp.id = c()
# )

# sp.ids[["Genus_aff_species_male"]] <- list(
#   DateDetermined = "",
#   Order = "Hymenoptera",
#   Family = "FamilyName",
#   Genus = "Genus",
#   SubGenus = "",
#   Species = "aff. species",
#   SubSpecies = "",
#   Sex = "male",
#   Caste = "",
#   Author = "Authority, year if known",
#   Determiner = "",
#   temp.id = c()
# )

source('prairie/dataPrep/speciesIDs/src/cleaning.R')
checkSpeciesIDs(sp.ids = sp.ids, project_abbrev = "PI-")
