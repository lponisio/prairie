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
#     Determiner = "A.S.Jackson",  ## required; combine multiple determiners with commas when merging
#     temp.id = c(332, 596, 700
#       ## Add IDs here. Examples: 101, 102, 103  OR  "ABBR-001", "ABBR-002"
#       ## For repeated prefixes, use: paste0("ABBR-", c("001", "002"))
#     )
# )
# 

sp.ids[["Agapostemon_subtilior_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Agapostemon",
    SubGenus = "Agapostemon",
    Species = "subtilior",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cresson, 1872",
    Determiner = "A.S.Jackson",
    temp.id = c(332, 596, 700
    )
)

sp.ids[["Agapostemon_virescens_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Agapostemon",
    SubGenus = "Agapostemon",
    Species = "virescens",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Fabricius, 1775",
    Determiner = "A.S. Jackson",
    temp.id = c(27, 59, 64, 156, 391, 395, 501, 522, 636, 719, 762, 819, 1370
    )
)

sp.ids[["Anthophora_urbana"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Anthophora",
    SubGenus = "Mystacanthophora",
    Species = "urbana",
    SubSpecies = "",
    Sex = "unknown",
    Caste = "",
    Author = "Cresson, 1878",
    Determiner = "",
    temp.id = c(1079
    )
)

sp.ids[["Apis_mellifera_female"]] <- list(
    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Apis",
    SubGenus = "Apis",
    Species = "mellifera",
    SubSpecies = "",
    Sex = "female",
    Caste = "worker",
    Author = "Linnaeus",
    Determiner = "",
    temp.id = c(13, 14, 15, 19, 20, 22, 25, 26, 37, 38, 39, 48, 50, 53, 55, 61, 
                62, 63, 71, 76, 81, 101, 111, 123, 134, 146, 147, 148, 153, 157, 
                191, 230, 232, 246, 248, 257, 269, 272, 298, 308, 318, 331, 344, 
                364, 378, 399, 405, 407, 412, 418, 423, 431, 432, 435, 439, 440, 
                442, 444, 455, 457, 462, 464, 466, 471, 472, 473, 577, 752, 754, 
                756, 871, 894, 914, 945, 989, 1061, 1068, 1122, 1126, 1127, 1130, 
                1133, 1134, 1135, 1136, 1137, 1138, 1139, 1141, 1142, 1143, 1144, 
                1145, 1146, 1149, 1150, 1151, 1152, 1153, 1155, 1157, 1158, 1159, 
                1203, 1214, 1251, 1253, 1256, 1263, 1264, 1266, 1280, 1281, 1283, 
                1284, 1285, 1287, 1289, 1290, 1291, 1293, 1294, 1296, 1297, 1299, 
                1300, 1301, 1302, 1304, 1306, 1309, 1345, 1350, 1353, 1357, 1366, 
                1367, 1368, 1369, 1372, 1376, 1379, 1384, 1385, 1388, 1393, 1394, 
                1396, 1397, 1402, 1404, 1405, 1406, 1412, 1415, 1421, 1423, 1424, 
                1430, 1431, 1438, 1440, 1441
    )
)

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

sp.ids[["Bombus_fervidus_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Bombus",
    SubGenus = "Thoracobombus",
    Species = "fervidus",
    SubSpecies = "",
    Sex = "female",
    Caste = "worker",
    Author = "Fabricius, 1798",
    Determiner = "A.S. Jackson",
    temp.id = c(143, 207, 326, 349, 404, 410, 411, 417, 420, 433, 481,541, 
                661, 683, 688, 955, 956, 782, 1204, 1277, 1391, 1392
    )
)

sp.ids[["Bombus_fervidus_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Bombus",
  SubGenus = "Thoracobombus",
  Species = "fervidus",
  SubSpecies = "",
  Sex = "male",
  Caste = "worker",
  Author = "Fabricius, 1798",
  Determiner = "A.S. Jackson",
  temp.id = c(1165
  )
)

sp.ids[["Bombus_flavifrons_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Bombus",
    SubGenus = "Pyrobombus",
    Species = "flavifrons",
    SubSpecies = "",
    Sex = "female",
    Caste = "worker",
    Author = "Cresson, 1863",
    Determiner = "A.S. Jackson",
    temp.id = c(166, 452, 458, 784
    )
)

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

sp.ids[["Bombus_mixtus_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Bombus",
    SubGenus = "Thoracobombus",
    Species = "mixtus",
    SubSpecies = "",
    Sex = "female",
    Caste = "worker",
    Author = "Cresson, 1878",
    Determiner = "A.S. Jackson",
    temp.id = c(456, 1305
    )
)

sp.ids[["Bombus_mixtus_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Bombus",
  SubGenus = "Thoracobombus",
  Species = "mixtus",
  SubSpecies = "",
  Sex = "male",
  Caste = "worker",
  Author = "Cresson, 1878",
  Determiner = "A.S. Jackson",
  temp.id = c(853
  )
)

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

sp.ids[["Bombus_vosnesenskii_female"]] <- list(
    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Bombus",
    SubGenus = "Pyrobombus",
    Species = "vosnesenskii",
    SubSpecies = "",
    Sex = "female",
    Caste = "worker",
    Author = "Radoszkowski, 1862",
    Determiner = "A.S. Jackson",
    temp.id = c(122, 190, 194, 212, 213, 275, 303, 314, 325, 419, 427, 438, 
                460, 463, 474, 498, 535, 538, 542, 543, 571, 572, 612, 614, 
                615, 621, 634, 741, 746, 750, 804, 839, 999, 1020, 1027, 
                1036, 1067, 1072, 1073, 1099, 1124, 1181, 1190, 1192, 1198, 
                1202, 1226, 1238, 1311, 1333, 1346, 1347, 1382, 1409, 1418
    )
)

sp.ids[["Bombus_vosnesenskii_male"]] <- list(
    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Bombus",
    SubGenus = "Pyrobombus",
    Species = "vosnesenskii",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Radoszkowski, 1862",
    Determiner = "A.S. Jakcson",
    temp.id = c(1206, 1209, 1215, 1216, 1217, 1219, 1221, 1225, 1321, 1331, 
                1363, 1399, 1429
    )
)

sp.ids[["Bombus_griseocollis_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Bombus",
  SubGenus = "Cullumanobombus",
  Species = "griseocollis",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "De Geer, 1773",
  Determiner = "A.S. Jackson",
  temp.id = c(1009
  )
)

sp.ids[["Bombus_vandykei_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Bombus",
  SubGenus = "Pyrobombus",
  Species = "vandykei",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Frison, 1927",
  Determiner = "A.S. Jackson",
  temp.id = c(434, 1437
  )
)

sp.ids[["Ceratina_acantha_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Ceratina",
    SubGenus = "Zadontomerus",
    Species = "acantha",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Provancher, 1895",
    Determiner = "A.S. Jackson",
    temp.id = c(2, 8, 9, 10, 87, 165, 175, 185, 270, 273, 274, 277, 291, 
                293, 321, 329, 335, 343, 348, 369, 374, 382, 383, 385, 
                467, 468, 482, 529, 533, 548,551, 557, 657, 658, 682, 
                690, 691, 692, 734, 831, 838, 1060, 1242, 1243, 1354, 1361
    )
)

sp.ids[["Heriades_carinata_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Heriades",
  SubGenus = "Physotetha",
  Species = "carinata",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1864",
  Determiner = "A.S. Jackson",
  temp.id = c(737, 992, 1324
  )
)

sp.ids[["Protosmia_rubifloris_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Protosmia",
  SubGenus = "Chelostomopis",
  Species = "rubifloris",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1898",
  Determiner = "A.S. Jakcson",
  temp.id = c(201, 368, 376
  )
)

sp.ids[["Coelioxys_rufitarsis_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Coelioxys",
  SubGenus = "Boreocoelioxys",
  Species = "rufitarsis",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Smith, 1854",
  Determiner = "A.S. Jakcson",
  temp.id = c(323
  )
)

sp.ids[["Panurginus_atriceps_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Panurginus",
  SubGenus = "",
  Species = "atriceps",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1878",
  Determiner = "",
  temp.id = c(152
  )
)

sp.ids[["Hylaeus_affinis_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Prospis",
  Species = "affinis",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Smith, 1853",
  Determiner = "A.S. Jackson",
  temp.id = c(582, 743
  )
)

sp.ids[["Hylaeus_saniculae_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Hylaeus",
  Species = "saniculae",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Robertson, 1896",
  Determiner = "A.S. Jackson",
  temp.id = c(430
  )
)

sp.ids[["Hylaeus_punctatus_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Spaturlariella",
  Species = "punctatus",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Brulle, 1832",
  Determiner = "A.S. Jackson",
  temp.id = c(994
  )
)

sp.ids[["Hylaeus_modestus_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Prospis",
  Species = "modestus",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Say, 1837",
  Determiner = "A.S. Jackson",
  temp.id = c(220, 590, 757
  )
)

sp.ids[["Hylaeus_leptocephalus_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Hylaeus",
  Species = "leptocephalus",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Morawitz, 1870",
  Determiner = "A.S. Jackson",
  temp.id = c(995
  )
)

sp.ids[["Hylaeus_communis"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Hylaeus",
  Species = "communis",
  SubSpecies = "",
  Sex = "",
  Caste = "",
  Author = "Nylander, 1852",
  Determiner = "A.S. Jackson",
  temp.id = c(987, 1401, 1422
  )
)

sp.ids[["Hylaeus_annulatus_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Colletidae",
  Genus = "Hylaeus",
  SubGenus = "Hylaeus",
  Species = "annulatus",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Linnaeus, 1758",
  Determiner = "A.S. Jackson",
  temp.id = c(580
  )
)

sp.ids[["Andrena_evoluta_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Diandrena",
  Species = "evoluta",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Linsley & MacSwain, 1961",
  Determiner = "A.S.Jackson",
  temp.id = c(322, 424, 639
  )
)

sp.ids[["Andrena_candida_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Thysandrena",
  Species = "candida",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Smith, 1879",
  Determiner = "A.S.Jackson",
  temp.id = c(1282
  )
)

sp.ids[["Andrena_candida_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Thysandrena",
  Species = "candida",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Smith, 1879",
  Determiner = "A.S.Jackson",
  temp.id = c(589
  )
)

sp.ids[["Andrena_nigrocaerulea_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Ptilandrena",
  Species = "nigrocaerulea",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1897",
  Determiner = "A.S.Jackson",
  temp.id = c(205
  )
)

sp.ids[["Andrena_angustitarsata_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Simandrena",
  Species = "angustitarsata",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Viereck, 1904",
  Determiner = "A.S.Jackson",
  temp.id = c(16
  )
)

sp.ids[["Andrena_astragali_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Ptilandrena",
  Species = "astragali",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Viereck & Cockerell, 1914",
  Determiner = "A.S.Jackson",
  temp.id = c(202
  )
)

sp.ids[["Andrena_prunorum_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Plastandrena",
  Species = "punorum",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1896",
  Determiner = "A.S.Jackson",
  temp.id = c(1207
  )
)

sp.ids[["Andrena_melanochroa_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Microandrena",
  Species = "melanochroa",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1898",
  Determiner = "A.S.Jackson",
  temp.id = c(35
  )
)

sp.ids[["Andrena_chlorura_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Ptilandrena",
  Species = "chlorura",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1916",
  Determiner = "A.S.Jackson",
  temp.id = c(29, 88, 138, 158, 330, 575
  )
)

sp.ids[["Andrena_cresson_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Opandrena",
  Species = "cressonii",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Robertson, 1891",
  Determiner = "A.S.Jackson",
  temp.id = c(186
  )
)

sp.ids[["Andrena_knuthiana_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Thysandrena",
  Species = "knuthiana",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1901",
  Determiner = "A.S.Jackson",
  temp.id = c(1211
  )
)

sp.ids[["Andrena_w-scripta_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Andrenidae",
  Genus = "Andrena",
  SubGenus = "Thysandrena",
  Species = "w-scripta",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Viereck, 1904",
  Determiner = "A.S.Jackson",
  temp.id = c(1148
  )
)

sp.ids[["Nomada_cf. edwardsii_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Nomada",
  SubGenus = "Nomadinae",
  Species = "cf. edwardsii",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Cresson, 1878",
  Determiner = "A.S.Jackson",
  temp.id = c(67, 69, 77
  )
)

sp.ids[["Nomada_Sp.1_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Nomada",
  SubGenus = "",
  Species = "Sp.1",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "",
  Determiner = "A.S.Jackson",
  temp.id = c(89, 278, 555
  )
)

sp.ids[["Nomada_Sp.2_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Nomada",
  SubGenus = "",
  Species = "Sp. 2",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "",
  Determiner = "A.S.Jackson",
  temp.id = c(72
  )
)

sp.ids[["Ceratina_acantha_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Ceratina",
    SubGenus = "Zadontomerus",
    Species = "acantha",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Provancher, 1895",
    Determiner = "A.S.Jackson",
    temp.id = c(80, 86, 222, 223, 227,242, 243, 285, 288, 292, 294, 
                336, 544, 1074, 1241, 1247
    )
)

sp.ids[["Ceratina_micheneri_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Ceratina",
  SubGenus = "Zadontomerus",
  Species = "micheneri",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Daily, 1973",
  Determiner = "A.S.Jackson",
  temp.id = c(85, 93, 144, 174, 179, 224, 234, 253, 267, 287, 302, 306, 
              340, 524, 558
  )
)

sp.ids[["Ceratina_micheneri_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Ceratina",
  SubGenus = "Zadontomerus",
  Species = "micheneri",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Daily, 1973",
  Determiner = "A.S.Jackson",
  temp.id = c(237, 256, 301, 341, 483, 540, 554, 556, 559, 560, 562, 567, 
              576, 583, 588, 592, 669, 693, 739, 1070, 1066, 1075, 1077, 
              1080, 1182, 1332
  )
)

sp.ids[["Ceratina_nanula_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Ceratina",
    SubGenus = "Zadontomerus",
    Species = "nanula",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1897",
    Determiner = "A.S. Jackson",
    temp.id = c(858
    )
)

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

sp.ids[["Halictus_rubicundus_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Halictus",
    SubGenus = "Nealictus",
    Species = "confusus",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Smith, 1853",
    Determiner = "A.S.Jackson",
    temp.id = c(247, 319, 354, 584, 1258
    )
)

sp.ids[["Halictus_rubicundus_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Halictus",
  SubGenus = "Nealictus",
  Species = "confusus",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Smith, 1853",
  Determiner = "A.S.Jackson",
  temp.id = c(1121, 1308
  )
)

sp.ids[["Halictus_farinosus_female"]] <- list(
    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Halictus",
    SubGenus = "",
    Species = "farinosus",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Smith, 1853",
    Determiner = "A.S. Jackson",
    temp.id = c(1, 14, 78, 82, 83, 84, 104, 105, 119, 155, 187, 189, 195, 199, 210, 
                214, 215, 279, 280, 281, 282, 283, 284, 310, 311, 327, 429, 516, 
                536, 537, 539, 617, 701, 705, 707, 711, 715, 717, 740, 742, 774, 
                800, 818, 829, 830, 856, 857, 860, 880, 882, 911, 922, 923, 925, 
                926, 928, 929, 930, 936, 940, 946, 947, 948, 949, 950, 952, 962, 
                964, 967, 968, 970, 973, 978, 1004, 1085, 1091, 1095, 1110, 1111, 
                1184, 1187, 1194, 1196, 1197, 1200, 1201, 1212, 1213, 1223, 1224, 
                1235, 1380, 1395, 1426
    )
)

sp.ids[["Halictus_farinosus_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Halictus",
    SubGenus = "",
    Species = "farinosus",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Smith, 1853",
    Determiner = "A.S. Jackson",
    temp.id = c(1195, 1352, 1362, 1425, 1428, 1435
    )
)

sp.ids[["Halictus_ligatus_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Halictus",
    SubGenus = "Odontalictus",
    Species = "ligatus",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Say, 1837",
    Determiner = "A.S.Jackson",
    temp.id = c(7, 41, 43, 117, 124, 151, 258, 262, 300,375, 386, 387, 392, 393,
                398, 450, 534, 553, 769, 770, 771, 772, 773, 792, 855, 866, 870, 
                874, 875, 876, 884, 885, 888, 892, 931, 998, 1005, 1032, 1035, 1115,
                1183, 1189, 1210, 1249, 1255, 1267, 1271, 1274, 1275, 1276, 1286,
                1288, 1310 ,1313, 1315,1316, 1318, 1319, 1320, 1326, 1330, 1334, 
                1337, 1338, 1358, 1364, 1365, 1371, 1375, 1377, 1387, 1389, 1407, 
                1410, 1413, 1417, 1419
    )
)

sp.ids[["Halictus_ligatus_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Halictus",
  SubGenus = "Odontalictus",
  Species = "ligatus",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Say, 1837",
  Determiner = "A.S.Jackson",
  temp.id = c(851, 1245, 1348, 1349, 1351, 1360
  )
)


sp.ids[["Halictus_tripartitus_female"]] <- list(
  
    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Halictus",
    SubGenus = "Seladonia",
    Species = "tripartitus",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1895",
    Determiner = "A.S.Jackson",
    temp.id = c(11, 12, 31, 34, 154, 226, 235, 239, 245, 250, 261, 276, 286, 
                517, 655, 656, 673, 681, 685, 803, 820, 821, 825, 826, 842, 
                846, 893, 1098, 1233, 1246, 1261, 1356, 1374
    )
)

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

sp.ids[["Hoplitis_albifrons_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Megachilidae",
    Genus = "Hoplitis",
    SubGenus = "Alcidamea",
    Species = "albifrons",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Kirby, 1837",
    Determiner = "A.S. Jackson",
    temp.id = c(441
    )
)

sp.ids[["Hoplitis_producta_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Megachilidae",
    Genus = "Hoplitis",
    SubGenus = "Alcidamea",
    Species = "producta",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cresson, 1864",
    Determiner = "A.S. Jakcson",
    temp.id = c(581, 751, 793, 1044
    )
)

sp.ids[["Hoplitis_grinnelli_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Hoplitis",
  SubGenus = "Alcidamea",
  Species = "grinnelli",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1910",
  Determiner = "A.S. Jackson",
  temp.id = c(390, 578
  )
)

sp.ids[["Hoplitis_grinnelli_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Hoplitis",
  SubGenus = "Alcidamea",
  Species = "grinnelli",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Cockerell, 1910",
  Determiner = "A.S. Jackson",
  temp.id = c(304, 338
  )
)

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

sp.ids[["Lasioglossum_olympiae_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "Lasioglossum",
    Species = "olympiae",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Cockerell, 1898",
    Determiner = "A.S.Jackson",
    temp.id = c(641, 1160
    )
)

sp.ids[["Lasioglossum_olympiae_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "Lasioglossum",
  Species = "olympiae",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1898",
  Determiner = "A.S.Jackson",
  temp.id = c(17, 18, 23, 24, 30, 33, 44, 45, 46, 56, 58, 60, 103, 106, 
              107, 108, 109, 113, 193, 197, 252, 297, 316, 351, 352, 357, 
              358, 359, 360, 361, 363, 365, 373, 381, 400, 401, 403, 415, 
              475, 476, 477, 478, 479, 480, 490, 491, 492, 497, 500, 510, 
              668, 763, 864
  )
)

sp.ids[["Lasioglossum_argemonis_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "",
    Species = "argemonis",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1897",
    Determiner = "",
    temp.id = c(696
    )
)

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

sp.ids[["Lasioglossum_cordleyi_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "Lasioglossum",
    Species = "cordleyi",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Crawford, 1906",
    Determiner = "",
    temp.id = c(42
    )
)

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

sp.ids[["Lasioglossum_incompletum_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "Dialictus",
    Species = "incompletum",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Crawford, 1907",
    Determiner = "A.S.Jackson",
    temp.id = c(74, 162, 171, 176, 225, 388, 503, 637, 652, 664, 687, 718, 
                725, 777, 783, 795, 805, 809, 834, 979, 980, 991, 1033, 
                1237, 1259, 1343, 1386, 1414, 1416
    )
)

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

sp.ids[["Lasioglossum_occultum_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "",
    Species = "occultum",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Vachal, 1904",
    Determiner = "",
    temp.id = c(445, 505, 642, 689, 723, 748, 749, 837
    )
)

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

sp.ids[["Lasioglossum_sisymbrii_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "Lasioglossum",
    Species = "sisymbrii",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1895",
    Determiner = "A.S.Jackson",
    temp.id = c(120, 372, 603, 604, 643, 663, 712, 1317
    )
)

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

sp.ids[["Lasioglossum_titusi_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "Lasioglossum",
    Species = "titusi",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Crawford, 1902",
    Determiner = "A.S.Jackson",
    temp.id = c(5, 28, 90, 121, 125, 126, 127, 139, 164, 167, 168, 181, 
                198, 206, 209, 240, 244, 249, 265, 266, 299, 305, 313, 315, 
                317, 320, 328, 334, 345, 346, 346, 347, 350, 394, 402, 414, 
                422, 449, 453, 486, 487, 488, 489, 499, 502, 506, 507, 508, 
                509, 511, 512, 513, 515, 519, 520, 531, 546, 561, 563, 564, 
                565, 566, 569, 570, 594, 595, 597, 598, 599, 600, 601, 602, 
                605, 606, 607, 608, 609, 610, 611, 613, 618, 619, 620, 622, 
                623, 624, 625, 626, 627, 628, 629, 630, 631, 644, 645, 647, 
                648, 649, 651, 665, 670, 675, 676, 677, 678, 680, 695, 697, 
                698, 702, 708, 710, 721, 722, 727, 729, 730, 731, 733, 738, 
                764, 766, 802, 807, 808, 810, 812, 813, 814, 815, 872, 883, 
                890, 900, 901, 902, 903, 904, 905, 908, 910, 913, 915, 918, 
                919, 921, 924, 933, 934, 935, 937, 938, 939, 941, 942, 943, 
                944, 961, 963, 966, 981, 983, 985, 993, 1024, 1039, 1041, 
                1042, 1052, 1053, 1054, 1057, 1059, 1069, 1078, 1088, 1092, 
                1093, 1094, 1096, 1097, 1101, 1103, 1140, 1220, 1248, 1292, 
                1327, 1427, 1436
    )
)

sp.ids[["Lasioglossum_villosulum_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "",
    Species = "villosulum",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Kirby, 1802",
    Determiner = "A.S.Jackson",
    temp.id = c(36, 75, 79, 97, 98, 99, 100, 115, 137, 140, 141, 142, 
                150, 180, 251, 263, 333, 389, 396, 459, 699, 801, 906, 
                965, 969, 1001, 1227, 1270
    )
)

sp.ids[["Lasioglossum_titusi_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "Lasioglossum",
    Species = "titusi",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Crawford, 1902",
    Determiner = "A.S.Jackson",
    temp.id = c(728, 899
    )
)

sp.ids[["Lasioglossum_villosulum_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Halictidae",
    Genus = "Lasioglossum",
    SubGenus = "",
    Species = "villosulum",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Kirby, 1802",
    Determiner = "A.S.Jackson",
    temp.id = c(1273, 1439
    )
)

sp.ids[["Lasioglossum_kincaidii_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "",
  Species = "kincaidii",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1898",
  Determiner = "A.S.Jackson",
  temp.id = c(118, 130, 135, 136, 145, 521, 527, 528, 530, 1222, 1230, 1231
  )
)

sp.ids[["Lasioglossum_cressonii_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "Dialictus",
  Species = "cressonii",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Robertson, 1890",
  Determiner = "",
  temp.id = c(484
  )
)

sp.ids[["Lasioglossum_ruidosense_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "Dialictus",
  Species = "ruidosense",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1897",
  Determiner = "",
  temp.id = c(172
  )
)

sp.ids[["Lasioglossum_pacatum_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "Dialictus",
  Species = "pacatum",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Sandhouse, 1924",
  Determiner = "",
  temp.id = c(650
  )
)

sp.ids[["Lasioglossum_knereri_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "Dialictus",
  Species = "knereri",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Gibbs, 2010",
  Determiner = "",
  temp.id = c(1383
  )
)

sp.ids[["Lasioglossum_zonulus_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Halictidae",
  Genus = "Lasioglossum",
  SubGenus = "Leuchalictus",
  Species = "zonulus",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Smith, 1848",
  Determiner = "",
  temp.id = c(260
  )
)

sp.ids[["Eucera_edwardsii_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Eucera",
  SubGenus = "Synhalonia",
  Species = "edwardsii",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Cresson, 1879",
  Determiner = "A.S. Jackson",
  temp.id = c(133, 236
  )
)

sp.ids[["Eucera_virgata_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Eucera",
  SubGenus = "Synhalonia",
  Species = "virgata",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1950",
  Determiner = "A.S. Jackson",
  temp.id = c(568
  )
)

sp.ids[["Eucera_cordleyi_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Eucera",
  SubGenus = "Synhalonia",
  Species = "cordleyi",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Viereck, 1905",
  Determiner = "A.S. Jackson",
  temp.id = c(461
  )
)

sp.ids[["Megachile_montivaga_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Megachilidae",
    Genus = "Megachile",
    SubGenus = "Megachile",
    Species = "montivaga",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cresson, 1878",
    Determiner = "A.S.Jakcson",
    temp.id = c(745, 747, 927
    )
)

sp.ids[["Megachile_perihirta_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Megachilidae",
    Genus = "Megachile",
    SubGenus = "Xanthosarus",
    Species = "perihirta",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1898",
    Determiner = "A.S.Jackson",
    temp.id = c(854, 1185, 1335, 1398
    )
)

sp.ids[["Megachile_perihirta_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Megachilidae",
    Genus = "Megachile",
    SubGenus = "Xanthosarus",
    Species = "perihirta",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Cockerell, 1898",
    Determiner = "A.S.Jackson",
    temp.id = c(92, 1268, 1411
    )
)

sp.ids[["Megachile_fidelis_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "Sayapis",
  Species = "fidelis",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Cresson, 1878",
  Determiner = "A.S.Jackson",
  temp.id = c(852
  )
)

sp.ids[["Megachile_fidelis_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "Sayapis",
  Species = "fidelis",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1878",
  Determiner = "A.S.Jackson",
  temp.id = c(1229, 1329
  )
)

sp.ids[["Megachile_brevis_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "Litomegachile",
  Species = "brevis",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Say, 1837",
  Determiner = "A.S.Jackson",
  temp.id = c(891, 895
  )
)

sp.ids[["Megachile_brevis_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "Litomegachile",
  Species = "brevis",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Say, 1837",
  Determiner = "A.S.Jackson",
  temp.id = c(1278, 1355
  )
)

sp.ids[["Megachile_gravita_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "megachiloides",
  Species = "gravita",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Mitchell, 1933",
  Determiner = "A.S.Jackson",
  temp.id = c(744, 990
  )
)

sp.ids[["Megachile_centuncularis_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "megachile",
  Species = "centuncularis",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Linnaeus, 1758",
  Determiner = "A.S.Jackson",
  temp.id = c(65, 290, 586
  )
)

sp.ids[["Megachile_pugnata_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Megachile",
  SubGenus = "Sayapis",
  Species = "pugnata",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Say, 1837",
  Determiner = "A.S.Jackson",
  temp.id = c(736
  )
)

sp.ids[["Anthidium_Utahense_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Anthidium",
  SubGenus = "anthidium",
  Species = "Utahense",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Swenk, 1914",
  Determiner = "A.S.Jackson",
  temp.id = c(635
  )
)

sp.ids[["Melissodes_communis_female"]] <- list(
    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "Melissodes",
    Species = "communis",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cresson, 1878",
    Determiner = "A.S. Jakcson",
    temp.id = c(951, 953, 954, 984, 1002, 1381
    )
)

sp.ids[["Melissodes_communis_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "Melissodes",
    Species = "communis",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Cresson, 1878",
    Determiner = "A.S. Jackson",
    temp.id = c(451, 99
    )
)

sp.ids[["Melissodes_lupinus_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "Callimelissodes",
    Species = "lupinus",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cresson, 1878",
    Determiner = "A.S. Jackson",
    temp.id = c(828, 957, 1102, 1105, 1125
    )
)

sp.ids[["Melissodes_lupinus_male"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Melissodes",
  SubGenus = "Callimelissodes",
  Species = "lupinus",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "Cresson, 1878",
  Determiner = "A.S. Jackson",
  temp.id = c(640, 1084, 1087, 1104, 1106, 1112, 1113, 1114, 1118, 1120, 
              1234, 1244, 1250
  )
)

sp.ids[["Melissodes_metenua_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "Callimelissodes",
    Species = "metenua",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1924",
    Determiner = "A.S. Jackson",
    temp.id = c(1272, 1295
    )
)

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

sp.ids[["Melissodes_microstictus_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "Eumelissodes",
    Species = "microstictus",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1905",
    Determiner = "A.S. Jackson",
    temp.id = c(848, 861, 868, 1031, 1186, 1188, 1191, 1193, 1314, 1328, 1339, 
                1340, 1341
    )
)

sp.ids[["Melissodes_microstictus_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "Eumelissodes",
    Species = "microstictus",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Cockerell, 1905",
    Determiner = "A.S. Jackson",
    temp.id = c(1323
    )
)

sp.ids[["Melissodes_rivalis_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "",
    Species = "rivalis",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cresson, 1872",
    Determiner = "A.S. Jackson",
    temp.id = c(1199, 1252, 1342
    )
)

sp.ids[["Melissodes_rivalis_male"]] <- list(

    Order = "Hymenoptera",
    Family = "Apidae",
    Genus = "Melissodes",
    SubGenus = "",
    Species = "rivalis",
    SubSpecies = "",
    Sex = "male",
    Caste = "",
    Author = "Cresson, 1872",
    Determiner = "A.S. Jakcson",
    temp.id = c(1000
    )
)

sp.ids[["Melissodes_clarkiae_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Apidae",
  Genus = "Melissodes",
  SubGenus = "Callimelissodes",
  Species = "clarkiae",
  SubSpecies = "",
  Sex = "male",
  Caste = "",
  Author = "W. Clark, 1852",
  Determiner = "A.S. Jakcson",
  temp.id = c(897
  )
)

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

sp.ids[["Osmia_albolateralis_female"]] <- list(

    Order = "Hymenoptera",
    Family = "Megachilidae",
    Genus = "Osmia",
    SubGenus = "Melanosmia",
    Species = "albolateralis",
    SubSpecies = "",
    Sex = "female",
    Caste = "",
    Author = "Cockerell, 1906",
    Determiner = "A.S.Jakcson",
    temp.id = c(312, 355, 356, 525, 758, 960
    )
)

sp.ids[["Osmia_dolerosa_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "dolerosa",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Sandhouse, 1939",
  Determiner = "A.S.Jakcson",
  temp.id = c(6, 68, 196, 229, 694, 755
  )
)

sp.ids[["Osmia_Sp.1_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "",
  Species = "Sp.1",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "",
  Determiner = "A.S.Jackson",
  temp.id = c(32, 95
  )
)

sp.ids[["Osmia_Sp.2_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "",
  Species = "Sp.2",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "",
  Determiner = "A.S.Jackson",
  temp.id = c(91, 254, 307, 339, 397
  )
)

sp.ids[["Osmia_atrocyanea_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "atrocyanea",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1897",
  Determiner = "A.S.Jackson",
  temp.id = c(255, 761
  )
)

sp.ids[["Osmia_kincaidii_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "kincaidii",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1897",
  Determiner = "A.S.Jackson",
  temp.id = c(366, 367
  )
)

sp.ids[["Osmia_montana_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Cephalosmia",
  Species = "montana",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1864",
  Determiner = "A.S.Jackson",
  temp.id = c(259, 532
  )
)

sp.ids[["Osmia_regulina_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "regulina",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1911",
  Determiner = "A.S.Jackson",
  temp.id = c(654
  )
)

sp.ids[["Osmia_nemoris_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "nemoris",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Sandhouse, 1924",
  Determiner = "A.S.Jackson",
  temp.id = c(380, 384, 523, 616, 667, 790, 896, 958, 959, 1089, 1107, 1228, 
              1239, 1260, 1373, 1420
  )
)

sp.ids[["Osmia_coloradensis_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Helicosmia",
  Species = "coloradensis",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1878",
  Determiner = "A.S.Jackson",
  temp.id = c(735, 849, 850, 879
  )
)

sp.ids[["Osmia_pusilla_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "pusilla",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1864",
  Determiner = "A.S.Jackson",
  temp.id = c(192, 200, 216, 231, 353, 579, 662, 1076
  )
)

sp.ids[["Osmia_trevoris_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Melanosmia",
  Species = "trevoris",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cockerell, 1897",
  Determiner = "A.S.Jackson",
  temp.id = c(163, 211
  )
)

sp.ids[["Osmia_texana_female"]] <- list(
  
  Order = "Hymenoptera",
  Family = "Megachilidae",
  Genus = "Osmia",
  SubGenus = "Helicosmia",
  Species = "texana",
  SubSpecies = "",
  Sex = "female",
  Caste = "",
  Author = "Cresson, 1872",
  Determiner = "A.S.Jackson",
  temp.id = c(1322
  )
)

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
