# Run from the directory containing both prairie/ and prairie_saved/,
# as expected by 1prep.R.
save.dir <- "prairie/dataPrep/speciesIDs"
cleaning.dir <- "prairie_saved/data/cleaning"

required.scripts <- file.path(save.dir, c("src/AddData.R", "src/cleaning.R", "all_IDs.R"))
missing.scripts <- required.scripts[!file.exists(required.scripts)]
if (length(missing.scripts)) {
  stop("Cannot find required script(s): ", paste(missing.scripts, collapse = ", "),
       "\nWorking directory: ", getwd(),
       "\nRun from the directory containing prairie/ and prairie_saved/.")
}
if (!dir.exists(cleaning.dir) &&
    !dir.create(cleaning.dir, recursive = TRUE, showWarnings = FALSE)) {
  stop("Could not create ID diagnostic directory: ", cleaning.dir)
}

source(file.path(save.dir, "src/AddData.R"))
# all_IDs.R loads cleaning.R and calls checkSpeciesIDs() after defining sp.ids.
source(file.path(save.dir, "all_IDs.R"))

spec.data$Order <- spec.data$Family <- spec.data$Genus <- spec.data$SubGenus <-
  spec.data$SubSpecies <- spec.data$Sex <- spec.data$Determiner <-
  spec.data$Author <- NA

spec.data <- add.to.data(
  sp.ids = sp.ids, spec.dat = spec.data, output_dir = cleaning.dir
)
