checkSpeciesIDs <- function(path = NULL,
                            sp.ids = NULL,
                            project_abbrev = NULL,
                            print_counts = TRUE,
                            print_issues = TRUE) {
  ## ------------------------------------------------------------
  ## Species ID validator for sp.ids lists
  ##
  ## Use:
  ##   out <- checkSpeciesIDs("All_Species_ID.R", project_abbrev = "HJ-")
  ##   out <- checkSpeciesIDs(sp.ids = sp.ids, project_abbrev = "PNW-")
  ##
  ## Returns:
  ##   $counts_by_taxon
  ##   $issues
  ##   $issue_summary
  ##   $element_index
  ##   $duplicate_ids_within_elements
  ##   $duplicate_ids_across_elements
  ##
  ## Notes:
  ## - This is a flagging/checking function, not an auto-cleaner.
  ## - It checks morphology conventions, duplicate IDs, missing authors,
  ##   author years, element-name/metadata mismatches, determiners, etc.
  ## - It prints Genus Species SubSpecies Sex counts for tray checking.
  ## ------------------------------------------------------------

  add_issue <- function(severity, issue_type, element_name = NA_character_,
                        field = NA_character_, value = NA_character_,
                        detail = NA_character_, recommendation = NA_character_) {
    data.frame(
      severity = severity,
      issue_type = issue_type,
      element_name = element_name,
      field = field,
      value = as.character(value),
      detail = detail,
      recommendation = recommendation,
      stringsAsFactors = FALSE
    )
  }

  clean_str <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    x <- as.character(x[1])
    x <- gsub("\\\\&", "&", x)
    trimws(x)
  }

  normalize_sex <- function(x) {
    x <- tolower(clean_str(x))
    if (x %in% c("f", "female", "worker", "gyne", "queen")) return("female")
    if (x %in% c("m", "male", "drone")) return("male")
    if (x %in% c("mixed", "both")) return("mixed")
    if (x == "") return("")
    x
  }

  expected_element_name <- function(genus, species, subspecies, sex) {
    parts <- c(genus, species, subspecies, sex)
    parts <- parts[!is.na(parts) & parts != ""]
    parts <- gsub("^cf\\.\\s*", "cf_", parts)
    parts <- gsub("^aff\\.\\s*", "aff_", parts)
    parts <- gsub("\\.", "", parts)
    parts <- gsub("[^A-Za-z0-9]+", "_", parts)
    parts <- gsub("_+", "_", parts)
    parts <- gsub("^_|_$", "", parts)
    paste(parts, collapse = "_")
  }

  is_morphospecies <- function(species, subspecies, element_name) {
    species_l <- tolower(clean_str(species))
    subspecies_l <- tolower(clean_str(subspecies))
    elem_l <- tolower(clean_str(element_name))

    species_l %in% c("sp", "sp.", "spp", "spp.") ||
      grepl("^sp\\.?[A-Za-z0-9-]+$", species_l) ||
      grepl("^spp[A-Za-z0-9-]+$", species_l) ||
      grepl("morpho|_sp[._-]?[A-Za-z0-9]+|_spp[A-Za-z0-9]+", elem_l) ||
      grepl("^[A-Za-z]+-[A-Za-z0-9]+$", subspecies_l)
  }

  has_cf_aff <- function(x) {
    x <- tolower(clean_str(x))
    grepl("(^|[_[:space:]])(cf\\.|cf|aff\\.|aff)([_[:space:]]|$)", x)
  }

  author_has_year <- function(author) {
    author <- clean_str(author)
    author == "" || grepl("\\b(17|18|19|20)[0-9]{2}\\b", author)
  }

  source_duplicate_element_names <- function(path) {
    if (is.null(path) || !file.exists(path)) {
      return(data.frame())
    }

    txt <- readLines(path, warn = FALSE)

    ## Assignment style:
    ## sp.ids[["Name"]] <- list(
    assign_hits <- regmatches(
      txt,
      regexec('sp\\.ids\\s*\\[\\s*\\[\\s*"([^"]+)"\\s*\\]\\s*\\]\\s*<-\\s*list\\s*\\(', txt)
    )
    assign_names <- vapply(assign_hits, function(z) {
      if (length(z) >= 2) z[2] else NA_character_
    }, character(1))
    assign_names <- assign_names[!is.na(assign_names)]

    ## list style:
    ## Name = list(
    ## This intentionally only catches simple one-line element declarations.
    list_hits <- regmatches(
      txt,
      regexec("^\\s*([A-Za-z0-9_.-]+)\\s*=\\s*list\\s*\\(", txt)
    )
    list_names <- vapply(list_hits, function(z) {
      if (length(z) >= 2) z[2] else NA_character_
    }, character(1))
    list_names <- list_names[!is.na(list_names)]

    all_names <- c(assign_names, list_names)

    if (length(all_names) == 0) {
      return(data.frame())
    }

    tab <- sort(table(all_names), decreasing = TRUE)
    dup <- tab[tab > 1]

    if (length(dup) == 0) {
      return(data.frame())
    }

    data.frame(
      element_name = names(dup),
      occurrences_in_source = as.integer(dup),
      stringsAsFactors = FALSE
    )
  }

  ## ------------------------------------------------------------
  ## Load sp.ids
  ## ------------------------------------------------------------
  if (!is.null(path)) {
    env <- new.env(parent = emptyenv())
    sys.source(path, envir = env)
    if (!exists("sp.ids", envir = env, inherits = FALSE)) {
      stop("No object named `sp.ids` was created by sourcing the file.")
    }
    sp.ids <- get("sp.ids", envir = env)
  }

  if (is.null(sp.ids)) {
    stop("Provide either `path` or `sp.ids`.")
  }

  if (!is.list(sp.ids)) {
    stop("`sp.ids` must be a list.")
  }

  element_names <- names(sp.ids)
  if (is.null(element_names)) {
    element_names <- rep("", length(sp.ids))
  }

  required_fields <- c(
    "Order", "Family", "Genus", "SubGenus", "Species",
    "SubSpecies", "Sex", "Author", "Determiner", "temp.id"
  )

  issues <- list()
  element_rows <- list()
  id_rows <- list()

  ## ------------------------------------------------------------
  ## Source-level duplicate element names
  ## Important for assignment-style files where duplicates overwrite
  ## during sourcing.
  ## ------------------------------------------------------------
  source_dups <- source_duplicate_element_names(path)
  if (nrow(source_dups) > 0) {
    for (i in seq_len(nrow(source_dups))) {
      issues[[length(issues) + 1]] <- add_issue(
        severity = "error",
        issue_type = "duplicate_element_name_in_source",
        element_name = source_dups$element_name[i],
        field = "element_name",
        value = source_dups$occurrences_in_source[i],
        detail = "The same element name appears more than once in the source file.",
        recommendation = "Rename or merge duplicates before sourcing, because assignment-style duplicates may overwrite earlier records."
      )
    }
  }

  loaded_dup_names <- sort(table(element_names), decreasing = TRUE)
  loaded_dup_names <- loaded_dup_names[loaded_dup_names > 1]
  if (length(loaded_dup_names) > 0) {
    for (nm in names(loaded_dup_names)) {
      issues[[length(issues) + 1]] <- add_issue(
        severity = "error",
        issue_type = "duplicate_element_name_loaded",
        element_name = nm,
        field = "element_name",
        value = loaded_dup_names[[nm]],
        detail = "The loaded sp.ids list contains duplicated element names.",
        recommendation = "Merge same-taxon duplicates or rename biologically distinct entries so list names are unique."
      )
    }
  }

  ## ------------------------------------------------------------
  ## Per-element checks
  ## ------------------------------------------------------------
  for (i in seq_along(sp.ids)) {
    rec <- sp.ids[[i]]
    element_name <- element_names[i]
    if (is.na(element_name) || element_name == "") {
      element_name <- paste0("unnamed_element_", i)
    }

    if (!is.list(rec)) {
      issues[[length(issues) + 1]] <- add_issue(
        "error", "element_not_list", element_name,
        detail = "This element is not a list.",
        recommendation = "Each species ID element should be a list with metadata fields and temp.id."
      )
      next
    }

    missing_fields <- setdiff(required_fields, names(rec))
    if (length(missing_fields) > 0) {
      for (fld in missing_fields) {
        issues[[length(issues) + 1]] <- add_issue(
          "error", "missing_required_field", element_name, fld,
          detail = paste0("Missing field: ", fld),
          recommendation = paste0("Add `", fld, " = \"\"` or the correct value.")
        )
      }
    }

    genus <- clean_str(rec$Genus)
    subgenus <- clean_str(rec$SubGenus)
    species <- clean_str(rec$Species)
    subspecies <- clean_str(rec$SubSpecies)
    sex <- clean_str(rec$Sex)
    sex_norm <- normalize_sex(sex)
    author <- clean_str(rec$Author)
    determiner <- clean_str(rec$Determiner)
    family <- clean_str(rec$Family)
    order <- clean_str(rec$Order)

    ids <- rec$temp.id
    if (is.null(ids)) ids <- character(0)
    ids_chr <- as.character(ids)
    ids_chr <- trimws(ids_chr)
    ids_chr <- ids_chr[!is.na(ids_chr)]

    taxon_label <- paste(
      c(genus, species, subspecies, sex)[c(genus, species, subspecies, sex) != ""],
      collapse = " "
    )

    element_rows[[length(element_rows) + 1]] <- data.frame(
      element_name = element_name,
      element_index = i,
      Order = order,
      Family = family,
      Genus = genus,
      SubGenus = subgenus,
      Species = species,
      SubSpecies = subspecies,
      Sex = sex,
      sex_normalized = sex_norm,
      Caste = clean_str(rec$Caste),
      Author = author,
      Determiner = determiner,
      id_count = length(ids_chr),
      unique_id_count = length(unique(ids_chr)),
      taxon_label = taxon_label,
      stringsAsFactors = FALSE
    )

    if (length(ids_chr) > 0) {
      id_rows[[length(id_rows) + 1]] <- data.frame(
        element_name = element_name,
        Genus = genus,
        Species = species,
        SubSpecies = subspecies,
        Sex = sex,
        temp.id = ids_chr,
        stringsAsFactors = FALSE
      )
    }

    ## Blank core metadata
    for (fld in c("Genus", "Species", "Family", "Order")) {
      val <- clean_str(rec[[fld]])
      if (val == "") {
        issues[[length(issues) + 1]] <- add_issue(
          "error", "blank_core_taxon_field", element_name, fld, val,
          detail = paste0(fld, " is blank."),
          recommendation = paste0("Fill `", fld, "` before using this record.")
        )
      }
    }

    if (determiner == "") {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "missing_determiner", element_name, "Determiner", determiner,
        detail = "Determiner is blank.",
        recommendation = "Fill Determiner; when merging, combine names separated by commas."
      )
    }

    ## temp.id checks
    if (length(ids_chr) == 0) {
      issues[[length(issues) + 1]] <- add_issue(
        "info", "empty_temp_id", element_name, "temp.id", "",
        detail = "No specimen IDs entered for this element.",
        recommendation = "This is okay for templates; otherwise add IDs or comment/remove the element."
      )
    }

    if (any(ids_chr == "")) {
      issues[[length(issues) + 1]] <- add_issue(
        "error", "blank_temp_id", element_name, "temp.id", "",
        detail = "At least one temp.id is blank.",
        recommendation = "Remove blank IDs."
      )
    }

    dup_ids <- unique(ids_chr[duplicated(ids_chr)])
    if (length(dup_ids) > 0) {
      issues[[length(issues) + 1]] <- add_issue(
        "error", "duplicate_id_within_element", element_name, "temp.id",
        paste(dup_ids, collapse = ", "),
        detail = "The same temp.id appears more than once within this element.",
        recommendation = "Remove repeated IDs within the element, preserving first occurrence."
      )
    }

    ## Element name should roughly match metadata
    expected <- expected_element_name(genus, species, subspecies, sex_norm)
    if (expected != "" && !tolower(element_name) %in% tolower(c(expected, paste0(expected, "_1")))) {
      ## Do not over-flag intentionally numbered template entries like Bombus_x_female_2.
      genus_species <- paste0(gsub("[^A-Za-z0-9]+", "_", genus), "_",
                              gsub("[^A-Za-z0-9]+", "_", species))
      if (!grepl(tolower(genus_species), tolower(element_name), fixed = TRUE)) {
        issues[[length(issues) + 1]] <- add_issue(
          "warning", "element_name_metadata_mismatch", element_name, "element_name",
          element_name,
          detail = paste0("Expected name approximately like: ", expected),
          recommendation = "Rename the element so it mirrors Genus_species_subspecies_sex and stays unique."
        )
      }
    }

    ## Sex in element name vs Sex field
    elem_l <- tolower(element_name)
    elem_says_f <- grepl("(^|_)(f|female)($|_)", elem_l)
    elem_says_m <- grepl("(^|_)(m|male)($|_)", elem_l)
    if (elem_says_f && sex_norm != "" && sex_norm != "female") {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "element_name_sex_disagrees_with_Sex_field", element_name,
        "Sex", sex,
        detail = "Element name suggests female but Sex field differs.",
        recommendation = "Use the Sex field as the source of truth, then rename the element if needed."
      )
    }
    if (elem_says_m && sex_norm != "" && sex_norm != "male") {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "element_name_sex_disagrees_with_Sex_field", element_name,
        "Sex", sex,
        detail = "Element name suggests male but Sex field differs.",
        recommendation = "Use the Sex field as the source of truth, then rename the element if needed."
      )
    }

    ## Morphospecies checks
    morpho <- is_morphospecies(species, subspecies, element_name)
    if (morpho) {
      if (species != "sp.") {
        issues[[length(issues) + 1]] <- add_issue(
          "warning", "morphospecies_species_not_sp_dot", element_name,
          "Species", species,
          detail = "Morphospecies should use Species = \"sp.\" exactly.",
          recommendation = "Move the morphospecies code to SubSpecies."
        )
      }

      if (!is.null(project_abbrev) && project_abbrev != "") {
        if (subspecies != "" && !startsWith(subspecies, project_abbrev)) {
          issues[[length(issues) + 1]] <- add_issue(
            "warning", "morphospecies_subspecies_missing_project_prefix",
            element_name, "SubSpecies", subspecies,
            detail = paste0("Morphospecies code does not start with ", project_abbrev, "."),
            recommendation = paste0("Use SubSpecies = \"", project_abbrev, "a\" or similar.")
          )
        }

        if (subspecies == "" && species == "sp." &&
            grepl("(_sp[._-]?[A-Za-z0-9]+|morpho|spp[A-Za-z0-9]+)", elem_l)) {
          issues[[length(issues) + 1]] <- add_issue(
            "warning", "morphospecies_code_missing_from_SubSpecies",
            element_name, "SubSpecies", subspecies,
            detail = "Element name suggests a morphospecies code, but SubSpecies is blank.",
            recommendation = paste0("Put the morphocode in SubSpecies, e.g. \"", project_abbrev, "a\".")
          )
        }
      }

      if (author != "") {
        issues[[length(issues) + 1]] <- add_issue(
          "info", "morphospecies_author_nonblank",
          element_name, "Author", author,
          detail = "Morphospecies usually have blank Author.",
          recommendation = "Confirm this is not a named species; otherwise leave Author blank."
        )
      }
    }

    ## cf./aff. checks
    elem_has_cf_aff <- has_cf_aff(element_name)
    species_has_cf_aff <- has_cf_aff(species)

    if (elem_has_cf_aff && !species_has_cf_aff) {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "cf_aff_in_element_name_not_species",
        element_name, "Species", species,
        detail = "Element name includes cf./aff. but Species field does not.",
        recommendation = "Put the qualifier in Species, e.g. Species = \"cf. sodalis\"."
      )
    }

    if (species_has_cf_aff && !grepl("^(cf\\.|aff\\.)\\s+\\S+", tolower(species))) {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "cf_aff_species_format",
        element_name, "Species", species,
        detail = "cf./aff. Species field has unexpected formatting.",
        recommendation = "Use Species = \"cf. species\" or Species = \"aff. species\"."
      )
    }

    ## Author checks
    named_species <- species != "" &&
      species != "sp." &&
      !morpho &&
      !grepl("^(cf\\.|aff\\.)\\s+", tolower(species))

    uncertain_named_species <- grepl("^(cf\\.|aff\\.)\\s+", tolower(species))

    if ((named_species || uncertain_named_species) && author == "") {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "missing_author_for_named_species",
        element_name, "Author", author,
        detail = paste0("Author is blank for named taxon: ", taxon_label),
        recommendation = "Add taxonomic authority and year when known."
      )
    }

    if (author != "" && !author_has_year(author)) {
      issues[[length(issues) + 1]] <- add_issue(
        "warning", "author_missing_year",
        element_name, "Author", author,
        detail = "Author field does not contain a 4-digit year.",
        recommendation = "Use format like \"Cresson, 1878\" when known."
      )
    }

    if (grepl("^\\s+|\\s+$", clean_str(rec$Determiner))) {
      issues[[length(issues) + 1]] <- add_issue(
        "info", "determiner_whitespace",
        element_name, "Determiner", determiner,
        detail = "Determiner may contain leading/trailing whitespace.",
        recommendation = "Trim whitespace."
      )
    }

    if (subgenus != "" && grepl("^[a-z]", subgenus)) {
      issues[[length(issues) + 1]] <- add_issue(
        "info", "subgenus_starts_lowercase",
        element_name, "SubGenus", subgenus,
        detail = "SubGenus starts with a lowercase letter.",
        recommendation = "Confirm capitalization; many subgenera are capitalized."
      )
    }
  }

  element_index <- if (length(element_rows) > 0) {
    do.call(rbind, element_rows)
  } else {
    data.frame()
  }

  all_ids <- if (length(id_rows) > 0) {
    do.call(rbind, id_rows)
  } else {
    data.frame()
  }

  ## ------------------------------------------------------------
  ## Cross-element duplicate IDs
  ## ------------------------------------------------------------
  duplicate_ids_across <- data.frame()
  if (nrow(all_ids) > 0) {
    id_tab <- table(all_ids$temp.id)
    repeated_ids <- names(id_tab[id_tab > 1])

    if (length(repeated_ids) > 0) {
      duplicate_ids_across <- all_ids[all_ids$temp.id %in% repeated_ids, ]
      duplicate_ids_across <- duplicate_ids_across[
        order(duplicate_ids_across$temp.id, duplicate_ids_across$element_name),
      ]

      for (id in repeated_ids) {
        rows <- all_ids[all_ids$temp.id == id, ]
        taxa <- unique(paste(rows$Genus, rows$Species, rows$SubSpecies, rows$Sex))
        issues[[length(issues) + 1]] <- add_issue(
          "error", "duplicate_id_across_elements",
          element_name = paste(unique(rows$element_name), collapse = " | "),
          field = "temp.id",
          value = id,
          detail = paste("ID appears in", nrow(rows), "rows and", length(taxa), "taxon labels."),
          recommendation = "Check specimen tray/database and assign the ID to only one taxon unless this is an intentional duplicate."
        )
      }
    }
  }

  ## ------------------------------------------------------------
  ## Same taxon appears in multiple elements
  ## ------------------------------------------------------------
  if (nrow(element_index) > 0) {
    taxon_key <- paste(
      element_index$Order,
      element_index$Family,
      element_index$Genus,
      element_index$SubGenus,
      element_index$Species,
      element_index$SubSpecies,
      element_index$Sex,
      element_index$Caste,
      sep = "||"
    )
    taxon_tab <- table(taxon_key)
    dup_taxa <- names(taxon_tab[taxon_tab > 1])

    if (length(dup_taxa) > 0) {
      for (key in dup_taxa) {
        rows <- element_index[taxon_key == key, ]
        issues[[length(issues) + 1]] <- add_issue(
          "warning", "same_taxon_multiple_elements",
          element_name = paste(rows$element_name, collapse = " | "),
          field = "taxon metadata",
          value = rows$taxon_label[1],
          detail = paste("Same taxon metadata appears in", nrow(rows), "elements."),
          recommendation = "Merge IDs into one element if these are truly identical; combine determiners with commas."
        )
      }
    }
  }

  ## ------------------------------------------------------------
  ## Counts by taxon for unit-tray checking
  ## ------------------------------------------------------------
  if (nrow(all_ids) > 0) {
    all_ids$taxon_sort <- paste(
      all_ids$Genus,
      all_ids$Species,
      all_ids$SubSpecies,
      all_ids$Sex,
      sep = " "
    )

    counts_by_taxon <- aggregate(
      temp.id ~ Genus + Species + SubSpecies + Sex,
      data = all_ids,
      FUN = length
    )
    names(counts_by_taxon)[names(counts_by_taxon) == "temp.id"] <- "count"

    unique_counts <- aggregate(
      temp.id ~ Genus + Species + SubSpecies + Sex,
      data = all_ids,
      FUN = function(x) length(unique(x))
    )
    names(unique_counts)[names(unique_counts) == "temp.id"] <- "unique_count"

    counts_by_taxon <- merge(
      counts_by_taxon,
      unique_counts,
      by = c("Genus", "Species", "SubSpecies", "Sex"),
      all.x = TRUE
    )

    counts_by_taxon <- counts_by_taxon[
      order(
        counts_by_taxon$Genus,
        counts_by_taxon$Species,
        counts_by_taxon$SubSpecies,
        match(tolower(counts_by_taxon$Sex), c("female", "f", "male", "m", "mixed", "")),
        counts_by_taxon$Sex
      ),
    ]
    rownames(counts_by_taxon) <- NULL
  } else {
    counts_by_taxon <- data.frame()
  }

  duplicate_ids_within <- data.frame()
  if (nrow(all_ids) > 0) {
    key <- paste(all_ids$element_name, all_ids$temp.id, sep = "||")
    tab <- table(key)
    dup_key <- names(tab[tab > 1])
    if (length(dup_key) > 0) {
      duplicate_ids_within <- all_ids[key %in% dup_key, ]
      duplicate_ids_within <- duplicate_ids_within[
        order(duplicate_ids_within$element_name, duplicate_ids_within$temp.id),
      ]
      rownames(duplicate_ids_within) <- NULL
    }
  }

  issues_df <- if (length(issues) > 0) {
    do.call(rbind, issues)
  } else {
    data.frame(
      severity = character(),
      issue_type = character(),
      element_name = character(),
      field = character(),
      value = character(),
      detail = character(),
      recommendation = character(),
      stringsAsFactors = FALSE
    )
  }

  issue_summary <- if (nrow(issues_df) > 0) {
    as.data.frame(table(issues_df$severity, issues_df$issue_type),
                  stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
  if (nrow(issue_summary) > 0) {
    names(issue_summary) <- c("severity", "issue_type", "n")
    issue_summary <- issue_summary[issue_summary$n > 0, ]
    issue_summary <- issue_summary[order(issue_summary$severity, issue_summary$issue_type), ]
    rownames(issue_summary) <- NULL
  }

  if (print_counts) {
    cat("\nSPECIMEN UNIT-TRAY CHECK COUNTS\n")
    cat("--------------------------------\n")
    if (nrow(counts_by_taxon) == 0) {
      cat("No specimen IDs found.\n")
    } else {
      printable <- counts_by_taxon
      printable$label <- trimws(paste(
        printable$Genus,
        printable$Species,
        printable$SubSpecies,
        printable$Sex
      ))

      for (i in seq_len(nrow(printable))) {
        cat(sprintf(
          "%-55s %5s",
          printable$label[i],
          printable$count[i]
        ))
        if (!is.na(printable$unique_count[i]) &&
            printable$unique_count[i] != printable$count[i]) {
          cat(sprintf("  unique: %s", printable$unique_count[i]))
        }
        cat("\n")
      }
    }
  }

  if (print_issues) {
    cat("\nISSUE SUMMARY\n")
    cat("-------------\n")
    if (nrow(issue_summary) == 0) {
      cat("No issues flagged.\n")
    } else {
      print(issue_summary, row.names = FALSE)
    }
  }

  invisible(list(
    counts_by_taxon = counts_by_taxon,
    issues = issues_df,
    issue_summary = issue_summary,
    element_index = element_index,
    duplicate_ids_within_elements = duplicate_ids_within,
    duplicate_ids_across_elements = duplicate_ids_across,
    source_duplicate_element_names = source_dups
  ))
}
