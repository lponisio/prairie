# Saves a named list of R model objects to a directory, one .rds file per model.
# Args:
#   models : named list of model objects
#   dir    : path to the output directory (created if it doesn't exist)

save_models_list <- function(models, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  purrr::iwalk(models, ~ saveRDS(.x, file = file.path(dir, paste0(.y, ".rds"))))
}

# Loads all .rds files from a directory back into a named list of model objects.
# Args:
#   dir : path to the directory containing .rds model files
# Returns:
#   A named list where each element is a model object and
#   each name is the original filename without the .rds extension

load_models_list <- function(dir) {
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  purrr::set_names(lapply(files, readRDS),
                   tools::file_path_sans_ext(basename(files)))
}

# This functions saves pp checks
savePPCheck <- function(models,
                        pp_types = "dens_overlay",
                        out_dir  = "figures/pp_checks",
                        ndraws   = 1000,
                        width    = 12,
                        height   = 6,
                        prefix   = NULL) {
  
  if (is.null(prefix)) {
    prefix <- sub("^models_", "", deparse(substitute(models)))
  }
  prefix <- sub("_+$", "", prefix)                 
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  iwalk(models, function(mod, nm) {
    
    # responses: several for a multivariate fit, NULL for a univariate one
    responses <- names(mod$formula$forms)
    if (is.null(responses)) responses <- NA_character_
    
    plots <- unlist(lapply(responses, function(r) {
      lapply(pp_types, function(tp) {
        args <- list(object = mod, type = tp, ndraws = ndraws)
        if (!is.na(r)) args$resp <- r
        ttl <- if (is.na(r)) paste(nm, "-", tp) else paste(nm, "-", r, "-", tp)
        do.call(pp_check, args) + ggtitle(ttl)
      })
    }), recursive = FALSE)
    
    combined <- wrap_plots(plots, ncol = length(pp_types)) +
      plot_annotation(title = paste("Posterior Predictive Checks:", nm))
    
    # file-safe name: spaces and parentheses removed
    safe_nm <- gsub("[^A-Za-z0-9]+", "_", nm)
    safe_nm <- gsub("^_|_$", "", safe_nm)
    
    ggsave(
      filename = file.path(out_dir, paste0(prefix, "_", safe_nm, "_ppchecks_combined.pdf")),
      plot     = combined,
      width    = width,
      height   = height * length(responses)   # taller when there are several responses
    )
  })
  
  invisible(models)
}
