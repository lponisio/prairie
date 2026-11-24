
# Output: fitted brms model object

fit_one <- function(resp, data, rhs,
                    phi_rhs = NULL,
                    iter    = 4000, warmup = floor(iter/2),
                    chains  = 4,    cores  = chains,
                    threads = threading(2),
                    family  = gaussian(), #default family
                    max_treedepth = 15,
                    adapt_delta = 0.99,
                    seed    = 123,
                    pp_type = c("dens_overlay", "hist", "scatter_avg", "intervals", "stat"),
                    backend = "cmdstanr",
                    verbose = TRUE,
                    prior = NULL) {
  
  # Validate pp_type; defaults to "dens_overlay" if unspecified
  pp_type <- match.arg(pp_type)
  
  # Build formula dynamically from strings, optionally adding a phi sub-model
  # e.g. resp = "cover", rhs = "treatment + (1|stand)" -> cover ~ treatment + (1|stand)
  if (is.null(phi_rhs)) {
    f <- brms::bf(stats::as.formula(paste0(resp, " ~ ", rhs)))
  } else {
    f <- brms::bf(stats::as.formula(paste0(resp, " ~ ", rhs)),
                  stats::as.formula(paste0("phi ~ ", phi_rhs)))
  }
  
  # Fit the Bayesian model via Stan
  m <- brms::brm(
    formula = f,
    data    = data,
    family  = family,
    iter    = iter,
    warmup  = warmup,
    chains  = chains,
    cores   = cores,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed    = seed,
    init    = 0,     # initialize chains at 0 to reduce early divergences
    threads = threads,
    backend = backend,
    silent  = 2,
    refresh = 0,
    prior = prior
  )
  
  # --- Convergence diagnostics ---
  
  # Extract NUTS sampler diagnostics (divergences, treedepth, etc.)
  np  <- brms::nuts_params(m)
  # Count divergent transitions - ideally 0
  div <- sum(np$Parameter == "divergent__" & np$Value == 1)
  
  # Pull fixed effects summary for convergence metrics
  sumry_fix <- summary(m)$fixed
  # R-hat: chain convergence metric; values > 1.01 suggest poor convergence
  max_rhat     <- max(sumry_fix$Rhat,     na.rm = TRUE)
  # Bulk ESS: effective sample size; low values (<400) suggest poor chain mixing
  min_ess_bulk <- min(sumry_fix$Bulk_ESS, na.rm = TRUE)
  
  # Print diagnostic summary to console
  cat("\n====================\n")
  cat("Model:",        resp,                      "\n")
  cat("Family:",       m$family$family,            "\n")
  cat("Divergences:",  div,                        "\n")
  cat("Max R-hat:",    round(max_rhat, 4),         "\n")
  cat("Min bulk ESS:", round(min_ess_bulk, 1),     "\n")
  cat("====================\n\n")
  
  # Print full model summary (fixed/random effects, family, fit info)
  print(summary(m))
  
  # Print posterior predictive check plot to evaluate model fit
  print(brms::pp_check(m, type = pp_type, ndraws = 1000))
  
  # --- Posterior direction table ---
  # Uses shared helper to compute P(>0), P(<0), and stars for each fixed effect
  pdir <- posterior_direction(m)
  
  # Add direction label based on which side of zero has more posterior mass
  pdir$direction <- ifelse(pdir$Pgt0 >= pdir$Plt0, "positive", "negative")
  
  # Reorder columns for readability
  pdir <- pdir[, c("term", "direction", "Pgt0", "Pgt0Stars", "Plt0", "Plt0Stars")]
  
  cat("\n--- Posterior direction ---\n")
  print(pdir)
  
  # Return fitted model for use in summarise_fixed() or further diagnostics
  return(m)
}

# -----------------------------------------------------------------------------
# summarise_fixed: combine fixed effect summaries across all fitted models
#
# Takes a named list of fitted brms models, extracts fixed effects estimates
# and posterior direction probabilities, and saves results to CSV.

# -----------------------------------------------------------------------------
summarise_fixed <- function(models, out_csv) {
  
  # Loop over each model, binding rows into a single summary table
  tab <- purrr::imap_dfr(models, function(fit, response_name) {
    
    # 1) Extract fixed effects: estimate, SE, and 95% credible interval
    fe <- broom.mixed::tidy(
      fit,
      effects    = "fixed",
      conf.int   = TRUE,
      conf.level = 0.95
    ) %>%
      dplyr::mutate(
        response = response_name,    # add response variable name for identification
        family   = fit$family$family # add family for reference
      ) %>%
      dplyr::select(response, family, term, estimate, std.error, conf.low, conf.high)
    
    # 2) Get posterior direction probabilities using shared helper
    pdir <- posterior_direction(fit)
    
    # 3) Join direction results onto fixed effects table by term name
    fe %>%
      dplyr::left_join(pdir, by = "term")
  })
  
  # Create output directory if it doesn't exist, then save
  dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(tab, out_csv, row.names = FALSE)
  
  tab
}

