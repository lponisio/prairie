# fit_ml: fit a multivariate multilevel brms model
#
# formula : a brmsformula or mvbrmsformula, e.g. plants + bees
#           (each bf() carries its own family; don't add set_rescor, it's added below)
# rescor  : residual correlations; keep FALSE when families differ or when
#           one response is a predictor of another
# Returns : fitted brms model

fit_ml <- function(formula, data,
                   rescor        = FALSE,
                   # file          = NULL,
                   # file_refit    = "on_change", 
                   iter          = 4000, warmup = floor(iter / 2),
                   chains        = 4,    cores  = chains,
                   threads       = threading(2),
                   max_treedepth = 15,
                   adapt_delta   = 0.99,
                   seed          = 123,
                   pp_type       = c("dens_overlay", "hist", "scatter_avg",
                                     "intervals", "stat"),
                   backend       = "cmdstanr") {
  
  options(cmdstanr_verbose = FALSE)
  pp_type <- match.arg(pp_type)
  
  # Add residual-correlation setting to the combined formula
  f <- formula + brms::set_rescor(rescor)
  
  # No `family` argument: families live inside each bf()
  m <- brms::brm(
    formula = f,
    data    = data,
    iter    = iter,
    warmup  = warmup,
    chains  = chains,
    cores   = cores,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed    = seed,
    init    = 0,
    threads = threads,
    backend = backend,
    refresh = 0,
    silent  = 1
    # file    = file,
    # file_refit = file_refit
  )
  
  # --- Convergence diagnostics (across all responses) ---
  np  <- brms::nuts_params(m)
  div <- sum(np$Parameter == "divergent__" & np$Value == 1)
  
  sm  <- summary(m)
  # include random-effect SDs too, since multilevel models often struggle there
  pars <- do.call(rbind, c(list(sm$fixed),
                           lapply(sm$random, as.data.frame)))
  max_rhat     <- max(pars$Rhat,     na.rm = TRUE)
  min_ess_bulk <- min(pars$Bulk_ESS, na.rm = TRUE)
  
  responses <- names(m$formula$forms)
  if (is.null(responses)) responses <- m$formula$responses
  
  cat("\n====================\n")
  cat("Responses:",    paste(responses, collapse = ", "), "\n")
  cat("Divergences:",  div,                    "\n")
  cat("Max R-hat:",    round(max_rhat, 4),     "\n")
  cat("Min bulk ESS:", round(min_ess_bulk, 1), "\n")
  cat("====================\n\n")
  
  print(sm)
  
  # --- Posterior predictive checks, one per response ---
  for (r in responses) {
    print(brms::pp_check(m, resp = r, type = pp_type, ndraws = 1000) +
            ggplot2::ggtitle(paste("PP check:", r)))
  }
  
  # --- Posterior direction table ---
  cat("\n--- Posterior direction ---\n")
  print(posterior_direction_mv(m))
  
  m
}

posterior_direction_mv <- function(m, prob = 0.95) {
  d     <- posterior::as_draws_df(m)
  bcols <- grep("^b_", names(d), value = TRUE)
  lo <- (1 - prob) / 2
  hi <- 1 - lo
  
  out <- do.call(rbind, lapply(bcols, function(p) {
    x <- d[[p]]
    data.frame(
      term      = sub("^b_", "", p),
      estimate  = mean(x),
      std.error = sd(x),
      conf.low  = unname(quantile(x, lo)),
      conf.high = unname(quantile(x, hi)),
      Pgt0      = mean(x > 0),
      Plt0      = mean(x < 0),
      direction = ifelse(mean(x > 0) >= mean(x < 0), "positive", "negative")
    )
  }))
  rownames(out) <- NULL
  out
}

#Estimates how much of one variable's effect on another travels through a mediator
#using the posterior from the model by multiplying draw by draw to carry 
#uncertainty from both paths into product then summarizes the distribution.
indirect_effect <- function(m, a, b, prob = 0.95) {
  d <- posterior::as_draws_df(m)
  x <- d[[a]] * d[[b]]
  c(mean  = mean(x),
    lower = unname(quantile(x, (1 - prob) / 2)),
    upper = unname(quantile(x, 1 - (1 - prob) / 2)),
    Pgt0  = mean(x > 0))
}

# summarise_fixed_ml: combine population-level coefficient tables across
# fitted multivariate multilevel models and save to CSV
# models  : a NAMED list of fitted brms models
# out_csv : file path for the CSV
# drop_intercepts : TRUE removes the intercept rows from the table
# Returns : the combined table

summarise_fixed_ml <- function(models, out_csv, drop_intercepts = FALSE) {
  
  tab <- purrr::imap_dfr(models, function(fit, model_name) {
    out <- posterior_direction_mv(fit)
    if (drop_intercepts) out <- out[!grepl("Intercept", out$term), ]
    cbind(model = model_name, out)
  })
  
  dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(tab, out_csv, row.names = FALSE)
  
  tab
}