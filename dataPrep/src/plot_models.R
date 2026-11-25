# ---- helper: "typical" row for setting covariates in newdata ----
typical_row <- function(df, fit = NULL) {
  df <- dplyr::ungroup(df)
  
  safe_summarise <- function(df, predicate, summary_fn) {
    cols <- names(dplyr::select(df, where(predicate)))
    if (length(cols) == 0) return(tibble::tibble(.rows = 1))
    dplyr::summarise(df, dplyr::across(dplyr::all_of(cols), summary_fn))
  }
  
  out_num <- safe_summarise(df, is.numeric,   ~ mean(.x, na.rm = TRUE))
  out_fac <- safe_summarise(df, is.factor,    ~ levels(.x)[1])
  out_chr <- safe_summarise(df, is.character, ~ dplyr::first(na.omit(.x)))
  
  out <- dplyr::bind_cols(out_num, out_fac, out_chr)
  
  if (!is.null(fit)) {
    needed <- names(fit$data)
    missing_cols <- setdiff(needed, names(out))
    for (col in missing_cols) {
      col_data <- fit$data[[col]]
      out[[col]] <- if (is.factor(col_data))        levels(col_data)[1]
      else if (is.character(col_data)) dplyr::first(na.omit(col_data))
      else                             mean(col_data, na.rm = TRUE)
    }
  }
  
  if ("Year" %in% names(out)) out$Year <- NA
  
  as.data.frame(out)
}

# ---- helper: posterior_epred summary (median + interval) ----
pred_epred <- function(fit, newdata, re_formula = NA, probs = c(0.055, 0.945)) {
  ep <- brms::posterior_epred(fit, newdata = newdata, re_formula = re_formula)
  tibble::tibble(
    estimate = apply(ep, 2, stats::median),
    lo       = apply(ep, 2, stats::quantile, probs = probs[1]),
    hi       = apply(ep, 2, stats::quantile, probs = probs[2])
  )
}

# ---- continuous predictor: returns a ggplot (IMPORTANT) ----
plot_continuous <- function(fits, df, x, xlab = x, n = 120) {
  df <- dplyr::ungroup(df)
  
  base <- typical_row(df, fit = fits[[1]])
  xs <- seq(
    min(df[[x]], na.rm = TRUE),
    max(df[[x]], na.rm = TRUE),
    length.out = n
  )
  
  nd <- do.call(rbind, lapply(xs, function(v) {
    r <- base
    r[[x]] <- v
    r
  }))
  
  out <- purrr::imap_dfr(fits, function(fit, metric) {
    summ <- pred_epred(fit, nd, re_formula = NA)
    dplyr::bind_cols(tibble::tibble(metric = metric, xval = xs), summ)
  })
  
  p <- ggplot2::ggplot(out, ggplot2::aes(x = xval, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.2) +
    ggplot2::geom_line(linewidth = 0.7, ) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::labs(x = xlab, y = "Predicted mean (89% CrI)") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","))
  
  return(p)
}

# plots the marginal effect of a focal predictor from a Bayesian  model, 
# showing the posterior median and 95% credible interval across the predictor's range, 
# with optional fitted data points and significance-based line styling
plot_marginal_effect <- function(model,
                                   dat          = model$data,
                                   focal_var,                        
                                   focal_scaled,                     
                                   x_label      = NULL,
                                   y_label      = NULL,
                                   n_seq        = 50,
                                   sig_table    = NULL,
                                   lookup_term  = NULL,              
                                   response     = NULL,
                                   sig_only_solid = TRUE,
                                   prob_threshold = 0.95,
                                   show_points = TRUE,
                                   colour,
                                   ribbon_fill = colour) {
    
    # All scaled predictor columns (to zero out non-focal ones) 
    scaled_cols <- grep("^scale\\(", names(dat), value = TRUE)
    
    # Build prediction grid over focal predictor range 
    focal_seq <- seq(
      min(dat[[focal_var]], na.rm = TRUE),
      max(dat[[focal_var]], na.rm = TRUE),
      length.out = n_seq
    )
    
    newdat <- dat[rep(1, n_seq), ]
    newdat[[focal_var]] <- focal_seq
    
    # Re-scale the focal predictor using stored attributes
    sc <- attr(dat[[focal_scaled]], "scaled:center")
    ss <- attr(dat[[focal_scaled]], "scaled:scale")
    newdat[[focal_scaled]] <- (focal_seq - sc) / ss
    
    # Zero out all other scaled predictors (hold at their mean)
    other_scaled <- setdiff(scaled_cols, focal_scaled)
    for (col in other_scaled) {
      if (col %in% names(newdat))
        newdat[[col]] <- matrix(0, nrow = n_seq, ncol = 1,
                                dimnames = list(NULL, ""))
    }
    
    # Extract posterior predictions 
    pred    <- brms::posterior_epred(model, newdata = newdat, re_formula = NA)
    plotdat <- data.frame(
      focal = focal_seq,
      est   = apply(pred, 2, median),
      lo    = apply(pred, 2, quantile, 0.025),
      hi    = apply(pred, 2, quantile, 0.975)
    )
    # Fitted values at original data points — marginal (other predictors held at mean)
    if (show_points) {
      point_dat <- dat[, names(dat), drop = FALSE]  # copy original data
      
      # Zero out all OTHER scaled predictors (hold at their mean)
      for (col in other_scaled) {
        if (col %in% names(point_dat))
          point_dat[[col]] <- 0  # simple scalar, not a matrix
      }
      
      fitted_vals        <- brms::posterior_epred(model, newdata = point_dat, re_formula = NA)
      point_dat$fitted   <- apply(fitted_vals, 2, median)  # write to point_dat, not dat
    }
    
    # Significance lookup
    is_sig <- FALSE
    pstar  <- ""
    if (!is.null(sig_table) && !is.null(response)) {
      sig_row <- dplyr::filter(sig_table,
                               response == !!response,
                               term     == lookup_term)
      if (nrow(sig_row) > 0) {
        est   <- sig_row$estimate[1]
        p_dir <- if (est > 0) sig_row$Pgt0[1] else sig_row$Plt0[1]   # probability in the direction of the effect
        
        is_sig <- !is.na(p_dir) && p_dir >= prob_threshold
        
        # Stars still come from the table, but only shown if the threshold is met
        pstar_raw <- if (est > 0) sig_row$Pgt0Stars[1] else sig_row$Plt0Stars[1]
        pstar <- if (is_sig && !is.na(pstar_raw)) pstar_raw else ""
      }
    }
    
    lt <- if (sig_only_solid) ifelse(is_sig, "solid", "dashed") else "solid"
    
    # Make plots
    ggplot2::ggplot(plotdat, ggplot2::aes(x = focal, y = est)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                           fill = ribbon_fill, alpha = 0.15) +
      { if (show_points)
        ggplot2::geom_point(
          data = point_dat,
          ggplot2::aes(x = .data[[focal_var]], y = fitted),
          colour = colour, size = 2
        )
      } +
      ggplot2::geom_line(colour = "black", linewidth = 1, linetype = lt) +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      ggplot2::theme(
        axis.text  = ggplot2::element_text(size = 14),  
        axis.title = ggplot2::element_text(size = 14) 
      ) +
      ggplot2::labs(
        x = if (!is.null(x_label)) x_label else focal_var,
        y = if (!is.null(y_label)) y_label else response
      ) #+
      # { if (pstar != "")
      #   ggplot2::annotate(
      #     "text",
      #     x     = Inf, y = Inf,
      #     label = pstar,
      #     hjust = 1.1, vjust = 1.5,
      #     size  = 7
      #   )
      # }
  }

# Applies plot_marginal_effect across a named list of models and 
# combines the resulting plots into a grid using patchwork, 
# with optional subsetting by metric and per-model y-axis labels looked up by name.
plotNiceModels <- function(models,
                                ncol        = 2,
                                sig_table   = NULL,
                                metrics     = NULL,
                                lookup_term = NULL,
                                y_label     = NULL,
                                ...) {
  
  if (!is.null(metrics)) {
    missing_m <- setdiff(metrics, names(models))
    if (length(missing_m) > 0)
      warning("Metrics not found in models, skipping: ",
              paste(missing_m, collapse = ", "))
    models <- models[intersect(metrics, names(models))]
  }
  if (length(models) == 0) stop("No models remain after subsetting.")
  
  plots <- purrr::imap(models, function(mod, nm) {
    i <- which(names(models) == nm)
    ylab_i <- if (!is.null(y_label) && nm %in% names(y_label)) y_label[[nm]] else nm
    plot_marginal_effect(
      mod,
      sig_table   = sig_table,
      lookup_term = lookup_term,
      response    = nm,
      y_label     = ylab_i,
      ...
    )
  })
  
  patchwork::wrap_plots(plots, ncol = ncol)
}

plot_path_effects <- function(fit,
                              predictor = NULL,  # name as brms writes it
                              mediator  = NULL,
                              response  = NULL,
                              labels    = NULL,                  # optional named vector of pretty labels
                              width     = c(0.66, 0.95),
                              return_data = FALSE) {
  
  draws <- as_draws_df(fit)
  
  # brms names joint-model coefficients b_<response>_<term>
  nm <- list(
    a      = paste0("b_", mediator, "_", predictor),  # predictor -> mediator
    b      = paste0("b_", response, "_", mediator),   # mediator  -> response
    direct = paste0("b_", response, "_", predictor)   # predictor -> response
  )
  
  missing <- setdiff(unlist(nm), variables(draws))
  if (length(missing) > 0) {
    stop("These parameters were not found in the model: ",
         paste(missing, collapse = ", "),
         "\nAvailable b_ parameters: ",
         paste(grep("^b_", variables(draws), value = TRUE), collapse = ", "),
         call. = FALSE)
  }
  
  # default labels, overridable via `labels`
  lab <- c(
    a        = paste0(predictor, " \u2192 ", mediator),
    b        = paste0(mediator, " \u2192 ", response),
    direct   = paste0(predictor, " \u2192 ", response, " (direct)"),
    indirect = paste0("Indirect (", predictor, " \u2192 ", mediator,
                      " \u2192 ", response, ")")
  )
  if (!is.null(labels)) lab[names(labels)] <- labels
  
  paths <- tibble(
    path  = factor(rep(unname(lab), each = nrow(draws)), levels = rev(unname(lab))),
    value = c(draws[[nm$a]],
              draws[[nm$b]],
              draws[[nm$direct]],
              draws[[nm$a]] * draws[[nm$b]])
  )
  
  p <- ggplot(paths, aes(value, path, fill = path)) +     
    stat_halfeye(.width = width, point_interval = median_qi,
                 slab_alpha = 0.75, color = "black",
                 fill = "#0072B2") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
    labs(x = "Posterior estimate", y = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
  
  if (return_data) return(list(plot = p, data = paths))
  p
}

## Color Safe Pallette
pal <- c("#0072B2", "#E69F00", "#56B4E9", "#CC79A7")
