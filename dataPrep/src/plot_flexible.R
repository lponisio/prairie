# =============================================================================
# term_map: raw column name -> brms fixef() row name (for sig lookup only)
# =============================================================================
term_map <- c(
  "Pyrodiversity"                                 = "scalelogpyrodiv",
  "Percent Burned in Past 10 yrs"                 = "scalelogpct_burned_10yrs",
  "No. Burns in 10 Year Period"                   = "scalelogn_burn_10yrs",
  "Vegetation Rarefied Richness"                  = "scalelogVegRarefiedRichness"
)

# =============================================================================
# typical_row: median/mode values of all predictors in raw units
# =============================================================================
typical_row_pyro <- function(df) {
  data.frame(
    pyrodiv                    = median(df$pyrodiv,              na.rm = TRUE),
    pct_burned_10yrs           = median(df$pct_burned_10yrs, na.rm = TRUE),
    n_burn_10yrs               = median(df$n_burn_10yrs,        na.rm = TRUE),
    VegRarefiedRichness        = median(df$VegRarefiedRichness,     na.rm = TRUE)[1]
  )
}

# =============================================================================
# build_newdata: vary one predictor across its raw range, hold others typical
# =============================================================================
build_newdata <- function(df, x, n = 120) {
  base <- typical_row_pyro(df)
  xs   <- seq(min(df[[x]], na.rm = TRUE),
              max(df[[x]], na.rm = TRUE),
              length.out = n)
  nd <- do.call(rbind, lapply(xs, function(v) { r <- base; r[[x]] <- v; r }))
  list(xs = xs, nd = nd)
}

# =============================================================================
# pred_epred: get posterior mean + 95% CrI from a brms fit
# =============================================================================
pred_epred <- function(fit, nd, re_formula = NA) {
  epred <- brms::posterior_epred(fit, newdata = nd, re_formula = re_formula)
  tibble::tibble(
    estimate = apply(epred, 2, mean),
    lo       = apply(epred, 2, quantile, 0.025),
    hi       = apply(epred, 2, quantile, 0.975)
  )
}

# =============================================================================
# compute_r2_labels: whole-model Bayesian R² for each fit
# =============================================================================
compute_r2_labels <- function(fits, metric_labels = NULL) {
  purrr::imap_dfr(fits, function(fit, metric) {
    r2    <- brms::bayes_R2(fit)
    est   <- round(r2[1, "Estimate"], 2)
    lo    <- round(r2[1, "Q2.5"],    2)
    hi    <- round(r2[1, "Q97.5"],   2)
    label <- paste0("R\u00b2 = ", est, " [", lo, ", ", hi, "]")
    tibble::tibble(metric = metric, r2_label = label)
  }) |>
    dplyr::mutate(metric = dplyr::recode(metric, !!!metric_labels))
}

# =============================================================================
# compute_coef_labels: posterior of a single term's coefficient for each fit
# More appropriate when plotting one predictor at a time
# =============================================================================
compute_coef_labels <- function(fits, lookup_term, metric_labels = NULL) {
  purrr::imap_dfr(fits, function(fit, metric) {
    fe <- brms::fixef(fit)
    if (!lookup_term %in% rownames(fe)) {
      return(tibble::tibble(metric = metric, r2_label = ""))
    }
    est   <- round(fe[lookup_term, "Estimate"], 2)
    lo    <- round(fe[lookup_term, "Q2.5"],    2)
    hi    <- round(fe[lookup_term, "Q97.5"],   2)
    label <- paste0("\u03b2 = ", est, " [", lo, ", ", hi, "]")
    tibble::tibble(metric = metric, r2_label = label)
  }) |>
    dplyr::mutate(metric = dplyr::recode(metric, !!!metric_labels))
}

# =============================================================================
# plot_selected: main plotting function
# label_type = "r2"   -> whole-model Bayesian R² annotation
# label_type = "coef" -> posterior of the plotted term's coefficient
# label_type = "none" -> no annotation
# =============================================================================
plot_selected <- function(fits,
                          df,
                          x,
                          xlab           = x,
                          metrics        = NULL,
                          sig_only_solid = TRUE,
                          show_points    = TRUE,
                          point_alpha    = 0.4,
                          point_col      = "gray",
                          sig_table      = NULL,
                          term_map       = NULL,
                          n              = 120,
                          ncol           = NULL,
                          metric_labels  = NULL,
                          annotate_r2    = TRUE,
                          r2_size        = 3,
                          label_type     = c("r2", "coef", "none")) {
  
  label_type <- match.arg(label_type)
  
  # --- 1. Subset models to requested metrics --------------------------------
  if (!is.null(metrics)) {
    missing_m <- setdiff(metrics, names(fits))
    if (length(missing_m) > 0)
      warning("Metrics not found in fits, skipping: ",
              paste(missing_m, collapse = ", "))
    fits <- fits[intersect(metrics, names(fits))]
  }
  if (length(fits) == 0) stop("No models remain after subsetting.")
  
  # --- 2. Resolve brms term name for sig lookup -----------------------------
  lookup_term <- if (!is.null(term_map) && x %in% names(term_map)) {
    term_map[[x]]
  } else {
    x
  }
  
  # --- 3. Build raw prediction grid -----------------------------------------
  nd_list <- build_newdata(df, x, n)
  xs      <- nd_list$xs
  nd      <- nd_list$nd
  
  # --- 4. Predictions + significance per model ------------------------------
  out <- purrr::imap_dfr(fits, function(fit, metric) {
    summ <- pred_epred(fit, nd, re_formula = NA)
    
    pstar <- ""
    if (!is.null(sig_table)) {
      sig_row <- dplyr::filter(sig_table, response == metric, term == lookup_term)
      if (nrow(sig_row) > 0) {
        pstar <- if (sig_row$estimate[1] > 0) {
          sig_row$Pgt0Stars[1]
        } else {
          sig_row$Plt0Stars[1]
        }
      }
    }
    
    dplyr::bind_cols(
      tibble::tibble(metric = metric, xval = xs, pstar = pstar),
      summ
    )
  })
  
  # --- 5. Rename metric labels ----------------------------------------------
  if (!is.null(metric_labels)) {
    out <- out |>
      dplyr::mutate(metric = dplyr::recode(metric, !!!metric_labels))
  }
  
  # --- 6. Raw data points ---------------------------------------------------
  point_layer <- NULL
  if (show_points) {
    raw_cols <- intersect(names(fits), names(df))
    if (length(raw_cols) == 0) {
      message("show_points = TRUE but no metric columns found in df. Skipping.")
    } else {
      raw_long <- df |>
        dplyr::select(dplyr::all_of(c(x, raw_cols))) |>
        tidyr::pivot_longer(
          cols      = dplyr::all_of(raw_cols),
          names_to  = "metric",
          values_to = "y_raw"
        ) |>
        dplyr::rename(xval = dplyr::all_of(x))
      
      if (!is.null(metric_labels)) {
        raw_long <- raw_long |>
          dplyr::mutate(metric = dplyr::recode(metric, !!!metric_labels))
      }
      
      point_layer <- ggplot2::geom_point(
        data        = raw_long,
        mapping     = ggplot2::aes(x = xval, y = y_raw),
        alpha       = point_alpha,
        colour      = point_col,
        size        = 1.5,
        inherit.aes = FALSE
      )
    }
  }
  
  # --- 7. Linetype by significance ------------------------------------------
  out <- out |>
    dplyr::mutate(
      linetype = if (sig_only_solid)
        dplyr::if_else(pstar != "", "solid", "dashed")
      else
        "solid"
    )
  
  # --- 8. Build plot --------------------------------------------------------
  p <- ggplot2::ggplot(out, ggplot2::aes(x = xval, y = estimate)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo, ymax = hi),
      alpha = 0.15
    ) +
    ggplot2::geom_line(
      ggplot2::aes(linetype = linetype),
      linewidth = 0.8
    ) +
    ggplot2::scale_linetype_identity()
  
  if (!is.null(point_layer)) p <- p + point_layer
  
  # --- 9. Annotation (R², coefficient, or none) -----------------------------
  if (annotate_r2 && label_type != "none") {
    
    annot_df <- if (label_type == "r2") {
      compute_r2_labels(fits, metric_labels)
    } else {
      compute_coef_labels(fits, lookup_term, metric_labels)
    }
    
    # Append p-stars to annotation label
    pstar_df <- out |>
      dplyr::distinct(metric, pstar)
    
    annot_df <- dplyr::left_join(annot_df, pstar_df, by = "metric") |>
      dplyr::mutate(
        r2_label = dplyr::if_else(
          !is.na(pstar) & pstar != "",
          paste0(r2_label, "  ", pstar),
          r2_label
        )
      )
    
    x_min <- min(df[[x]], na.rm = TRUE)
    
    p <- p +
      ggplot2::geom_text(
        data        = annot_df,
        mapping     = ggplot2::aes(x = x_min, y = Inf, label = r2_label),
        hjust       = 0,
        vjust       = 1.6,
        size        = r2_size,
        colour      = "grey30",
        inherit.aes = FALSE
      )
  }
  
  # --- 10. Facet + theme ----------------------------------------------------
  p <- p +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = ncol) +
    ggplot2::labs(x = xlab, y = "Predicted mean (95% CrI)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey92"),
      strip.text       = ggplot2::element_text(size = 9)
    )
  
  return(p)
}

##### PLOT INTERACTION

  plot_interaction <- function(fits,
                                df,
                                x1,
                                x2,
                                xlab             = x1,
                                x2_breaks        = 3,
                                metrics          = NULL,
                                metric_labels    = NULL,
                                sig_table        = NULL,
                                term_map         = NULL,
                                interaction_term = NULL,
                                sig_only_solid   = TRUE,
                                n                = 120,
                                ncol             = NULL) {
    
    # --- 1. Subset models -----------------------------------------------------
    if (!is.null(metrics)) {
      missing_m <- setdiff(metrics, names(fits))
      if (length(missing_m) > 0)
        warning("Metrics not found in fits, skipping: ",
                paste(missing_m, collapse = ", "))
      fits <- fits[intersect(metrics, names(fits))]
    }
    if (length(fits) == 0) stop("No models remain after subsetting.")
    
    # --- 2. Resolve lookup term -----------------------------------------------
    lookup <- if (!is.null(interaction_term)) {
      interaction_term
    } else {
      if (!is.null(term_map) && x1 %in% names(term_map)) term_map[[x1]] else x1
    }
    
    # --- 3. x2 slice values (low/mid/high quantiles) --------------------------
    x2_probs  <- seq(0.1, 0.9, length.out = x2_breaks)
    x2_vals   <- stats::quantile(df[[x2]], probs = x2_probs, na.rm = TRUE)
    x2_labels <- paste(x2, c("Low", "Mid", "High")[seq_len(x2_breaks)], sep = " ")
    names(x2_vals) <- x2_labels
    
    # --- 4. x1 grid -----------------------------------------------------------
    x1_seq <- seq(min(df[[x1]], na.rm = TRUE),
                  max(df[[x1]], na.rm = TRUE),
                  length.out = n)
    
    # --- 5. Build newdata -----------------------------------------------------
    base <- typical_row_pyro(df)
    
    nd <- dplyr::bind_rows(lapply(x2_labels, function(lab) {
      do.call(rbind, lapply(x1_seq, function(v) {
        r          <- base
        r[[x1]]    <- v
        r[[x2]]    <- x2_vals[[lab]]
        r$x2_class <- lab
        r
      }))
    }))
    
    # --- 6. Predictions per model ---------------------------------------------
    out <- purrr::imap_dfr(fits, function(fit, metric) {
      
      pred   <- brms::posterior_epred(fit, newdata = nd[, names(nd) != "x2_class"],
                                      re_formula = NA)
      nd$est <- apply(pred, 2, stats::median)
      nd$lo  <- apply(pred, 2, function(x) stats::quantile(x, 0.025, na.rm = TRUE))
      nd$hi  <- apply(pred, 2, function(x) stats::quantile(x, 0.975, na.rm = TRUE))
      
      # Significance lookup — uses `lookup` and `metric` which are both in scope here
      pstar  <- ""
      is_sig <- FALSE
      if (!is.null(sig_table)) {
        sig_row <- dplyr::filter(sig_table, response == metric, term == lookup)
        if (nrow(sig_row) > 0) {
          is_sig <- (sig_row$estimate[1] > 0 & sig_row$Pgt0Stars[1] != "") |
            (sig_row$estimate[1] < 0 & sig_row$Plt0Stars[1] != "")
          pstar  <- if (sig_row$estimate[1] > 0) sig_row$Pgt0Stars[1] else sig_row$Plt0Stars[1]
        }
      }
      
      nd |>
        dplyr::select(x2_class, x1_val = dplyr::all_of(x1), est, lo, hi) |>
        dplyr::mutate(metric = metric, pstar = pstar, is_sig = is_sig)
    })
    
    # --- 7. Rename metric labels ----------------------------------------------
    if (!is.null(metric_labels)) {
      out <- out |>
        dplyr::mutate(metric = dplyr::recode(metric, !!!metric_labels))
    }
    
    # --- 8. Linetype ----------------------------------------------------------
    out <- out |>
      dplyr::mutate(
        linetype = if (sig_only_solid) ifelse(is_sig, "solid", "dashed") else "solid"
      )
    
    # Colorblind-friendly palette (Okabe-Ito)
    cb_palette <- c("#E69F00", "#0072B2", "#009E73")[seq_len(x2_breaks)]
    names(cb_palette) <- x2_labels
    
    # --- 9. Plot --------------------------------------------------------------
    ggplot2::ggplot(out, ggplot2::aes(x = x1_val, y = est,
                                      color = x2_class,
                                      fill  = x2_class)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                           alpha = 0.15, color = NA) +
      ggplot2::geom_line(ggplot2::aes(linetype = linetype), linewidth = 1) +
      ggplot2::scale_color_manual(values = cb_palette, name = x2) +
      ggplot2::scale_fill_manual(values  = cb_palette, name = x2) +
      ggplot2::scale_linetype_identity() +
      ggplot2::facet_wrap(~metric, scales = "free_y", ncol = ncol) +
      ggplot2::labs(x = xlab, y = "Predicted mean (95% CrI)") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "grey92"),
        strip.text       = ggplot2::element_text(size = 9)
      )
  }
