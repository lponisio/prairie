
##  Utility functions for fitting, diagnosing, and summarizing brms models

pstars <- function(x) {
  if (is.na(x)) return("")
  if (x >= 0.975) "***"       # ~p < 0.025: very strong directional evidence
  else if (x >= 0.95)  "**"  # ~p < 0.05:  strong directional evidence
  else if (x >= 0.90)  "*"   # ~p < 0.10:  moderate directional evidence
  else ""                     # weak or no directional evidence
}

posterior_direction <- function(fit) {
  
  # Extract posterior draws as a data frame (one row per MCMC iteration)
  draws <- posterior::as_draws_df(fit)
  
  # Get fixed effect term names from model summary
  fe_terms <- rownames(summary(fit)$fixed)
  
  # Map summary term names to their column names in the draws data frame.
  draw_cols <- dplyr::case_when(
    fe_terms %in% c("(Intercept)", "Intercept") ~ "b_Intercept",
    grepl("^hu_", fe_terms)                     ~ paste0("b_hu_", sub("^hu_", "", fe_terms)),
    TRUE                                        ~ paste0("b_", fe_terms)
  )
  
  # Calculate P(effect > 0) for each term across all posterior draws
  # Returns NA if the expected column is not found in draws 
  Pgt0 <- round(purrr::map_dbl(draw_cols, function(nm) {
    if (!nm %in% names(draws)) return(NA_real_)
    mean(draws[[nm]] > 0)
  }), 2)
  
  # Calculate P(effect < 0) for each term across all posterior draws
  Plt0 <- round(purrr::map_dbl(draw_cols, function(nm) {
    if (!nm %in% names(draws)) return(NA_real_)
    mean(draws[[nm]] < 0)
  }), 2)
  
  # Assemble and return results as a data frame
  data.frame(
    term      = fe_terms,
    Pgt0      = Pgt0,
    Plt0      = Plt0,
    Pgt0Stars = vapply(Pgt0, pstars, character(1)),  # stars for positive direction
    Plt0Stars = vapply(Plt0, pstars, character(1)),  # stars for negative direction
    row.names = NULL
  )
}