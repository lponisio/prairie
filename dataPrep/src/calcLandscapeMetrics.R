## This function calculates landscape metrics

calculateLSM <- function(rasters, centroids, years, buffer_dists, 
                         metrics_wanted, 
                         site_field = "MERGE_SRC",
                         shape= "circle",
                         all_classes = TRUE,
                         verbose = TRUE) {
  results <- list()
  for (yr in years) {
    for (buf_dist in buffer_dists) {
      lsm_result <- sample_lsm(
        landscape = rasters[[as.character(yr)]],
        y = centroids,
        plot_id = values(centroids)[, site_field],
        shape = shape, 
        size = buf_dist,
        what = metrics_wanted,
        all_classes = all_classes,
        verbose = verbose
      )
      lsm_result$Buffer_m <- buf_dist
      lsm_result$Year <- yr
      
      results[[length(results) + 1]] <- lsm_result
    }
  }
  final_results <- do.call(rbind, results)
  
  if (!is.null(class_lookup)) {
    final_results <- final_results |> 
      left_join(class_lookup, by = c("class" = "Value"))
  }
  
  return(final_results)
}