## Create dataframe with class type to merge final results
class_lookup <- data.frame(
  Value = 0:6,
  Class_name = c("Planted/Cultivated", "Wetland",
                 "Herbaceous", "Developed",
                 "Water", "Evergreen Forest", "Deciduous Forest"))
                  

## This function calculates the weighted proximity-area for each land cover class within 
## a designated buffer radius.

calcHabitatArea <- function(rasters, centroids, years, buffer_dists,
                                           site_field = NULL,
                                           #bee_size_labels = NULL,
                                           class_lookup = NULL)  {
  site_ids <- centroids[[site_field]][,1]
  results <- list()
  
  for (yr in years) {
    raster <- rasters[[as.character(yr)]]
    cell_area <- prod(terra::res(raster))
    
    for (i in seq_len(nrow(centroids))) {
      # Pulls site name from column in site_field for current row i
      site_id <- site_ids[i]
      # Stores sites point geometry
      pt <- centroids[i, ]
      
      for (buf_dist in buffer_dists) {
        
        # Buffer around centroid
        buf <- terra::buffer(pt, width = buf_dist)
        
        # Crop and mask raster to buffer
        raster_mask <- terra::mask(terra::crop(raster, buf), buf)
        
        # Counts how many cells in each land cover class
        class_counts <- terra::freq(raster_mask)
        
        class_summary <- data.frame(
          Year = yr,
          Site = site_id,
          Buffer_m = buf_dist,
          Value = class_counts$value,
          Area_m2 = class_counts$count * cell_area
        )
        class_summary$Area_ha <- class_summary$Area_m2 / 10000
        results[[length(results) + 1]] <- class_summary
      }
    }
  }
  
  final_results <- do.call(rbind, results)
  
  # Full set of possible classes
  if (!is.null(class_lookup)) {
    all_values <- class_lookup$Value
  } else {
    all_values <- sort(unique(final_results$Value))
  }
  
  # Every Year x Site x Buffer x Class combination
  full_grid <- expand.grid(
    Year     = years,
    Site     = unique(site_ids),
    Buffer_m = buffer_dists,
    Value    = all_values,
    stringsAsFactors = FALSE
  )
  
  # Left join; unmatched combinations become NA, then 0
  merged <- merge(full_grid, final_results,
                  by = c("Year", "Site", "Buffer_m", "Value"),
                  all.x = TRUE)
  
  merged$Area_m2[is.na(merged$Area_m2)] <- 0
  merged$Area_ha[is.na(merged$Area_ha)] <- 0
  
  merged[order(merged$Year, merged$Site, merged$Buffer_m, merged$Value), ]
}
