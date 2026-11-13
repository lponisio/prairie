## This function calculates connectivity indexes proposedby Saura & Pascual-Hortal 2007
##for each site and year, it crops a buffer window around the site's centroid, 
## reclassifies the landscape into a resistance surface 
## (using predetermined per-class resistance values from literature or expert judgement), 
## delineates all grassland patches within that window, 
## computes the Probability of Connectivity index across those patches using 
## resistance-weighted (least-cost) effective distances, 
## and then reports back just the connectivity contribution of 
## the one patch that contains that site's own centroid

## Makurhini R package Godinez-Gomez et al. 2006

## dPC = How much does this specific site's grassland 
## patch benefit from the surrounding network. how much other grassland exists nearby, 
## and how easy the intervening landscape is to cross to reach it

## Create resistance table
## the resistance per class type will require expert judgement or
## review of literature to support resistance decision
resistance_table <- data.frame(
  Value = c(1, 2, 3, 4, 5, 6, 7, 8),
  Class_name = c("Deciduous Forest", "Evergreen Forest", "Shrubland", 
                 "Grassland/Herbaceous", "Planted / Cultivated", "Pasture/Hay",
                 "Developed/Barren", "Wetlands"),
  Resistance = c(50, 60, 20, 1, 50, 40, 100, 15))

calculateSitePC <- function(rasters, centroids, years,        # main inputs: yearly rasters, site points, years to run
                            analysis_radius,           # the true radius (m) you care about around each site
                            edge_buffer = 500,                # extra padding so patches aren't cut off at the window edge
                            resistance_table,                 # lookup table: land-cover class -> resistance value
                            habitat_classes,                  # class code(s) that count as habitat/nodes (grassland)
                            site_field = "MERGE_SRC",         # column in centroids holding each site's name/ID
                            distance_thresholds = 500,         # dispersal distance (m) defining the PC probability function
                            probability = 0.5,                # probability of connection at distance_thresholds
                            distance_type = "least-cost",      # how effective distance is computed (vs. Euclidean)
                            min_patch_ha = 0.5) {              # smallest patch size (ha) to keep; drops tiny slivers
  # actual crop radius = true radius + padding, to avoid edge truncation
  # empty container to collect one result row per site-year
  extract_radius <- analysis_radius + edge_buffer   
  results <- list()                                 
  
  # loop 1: step through each year
  # grab that year's land-cover raster from the list
  for (yr in years) {                               
    lc <- rasters[[as.character(yr)]]   
    
  # loop 2: step through each site (row) in centroids
  # pull this site's name/ID out of the specified column
  # grab this site's single point geometry
  # ensure it's an sf object (needed for st_buffer/st_intersects)
    for (i in seq_len(nrow(centroids))) {            
      site_id <- as.data.frame(centroids)[i, site_field]  
      pt <- centroids[i, ]                           
      pt_sf <- st_as_sf(pt)                          
      
      # draw a circle of extract_radius around the site
      # cut the raster down to just that circle
      buf_extract <- st_buffer(pt_sf, dist = extract_radius)  
      lc_crop <- crop(lc, vect(buf_extract), mask = TRUE)     
      
      # check: is the whole cropped window empty (e.g. near raster edge)?
      # log why this site/year is being skipped
      # skip to the next site - nothing to analyze here
      if (all(is.na(values(lc_crop)))) {             
        message(site_id, " ", yr, ": SKIPPED - no raster data in window")  
        next                                          
      }
      
      # reclassify the cropped land-cover raster
      # into resistance values per class
      # build a habitat/not-habitat raster:
      # habitat_classes (grassland) -> 1
      # everything else -> NA (so it's excluded from patches)
      resistance_crop <- classify(lc_crop,             
                                  rcl = as.matrix(resistance_table[, c("Value", "Resistance")]))  
      habitat_binary <- classify(lc_crop, rcl = matrix(c(  
        habitat_classes, rep(1, length(habitat_classes))    
      ), ncol = 2), others = NA)                             
      
      # group touching (incl. diagonal) habitat cells into patch IDs
      # check: is there any grassland at all in this window?
      # log the reason
      # skip - no patches means nothing to connect
      patches_r <- patches(habitat_binary, directions = 8)   
      
      if (all(is.na(values(patches_r)))) {            
        message(site_id, " ", yr, ": SKIPPED - no grassland patches found")  
        next                                           
      }
      
      # convert patch raster -> vector polygons (sf)
      # compute each patch's area in hectares
      # drop patches smaller than the minimum size
      patches_poly <- st_as_sf(as.polygons(patches_r, dissolve = TRUE))  
      patches_poly$area_ha <- as.numeric(st_area(patches_poly)) / 10000  
      patches_poly <- patches_poly[patches_poly$area_ha > min_patch_ha, ]  
      
      # check: are there at least 2 patches left to connect?
      # log reason
      # skip - can't compute connectivity with 0 or 1 patch
      if (nrow(patches_poly) < 2) {                    
        message(site_id, " ", yr, ": SKIPPED - fewer than 2 grassland patches (n=", nrow(patches_poly), ")")  
        next                                            
      }
      
      # find which patch polygon contains the site's centroid
      # check: did the centroid actually land inside a kept patch?
      # log reason
      # skip - can't report a site-specific score with no matching patch
      site_patch_idx <- st_intersects(pt_sf, patches_poly)[[1]]  
      
      if (length(site_patch_idx) == 0) {               
        message(site_id, " ", yr, ": SKIPPED - site centroid does not fall within a retained grassland patch")  
        next                                            
      }
      
      # attempt the connectivity calculation, catching any errors safely
      # run PC across all patches in this window
      # the grassland patches (nodes) to connect
      # area units for patch size in the output
      pc_result <- tryCatch({                          
        result_list <- MK_dPCIIC(                      
          nodes = patches_poly,                        
          area_unit = "ha",                             
          distance = list(type = distance_type, resistance = resistance_crop),  # use least-cost distance through resistance surface
          metric = "PC",                                # use Probability of Connectivity (not IIC)
          probability = probability,                    # dispersal probability at distance_thresholds
          distance_thresholds = distance_thresholds,     # the distance defining that probability
          overall = TRUE                                 # also compute the landscape-wide PC value
        )
        # if MK_dPCIIC() throws an error..
        # ...print the actual error message
        # ...and return NULL instead of crashing the loop
        result_list[[1]]                                 
      }, error = function(e) {                            .
        message(site_id, " ", yr, ": ERROR - ", conditionMessage(e))  
        NULL                                              
      })
      
      # if the calculation failed, skip to the next site
      if (is.null(pc_result)) next                       
      
      # extract just the row for this site's own grassland patch
      # tag it with the site name/ID
      # tag it with the year
      # tag it with the buffer radius used
      site_row <- pc_result[site_patch_idx, ]              
      site_row$site_id <- site_id                          
      site_row$Year <- yr                                  
      site_row$Buffer_m <- analysis_radius                 
      
      # append this site-year's result onto the results list
      results[[length(results) + 1]] <- site_row           
    }
  }
  # stack every site-year result into one final table
  do.call(rbind, results)                                  
}