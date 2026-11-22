## This function calculates pyrodiversity
calcPyrodiversity <- function(sf_list, site, window, buffers,
                              min_area = 30, pattern) {
  
  unburned_hist <- strrep("0", length(window))
  
  site_list <- sf_list[grepl(pattern, names(sf_list), ignore.case = TRUE)]
  
  ## Stack polygons and pull years
  site_all <- site_list %>%
    map(~ st_sf(geometry = st_geometry(.x))) %>%
    bind_rows(.id = "source_file") %>%
    mutate(yr = as.numeric(str_extract(source_file, "\\d+$")),
           year = if_else(yr < 100, 2000 + yr, yr))
  
  ## Merge polygons within each year
  site_yr <- site_all %>% 
    filter(year %in% window) %>% 
    st_make_valid() %>% 
    group_by(year) %>% 
    summarise(geometry = st_union(geometry), .groups = "drop") %>% 
    st_as_sf()
  
  print(site_yr$year)
  
  if(nrow(site_yr) == 0) return (NULL) ## returns NULL for year without burn
  
  ## Split into non-overlapping burn fragments
  site_freq <- st_intersection(site_yr) %>% 
    select(-year) %>% 
    mutate(
      burn_hist = map_chr(origins, ~ paste(as.integer(window %in% site_yr$year[.x]), collapse = "")),
      years_burned = map_chr(origins, ~ paste(site_yr$year[.x], collapse = ", ")),
      area_sqm = as.numeric(st_area(geometry))
    )
  
  ## Create centroid of polygons after union
  site_centroid <- site_all %>% st_make_valid() %>% st_union() %>% st_centroid()
  
  results <- list()
  
  buffers_list <- list()
  
  for (b in buffers) {
    
    ## Buffer around the single site centroid
    study_area <- st_buffer(site_centroid, b)
    
    ## Save buffer and centroid for plotting
    buffers_list[[as.character(b)]] <- st_sf(buffer_m = b, geometry = st_geometry(study_area))
    
    ## Clip burn fragments to the circle, keep polygons only, drop slivers
    freq_b <- st_intersection(site_freq, study_area)
    if (nrow(freq_b) > 0) {
      freq_b <- freq_b %>%
        st_collection_extract("POLYGON") %>%
        mutate(area_sqm = as.numeric(st_area(geometry))) %>%
        filter(area_sqm >= min_area)
    }
    
    total_burned_sqm <- sum(freq_b$area_sqm)
    
    ## Unburned = circle minus all burned fragments (empty if fully burned)
    unburned_geom <- if (nrow(freq_b) > 0) {
      st_difference(study_area, st_union(freq_b))
    } else {
      study_area
    }
    
    unburned_sqm <- if (length(unburned_geom) == 0 || all(st_is_empty(unburned_geom))) {
      0
    } else {
      as.numeric(sum(st_area(unburned_geom)))
    }
    
    ## Add unburned row only if there is unburned area of at least min_area
    if (unburned_sqm >= min_area) {
      unburned_row <- st_sf(burn_hist    = unburned_hist,
                            years_burned = "none",
                            area_sqm     = unburned_sqm,
                            geometry     = st_geometry(unburned_geom))
      freq_b <- bind_rows(freq_b, unburned_row)
    }
    
    ## Shannon index across burn histories (Ulyshen et al. 2023)
    burn_summary <- freq_b %>%
      st_drop_geometry() %>%
      summarise(area_sqm = sum(area_sqm), .by = burn_hist) %>%
      mutate(proportion = area_sqm / sum(area_sqm))
    
    shannon <- -sum(burn_summary$proportion * log(burn_summary$proportion))
    
    results[[as.character(b)]] <- freq_b %>%
      mutate(site = site, buffer_m = b,
             area_sqm = round(area_sqm, 2),
             total_burned_sqm = round(total_burned_sqm, 2),
             pyrodiversity = shannon) %>%
      select(site, buffer_m, burn_hist, years_burned, area_sqm,
             total_burned_sqm, pyrodiversity)
  }
  list(
    burns    = bind_rows(results),
    buffers  = bind_rows(buffers_list),
    centroid = st_sf(site = site, geometry = st_geometry(site_centroid))
  )
}

plotBurnFreq <- function(burn_data, buffer_data, centroid_data, prairie, limit) {
  ggplot() +
    geom_sf(data = burn_data %>%  filter(site == prairie),
            aes(fill = factor(burn_frequency)),
            color = "grey20",
            linewidth = 0.25,
            alpha = 0.8,
            show.legend = TRUE) +
    geom_sf(data = buffer_data %>% filter(site == prairie, buffer_m == "1000"),
            fill = NA,
            color = "black",
            linewidth = 0.7) +
    geom_sf(data = buffer_data %>% filter(site == prairie, buffer_m == "500"),
            fill = NA,
            color = "black",
            linewidth = 0.7) +
    geom_sf(data = buffer_data %>% filter(site == prairie, buffer_m == "250"),
            fill = NA,
            color = "black",
            linewidth = 0.7) +
    geom_sf(data = centroid_data %>%  filter(site == prairie),
            shape = 21,
            fill = "black",
            color = "black",
            size = 2,
            stroke = 0.5) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6, "Reds"),
                      limits = as.character(limit), drop = FALSE,
                      name = "Number of Burns") +
    labs(title = prairie) +
    theme_void() +
    theme(legend.position = "right",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
}
