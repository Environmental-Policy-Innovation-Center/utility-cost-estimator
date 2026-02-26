## OSRM GET NEIGHBORS ###


```{r}
get_neighbors <- function(sabs, cutoff){
  # sf_use_s2(FALSE)  # Avoid spherical geometry warnings
  
  message("Calculating centroids...")
  centroids <- st_centroid(sabs$geometry)
  centroid_distance_matrix <- st_distance(centroids, centroids)
  
  message("Processing ", nrow(sabs), " pwsids...")
  
  df <- sabs %>%
    mutate(rec = map(1:n(), function(i) {
      if(i %% 10 == 0) message("  Processing pwsid ", i, " of ", n())
      
      # Find neighbors within cutoff distance
      centroid_distances <- as.numeric(centroid_distance_matrix[i, ]) * 0.000621371
      matches <- tibble(
        pwsid = pwsid[-i],
        centroid_distance = centroid_distances[-i]
      ) %>%
        filter(centroid_distance <= cutoff)
      
      if(nrow(matches) == 0) {
        return(tibble(pwsid = character(), centroid_distance = numeric(), 
                      travel_distance = numeric(), overlap = logical()))
      }
      
      # Get parent and child geometries
      parent_geom <- sabs$geometry[i]
      child_geoms <- sabs$geometry[sabs$pwsid %in% matches$pwsid]
      print("finished calcing centriods")
      # Calculate travel distances
      travel_table <- osrmTable(
        src = st_centroid(parent_geom), 
        dst = st_centroid(child_geoms), 
        measure = "distance"
      )
      
      print("finished calcing travel distance")
      
      # Check overlaps and add results
      matches %>%
        mutate(
          travel_distance = as.numeric(travel_table$distances) * 0.000621371,
          overlap = st_overlaps(parent_geom, child_geoms, sparse = FALSE)[1,],
        ) %>%
        filter(travel_distance <= cutoff)
    })) %>%
    mutate(rec_num_neighbors = map_int(rec, nrow))
  
  message("Unnesting results...")
  df <- unnest(df, rec, names_sep = "_")
  
  
  ## TO DO 
  ## Clean up this variable rename to just use the rec system prefix 
  df <- df %>% 
    left_join(.,sabs %>% data.frame() %>% select(pwsid,num_facilities )%>% rename(rec_pwsid = pwsid,rec_num_facilities = num_facilities))%>%
    mutate(rec_num_facilities = ifelse(is.na(rec_num_facilities),0,rec_num_facilities))
  
  message("Complete! Found neighbors for ", nrow(df), " pairs")
  return(df)
}
```

