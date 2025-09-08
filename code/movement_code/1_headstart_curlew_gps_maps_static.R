##############################
#
#    NE103: Headstarted Curlew movement data
#
#
##############################

# Code to create visualisations of GPS tracking data


# Static maps


# Uses 0_hadstart_curlew_gps_maps_data_prep.R to prepare dataset for mapping


# =======================    Static maps   =================

# # Quick look at all tracks together Wash-area
# ggplot() +
#   geom_sf(data = ne_coastline(returnclass = "sf", 10)) +
#   theme_linedraw() +
#   geom_sf(data = all_tags_filtered) +
#   geom_sf(data = mt_track_lines(all_tags_filtered), aes(color = `individual_local_identifier`)) +
#   coord_sf(xlim = c(0.1, 1),
#            ylim = c(52.6, 53.2)
#   )


# List of birds to create maps for  -----------------

bird_flag_list <- unique(all_tags_filtered$flag_id)
bird_flag_list


# Static visualisation with inset maps   -----------------

if (map_all_birds_together) {
  
  # path map
  draw_movement_map_all_birds(all_tags_filtered, map_type = "path", basemap_alpha = 0.8, out_type = "jpg", map_dpi = 150, map_buffer_km = 5)
  
  # point map
  draw_movement_map_all_birds(all_tags_filtered, map_type = "points", basemap_alpha = 0.8, out_type = "jpg", map_dpi = 150, map_buffer_km = 5)
  
  # path + point map
  draw_movement_map_all_birds(all_tags_filtered, map_type = "path points", basemap_alpha = 0.8, out_type = "jpg", map_dpi = 150, map_buffer_km = 5)
  
} else {
  
  for (b in bird_flag_list) {
    
    # filter movement data to site, cohort
    individual_df <- all_tags_filtered %>% 
      filter(flag_id %in% b)
    
    # skip to next bird if no data from the last month
    if (nrow(individual_df) < 1) next
    
    # map aesthetics can be controlled by function arguments
    # filter_date = TRUE will filter to last 60 days of data
    # map_colour = colour of path / points
    # basemap_alpha = alpha level of the main basemap
    # out_type = image file output type (png or jpg)
    # map_dpi = DPI of image output
    # map_buffer_km = basemap buffer around GPS track data, in km
    # path_alpha = alpha level of the path
    
    # path map
    draw_movement_map(individual_df, map_type = "path", filter_date = filter_by_date, map_colour="magenta", basemap_alpha = 1, out_type = "png", map_dpi = 150, map_buffer_km = 10, path_alpha = 0.5)
    
    # point map
    draw_movement_map(individual_df, map_type = "points", filter_date = filter_by_date, map_colour="magenta", basemap_alpha = 1, out_type = "png", map_dpi = 150, map_buffer_km = 10)
    
    # path + point map
    draw_movement_map(individual_df, map_type = "path points", filter_date = filter_by_date, map_colour="magenta", basemap_alpha = 1, out_type = "jpg", map_dpi = 150, map_buffer_km = 10, path_alpha = 0.5)
    
    
    
  }
  
}


# plot all obs for only the Wash area polygon

if (wash_obs_only) {
  
  # bounding box polygon around the Wash / North Norfolk coast
  #gis_wash_dir <- file.path("../../GIS/curlew/headstarting") # Sam's computer GIS filepath
  gis_wash_dir <- file.path("../GIS/curlew/wwrg/") # Katharine's computer GIS path
  
  # Load WWRG Wash study shapefile -----------------
  #wash_area <- st_read(file.path(gis_wash_dir, "wash_north_norfolk_study_area_polygon.shp"))
  wash_area <- st_read(file.path(gis_wash_dir, "wwrg_wash_study_area_polygon.shp"))
  
  if (map_all_birds_together) {
    
  # filter all_tags_points (sf object)
  # clip to only those points falling within the Wash study area
  bird_df_sf <-  all_tags_filtered %>% 
    # filter(flag_id %in% bird_flag_list) %>%
    st_intersection(., wash_area) %>% 
    mutate(year_as_factor = as.factor(year))
    
  # should not be mapped as path or path/points with terminus point as this gives false impression that it is the 'end of the track'
  # map only as points
  draw_movement_map_all_birds(bird_df_sf, map_type = "points", filter_date = filter_by_date, basemap_alpha = 0.8, out_type = "png", map_dpi = 150, map_buffer_km = 1)
    
  } else {
    
    for (b in bird_flag_list) {
      
      # filter all_tags_points (sf object)
      # clip to only those points falling within the Wash study area
      bird_df_sf <-  all_tags_filtered %>% 
        filter(flag_id %in% bird_flag_list) %>%
        st_intersection(., wash_area) %>% 
        mutate(year_as_factor = as.factor(year))
      
      # skip to next bird if no data
      if (nrow(bird_df_sf) < 1) next
      
      # map aesthetics can be controlled by function arguments
      # filter_date = TRUE will filter to last 60 days of data
      # map_colour = colour of path / points
      # basemap_alpha = alpha level of the main basemap
      # out_type = image file output type (png or jpg)
      # map_dpi = DPI of image output
      # map_buffer_km = basemap buffer around GPS track data, in km
      # path_alpha = alpha level of the path
      
      # point map
      draw_movement_map(bird_df_sf, map_type = "points", filter_date = filter_by_date, map_colour="magenta", basemap_alpha = 1, out_type = "png", map_dpi = 150, map_buffer_km = 1)

    }
  }
  
  
  
}


