


# FUNCTION to draw a static movement map (path or points) for individual birds

draw_movement_map_to_object <- function(bird_df_sf, basemap_alpha, map_colour, map_buffer_km = 30) {
  
  bird_df_sf_3857 <- st_transform(bird_df_sf, crs = 3857)
  
  bird_df_sf_3857 <- bird_df_sf_3857 %>% 
    mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
           lat_3857 = st_coordinates(bird_df_sf_3857)[,2])
  
  
  # create basemaps for main & inset maps
  basemap_main <- make_basemap(sf_data = bird_df_sf_3857,
                               buff_dist = map_buffer_km*1000,
                               map_type = "main",
                               map_provider = "Esri.WorldImagery",
                               alpha_level = basemap_alpha)
  
  
  # ----- Map aesthetics  -------
  
  xlims <- c(st_bbox(basemap_main$geom)[1], st_bbox(basemap_main$geom)[3])
  ylims <- c(st_bbox(basemap_main$geom)[2], st_bbox(basemap_main$geom)[4])
  
  
  # ----- POINTS map  -------
  
  # Main map
  
  gg_main_map <- basemap_main$map +
    geom_sf(data = bird_df_sf_3857, col = map_colour, size = 0.2) +
    coord_sf(xlim = xlims, ylim = ylims) +
    theme_void()
  # gg_main_map
  
  return(gg_main_map)
  
}