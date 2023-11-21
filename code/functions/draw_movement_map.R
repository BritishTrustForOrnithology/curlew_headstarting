


# FUNCTION to draw a static movement map (path or points) for individual birds

draw_movement_map <- function(bird_df, map_type = c("path", "point"), inset_map = TRUE, filter_date = filter_by_date) {
  
  last_date <- max(bird_df$timestamp)
  first_date <- last_date - 60*86400
  
  if (filter_date) {
    bird_df <- bird_df %>% 
      filter(timestamp >= first_date & timestamp < last_date)
  }
  
  num_days_vis <- floor(max(bird_df$timestamp) - min(bird_df$timestamp))
  
  message("Creating inset map for ", b, 
          "\n\nVisualisation runs from ", min(bird_df$timestamp), " to ", max(bird_df$timestamp), " over a time period of ", num_days_vis, " days.........\n\n\n")
  
  # turn bird_df into spatial points
  # bird_df_sf <- bird_df %>% 
  #   sf::st_as_sf(.,
  #                coords = c("location_long",
  #                           "location_lat"),
  #                crs = 4326) %>% 
  #   mutate(lon = st_coordinates(.)[,1],
  #          lat = st_coordinates(.)[,2]) %>% 
  #   arrange(new_datetime)
  
  bird_df_sf <- bird_df
  
  
  bird_df_sf_3857 <- st_transform(bird_df_sf, crs = 3857)
  
  bird_df_sf_3857 <- bird_df_sf_3857 %>% 
    mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
           lat_3857 = st_coordinates(bird_df_sf_3857)[,2])
  
  
  # create basemaps for main & inset maps
  basemap_main <- make_basemap(sf_data = bird_df_sf_3857,
                               buff_dist = 30*1000,
                               map_type = "main",
                               map_provider = "Esri.WorldImagery")
  
  basemap_inset <- make_basemap(sf_data = bird_df_sf_3857,
                                buff_dist = 800*1000,
                                map_type = "inset",
                                map_provider = "Esri.WorldImagery")
  
  
  # # ----- POINTS map -----
  # 
  # if (map_type %in% "point") {
  #   # Plot RGB rasterbrick with points geom on top
  #   gg_main_map <- RStoolbox::ggRGB(basemap_raster, r=1, g=2, b=3) +
  #     geom_sf(data = st_transform(bird_df_sf$geometry, projection(basemap_raster)), fill = "magenta", col = "magenta", size = 0.5) +
  #     geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
  #     coord_sf(crs = projection(basemap_raster)) +
  #     # theme(axis.text.x=element_blank(), #remove x axis labels
  #     #       axis.ticks.x=element_blank(), #remove x axis ticks
  #     #       axis.text.y=element_blank(),  #remove y axis labels
  #     #       axis.ticks.y=element_blank()  #remove y axis ticks
  #     # ) +
  #     theme_void()
  #   
  # }
  
  
  # ----- PATH map  -------
  
  if (map_type %in% "path") {
    
    # Main map
    
    gg_main_map <- basemap_main$map +
      geom_path(data = bird_df_sf_3857, aes(x = lon_3857, y = lat_3857), col = "magenta", linewidth = 0.5) +
      #     geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
      #     coord_sf(crs = projection(basemap_raster)) +
      #     # theme(axis.text.x=element_blank(), #remove x axis labels
      #     #       axis.ticks.x=element_blank(), #remove x axis ticks
      #     #       axis.text.y=element_blank(),  #remove y axis labels
      #     #       axis.ticks.y=element_blank()  #remove y axis ticks
      #     # ) +
          theme_void()
    gg_main_map
    
    
    # Inset map
    
    # create rectangle boundary for outline of the main map region to show on the inset map
    outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))
    
    gg_inset_map <- basemap_inset$map +
      geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
      theme_void()
    gg_inset_map
    
    # use cowplot package to layer ggplots using ggdraw
    cowplot::ggdraw() +
      cowplot::draw_plot(gg_main_map) +
      cowplot::draw_plot(gg_inset_map, -0.1, -0.1, scale = 0.7, width = 0.5, height = 0.5)
    
    dir.create(file.path(outputwd, "path_map"), recursive = TRUE)
    
    ggsave(
      filename = paste0(b, "_gps_static_inset_map_", today_date, "_", filter_by_date, ".png"),
      device="png",
      path = file.path(outputwd, "path_map"),
      # height = 10,
      # width = 12,
      # units = "in",
      # width = 800,
      # units = "px",
      dpi=300
    )
    
  }
    
    # ----- POINTS map  -------
    
    
    if (map_type %in% "points") {
      
      # Main map
      
      gg_main_map <- basemap_main$map +
        geom_sf(data = bird_df_sf_3857, col = "magenta", size = 0.2) +
        #     geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
        #     coord_sf(crs = projection(basemap_raster)) +
        #     # theme(axis.text.x=element_blank(), #remove x axis labels
        #     #       axis.ticks.x=element_blank(), #remove x axis ticks
        #     #       axis.text.y=element_blank(),  #remove y axis labels
        #     #       axis.ticks.y=element_blank()  #remove y axis ticks
        #     # ) +
        theme_void()
      gg_main_map
      
      # Inset map
      
      # create rectangle boundary for outline of the main map region to show on the inset map
      outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))
      
      gg_inset_map <- basemap_inset$map +
        geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
        theme_void()
      gg_inset_map
      
      # use cowplot package to layer ggplots using ggdraw
      cowplot::ggdraw() +
        cowplot::draw_plot(gg_main_map) +
        cowplot::draw_plot(gg_inset_map, -0.1, -0.1, scale = 0.7, width = 0.5, height = 0.5)
      
      dir.create(file.path(outputwd, "points_map"), recursive = TRUE)
      
      ggsave(
        filename = paste0(b, "_gps_static_inset_map_", today_date, "_", filter_by_date, ".png"),
        device="png",
        path = file.path(outputwd, "points_map"),
        # height = 10,
        # width = 12,
        # units = "in",
        # width = 800,
        # units = "px",
        dpi=300
      )
  
    }
  
  
}