


# FUNCTION to draw a static movement map (path or points) for all birds together

draw_movement_map_all_birds <- function(bird_df_sf, map_type = c("path", "points", "path points"), filter_date = filter_by_date, basemap_alpha, out_type = c("png", "jpg"), map_dpi, map_buffer_km = 30) {
  
  # set file output
  if (wash_obs_only) out_file <- paste0("all_birds_gps_static_map_WASH")
  if (!wash_obs_only) out_file <- paste0("all_birds_gps_static_map")
  out_file <- paste0(out_file, "_", today_date, ".", out_type)
  
  num_days_vis <- floor(max(bird_df_sf$timestamp) - min(bird_df_sf$timestamp))
  
  message("Creating static map for all birds", 
          "\n\nVisualisation runs from ", min(bird_df_sf$timestamp), " to ", max(bird_df_sf$timestamp), " over a time period of ", num_days_vis, " days.........\n\n\n")
  
  # transform data from epsg 4326 (move2 object default) to epsg 3857 (units in metres)
  # units in metres easier for adding buffer to bounding box
  # 3857 is projected CRS for rendering tile mapping
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
  
  # basemap_inset <- make_basemap(sf_data = bird_df_sf_3857,
  #                               buff_dist = 800*1000,
  #                               map_type = "inset",
  #                               map_provider = "Esri.WorldImagery")
  
  
  # ----- Map aesthetics  -------
  
  xlims <- c(st_bbox(basemap_main$geom)[1], st_bbox(basemap_main$geom)[3])
  ylims <- c(st_bbox(basemap_main$geom)[2], st_bbox(basemap_main$geom)[4])
  
  # ----- PATH map  -------
  
  
  
  if (map_type %in% "path") {
    
    # Main map
    
    # Path + terminus point coloured
    gg_main_map <- basemap_main$map +
      geom_path(data = bird_df_sf_3857, aes(x = lon_3857, y = lat_3857, col = flag_id), linewidth = 0.5) +
      geom_sf(data = bird_df_sf_3857 %>% group_by(flag_id) %>% 
                slice(which.max(timestamp)), size = 1.5, aes(colour = flag_id)) +
      scale_colour_viridis_d(name = "Flag code", option = "magma") +
      coord_sf(xlim = xlims, ylim = ylims) #+
      # theme_void()
    # gg_main_map
      
      # Terminus point white outline
      gg_main_map <- gg_main_map +
        geom_sf(data = bird_df_sf_3857 %>% group_by(flag_id) %>%
                  slice(which.max(timestamp)), colour="white", shape=21, size = 1.5, stroke = 0.2) #+
      # coord_sf(xlim = xlims, ylim = ylims) #+
      # theme_void()
      # gg_main_map
    
    
    # # Inset map
    # 
    # # create rectangle boundary for outline of the main map region to show on the inset map
    # outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))
    # 
    # gg_inset_map <- basemap_inset$map +
    #   geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
    #   theme_void()
    # # gg_inset_map
    # 
    # # use cowplot package to layer ggplots using ggdraw
    # cowplot::ggdraw() +
    #   cowplot::draw_plot(gg_main_map) +
    #   cowplot::draw_plot(gg_inset_map, -0.05, -0.1, scale = 0.7, width = 0.5, height = 0.5)
    
    map_dir <- file.path(outputwd, "path_map", paste("filter_by_date", filter_date, sep="_"))
    dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
    
    ggsave(
      gg_main_map,
      filename = out_file,
      device=out_type,
      path = map_dir,
      # height = 10,
      # width = 12,
      # units = "in",
      # width = 800,
      # units = "px",
      dpi=map_dpi
    )
    
  }
  
  # ----- POINTS map  -------
  
  
  if (map_type %in% "points") {
    
    # Main map
    
    gg_main_map <- basemap_main$map +
      geom_sf(data = bird_df_sf_3857, aes(col = flag_id), size = 0.2) +
      scale_colour_viridis_d(name = "Flag code", option = "magma") +
      guides(colour = guide_legend(override.aes = list(size = 3))) +
      coord_sf(xlim = xlims, ylim = ylims) +
      theme_void()
    # gg_main_map
    
    # # Inset map
    # 
    # # create rectangle boundary for outline of the main map region to show on the inset map
    # outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))
    # 
    # gg_inset_map <- basemap_inset$map +
    #   geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
    #   theme_void()
    # # gg_inset_map
    # 
    # # use cowplot package to layer ggplots using ggdraw
    # cowplot::ggdraw() +
    #   cowplot::draw_plot(gg_main_map) +
    #   cowplot::draw_plot(gg_inset_map, -0.05, -0.1, scale = 0.7, width = 0.5, height = 0.5)
    
    map_dir <- file.path(outputwd, "points_map", paste("filter_by_date", filter_date, sep="_"))
    dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
    
    ggsave(
      filename = out_file,
      device=out_type,
      path = map_dir,
      # height = 10,
      # width = 12,
      # units = "in",
      # width = 800,
      # units = "px",
      dpi=map_dpi
    )
    
  }
  
  # ----- POINTS map  -------
  
  
  if (map_type %in% "path points") {
    
    # # plot with year_as_factor as colours
    # gg_main_map <- basemap_main$map +
    #   geom_path(data = bird_df_sf_3857, aes(x = lon_3857, y = lat_3857, col = year_as_factor), linewidth = 0.5) +
    #   geom_sf(data = bird_df_sf_3857, aes(col = year_as_factor), size = 0.5) +
    #   scale_colour_viridis_d(name = "Year", option = "viridis") +
    #   coord_sf(xlim = xlims, ylim = ylims) +
    #   theme_void()
    
    # plot with flag_id as colours
    gg_main_map <- basemap_main$map +
      geom_path(data = bird_df_sf_3857, aes(x = lon_3857, y = lat_3857, col = flag_id), linewidth = 0.5) +
      geom_sf(data = bird_df_sf_3857, aes(col = flag_id), size = 0.5) +
      scale_colour_viridis_d(name = "Flag code", option = "magma") +
      guides(colour = guide_legend(override.aes = list(size = 3))) +
      coord_sf(xlim = xlims, ylim = ylims) +
      theme_void()
    
    # Main map
    
    # gg_main_map <- gg_main_map +
    #   geom_sf(data = bird_df_sf_3857, aes(col = flag_id), size = 0.2) +
    #   scale_colour_viridis_d(name = "", option = "magma") +
    #   coord_sf(xlim = xlims, ylim = ylims) +
    #   theme_void()
    # gg_main_map
    
    # # Inset map
    # 
    # # create rectangle boundary for outline of the main map region to show on the inset map
    # outline_box_main <- st_as_sfc(st_bbox(basemap_main$geom))
    # 
    # gg_inset_map <- basemap_inset$map +
    #   geom_sf(data = outline_box_main, fill = NA, color = "white", size = 0.7) +  
    #   theme_void()
    # # gg_inset_map
    # 
    # # use cowplot package to layer ggplots using ggdraw
    # cowplot::ggdraw() +
    #   cowplot::draw_plot(gg_main_map) +
    #   cowplot::draw_plot(gg_inset_map, -0.05, -0.1, scale = 0.7, width = 0.5, height = 0.5)
    
    map_dir <- file.path(outputwd, "path_points_map", paste("filter_by_date", filter_date, sep="_"))
    dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
    
    ggsave(
      filename = out_file,
      device=out_type,
      path = map_dir,
      height = 200,
      width = 350,
      units = "mm",
      # width = 800,
      # units = "px",
      dpi=map_dpi
    )
    
  }
  
  
}