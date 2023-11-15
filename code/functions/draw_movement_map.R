


# FUNCTION to draw a static movement map (path or points) for individual birds

draw_movement_map <- function(bird_df, map_type = c("path", "point"), inset_map = TRUE, filter_date = filter_by_date) {
  
  if (filter_date) {
    bird_df <- bird_df %>% 
      filter(new_datetime >= ymd(first_date) & new_datetime < ymd(last_date))
  }
  
  num_days_vis <- floor(max(bird_df$new_datetime) - min(bird_df$new_datetime))
  
  message("Creating inset map for ", b, 
          "\n\nVisualisation runs from ", min(bird_df$new_datetime), " to ", max(bird_df$new_datetime), " over a time period of ", num_days_vis, " days.........\n\n\n")
  
  # turn bird_df into spatial points
  bird_df_sf <- bird_df %>% 
    sf::st_as_sf(.,
                 coords = c("location_long",
                            "location_lat"),
                 crs = 4326) %>% 
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2]) %>% 
    arrange(new_datetime)
  
  bird_df_sf_3857 <- st_transform(bird_df_sf, crs = 3857)
  
  bird_df_sf <- bird_df_sf %>% 
    mutate(lon_3857 = st_coordinates(bird_df_sf_3857)[,1],
           lat_3857 = st_coordinates(bird_df_sf_3857)[,2])
  
  current_extent <- st_bbox(bird_df_sf)
  
  set_extent <- st_bbox(c(current_extent[1] - 0.5,
                          current_extent[2] - 0.2, 
                          current_extent[3] + 0.5,
                          current_extent[4] + 0.2), crs = st_crs(4326))
  
  set_extent_inset <- st_bbox(c(current_extent[1] - 7,
                                current_extent[2] - 4, 
                                current_extent[3] + 7,
                                current_extent[4] + 4), crs = st_crs(4326))
  
  # set defaults for the basemap
  # set_defaults(map_service = "mapbox", map_type = "hybrid", map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", map_dir = file.path("gis"))
  set_defaults(map_service = "esri", map_type = "world_imagery", map_dir = file.path("gis"))
  
  # Plot inset map wider area
  basemap_raster_inset <- basemap_raster(set_extent_inset)
  # plotRGB(basemap_raster_inset)
  
  # Plot main map raster
  basemap_raster <- basemap_raster(set_extent)
  # plotRGB(basemap_raster)
  
  
  # # Plot png file
  # png(file.path(outputwd, paste0(b, "_static_inset_map_", today_date, ".png")), width = 1200, units = "px", res = 300, pointsize = 14)
  # # raster basemap
  
  # plotRGB(basemap_raster_inset, add = TRUE)
  # plot(st_transform(bird_df_sf$geometry, projection(basemap_raster)), add = TRUE, col = "orangered", bg = "orange", pch = 21, cex = 0.2, lwd = 0.2)
  # dev.off()
  
  
  # ----- POINTS map -----
  
  if (map_type %in% "point") {
    # Plot RGB rasterbrick with points geom on top
    gg_main_map <- RStoolbox::ggRGB(basemap_raster, r=1, g=2, b=3) +
      geom_sf(data = st_transform(bird_df_sf$geometry, projection(basemap_raster)), fill = "magenta", col = "magenta", size = 0.5) +
      geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
      coord_sf(crs = projection(basemap_raster)) +
      # theme(axis.text.x=element_blank(), #remove x axis labels
      #       axis.ticks.x=element_blank(), #remove x axis ticks
      #       axis.text.y=element_blank(),  #remove y axis labels
      #       axis.ticks.y=element_blank()  #remove y axis ticks
      # ) +
      theme_void()
    
  }
  
  
  # ----- PATH map  -------
  
  if (map_type %in% "path") {
    # Plot RGB rasterbrick with path geom on top
    gg_main_map <- RStoolbox::ggRGB(basemap_raster, r=1, g=2, b=3) +
      geom_path(data = bird_df_sf, aes(x = lon_3857, y = lat_3857), col = "magenta", size = 1) +
      geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
      coord_sf(crs = projection(basemap_raster)) +
      # theme(axis.text.x=element_blank(), #remove x axis labels
      #       axis.ticks.x=element_blank(), #remove x axis ticks
      #       axis.text.y=element_blank(),  #remove y axis labels
      #       axis.ticks.y=element_blank()  #remove y axis ticks
      # ) +
      theme_void()
    
  }
  
  
  # Create rectangle bounding box for buffered area of the zoomed in basemap raster
  # Buffer by 20000m
  bb <- st_bbox(basemap_raster)
  set_extent_buffer <- st_bbox(c(bb[1] - 20000,
                                 bb[2] - 20000, 
                                 bb[3] + 20000,
                                 bb[4] + 20000), crs = projection(basemap_raster))
  set_extent_buffer <- st_as_sfc(set_extent_buffer)
  
  # create rectangle boundary for outline of the inset map
  outline_box_inset <- st_as_sfc(st_bbox(basemap_raster_inset))
  
  # create inset map
  # plot inset raster basemap using ggplot RGB conversion
  # plot zoomed in basemap bounding box (white rectangle)
  # plot black outline around inset map
  gg_inset_map <-  RStoolbox::ggRGB(basemap_raster_inset, r=1, g=2, b=3) +
    # geom_sf(data = points_bb, fill = NA, color = "white", size = 0.7) +
    geom_sf(data = set_extent_buffer, fill = NA, color = "white", size = 0.7) +  
    geom_sf(data = outline_box_inset, fill = NA, color = "black", size = 1) +
    theme_void()
  
  # use cowplot package to layer ggplots using ggdraw
  if (inset_map) {
    cowplot::ggdraw() +
      cowplot::draw_plot(gg_main_map) +
      cowplot::draw_plot(gg_inset_map, -0.08, -0.08, scale = 0.7, width = 0.5, height = 0.5)
  } else {
    cowplot::ggdraw() +
      cowplot::draw_plot(gg_main_map)
  }
  
  ggsave(
    filename = paste0(b, "_static_inset_map_", today_date, "_", map_type, "_", filter_by_date,".png"),
    device="png",
    path = outputwd,
    # height = 10,
    # width = 12,
    # units = "in",
    # width = 800,
    # units = "px",
    dpi=300
  )
  
}