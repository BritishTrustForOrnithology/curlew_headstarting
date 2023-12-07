


# FUNCTION to draw a basemap
# dependencies = sf, maptiles, tidyterra

# sf_data = spatial points or polygon
# buff_dist_main = buffer distance (in metres) around sf_data for giving main map some padding; 30km about right
# buff_dist_inset = buffer distance around sf_data for wider context; 500-800km about right
# maptype = "main" for main map showing resightings, or "inset" for showing wider geographic context
# map_provider = see ?maptiles::get_tiles for list of providers
make_basemap <- function(sf_data, buff_dist, map_type, map_provider, alpha_level = 1) {
  
  # Set the bounding box for the basemap
  
  if (map_type %in% c("main", "inset")) {
    
    if (map_type %in% "main") {
      
      # set a buffer on the current set of points extent
      mapzone_3857 <- st_buffer(sf_data, dist = buff_dist)
      
      # determine current bounding box width & height (in metres)
      dt_bbox <- st_bbox(mapzone_3857)
      width <- dt_bbox$xmax - dt_bbox$xmin
      height <- dt_bbox$ymax - dt_bbox$ymin
      aspect <- as.numeric(height/width)
      
      # first make bbox square
      if(aspect>1) { # make wider
        offset  <- 0.5 * (dt_bbox$xmax - dt_bbox$xmin)
        centre <- dt_bbox$xmin + offset
        dt_bbox[1] <- centre - (offset * aspect)
        dt_bbox[3] <- centre + (offset * aspect)
      }
      if(aspect<1) { # make taller
        offset  <- 0.5 * (dt_bbox$ymax - dt_bbox$ymin)
        centre <- dt_bbox$ymin + offset
        dt_bbox[2] <- centre - (offset / aspect)
        dt_bbox[4] <- centre + (offset / aspect)
      }
      
      # then make width 1.5 times the height for main map
      offset  <- 0.5 * (dt_bbox$xmax - dt_bbox$xmin)
      centre <- dt_bbox$xmin + offset
      dt_bbox[1] <- centre - (offset * 1.5)
      dt_bbox[3] <- centre + (offset * 1.5)
      
      # bbox for basemap
      basemap_bbox <- st_as_sfc(dt_bbox)
      
    }
    
    if (map_type %in% "inset") {
      
      # set a buffer on the current set of points extent
      mapzone_3857 <- st_buffer(sf_data, dist = buff_dist)
      
      # determine current bounding box width & height (in metres)
      dt_bbox <- st_bbox(mapzone_3857)
      width <- dt_bbox$xmax - dt_bbox$xmin
      height <- dt_bbox$ymax - dt_bbox$ymin
      aspect <- as.numeric(height/width)
      
      # make bbox square
      if(aspect>1) { # make wider
        offset  <- 0.5 * (dt_bbox$xmax - dt_bbox$xmin)
        centre <- dt_bbox$xmin + offset
        dt_bbox[1] <- centre - (offset * aspect)
        dt_bbox[3] <- centre + (offset * aspect)
      }
      if(aspect<1) { # make taller
        offset  <- 0.5 * (dt_bbox$ymax - dt_bbox$ymin)
        centre <- dt_bbox$ymin + offset
        dt_bbox[2] <- centre - (offset / aspect)
        dt_bbox[4] <- centre + (offset / aspect)
      }
      
      # bbox for basemap
      basemap_bbox <- st_as_sfc(dt_bbox)
      
    }
    
  } else {
    stop(map_type, " is not a valid map type")
  }
  
  
  # Get basemap tiles (package 'maptiles' and 'tidyterra')
  
  #convert to 4326 for getting bbox for zoom level
  bbox_4326 <- st_transform(basemap_bbox, 4326)
  
  # estimate best zoom level
  bbox_zoom <- st_bbox(bbox_4326)
  names(bbox_zoom) <- c('left','bottom','right','top')
  best_zoom <- ggmap::calc_zoom(bbox_zoom, adjust = as.integer(-1))
  
  # get the map tiles
  if (map_type %in% "main") tile_map <- maptiles::get_tiles(basemap_bbox, provider = map_provider, zoom = best_zoom + 2)
  if (map_type %in% "inset") tile_map <- maptiles::get_tiles(basemap_bbox, provider = map_provider, zoom = best_zoom)
  
  # crop exactly to extent
  tile_crop_map <- terra::crop(tile_map, basemap_bbox)
  
  # make the basemap
  basemap <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = tile_crop_map, alpha = alpha_level) +
    geom_sf(data = st_as_sfc(st_bbox(basemap_bbox)), fill = NA, color = "black", size = 2) +
    coord_sf(expand = FALSE) + theme_void()
  
  basemap_list <- list(basemap, basemap_bbox)
  names(basemap_list) <- c("map","geom")
  
  return(basemap_list)
  
  
}