
# DEPRECATED FOR NOW - ANIMATED MAPS RELYING ON moveVis package

# these will no longer work due to basemap tiles rendering incorrectly
# This is almost certainly caused by the same issue logged at https://github.com/16EAGLE/basemaps/issues/22for the basemaps package, caused by major changes in the terra package


##############################
#
#    NE103: Headstarted Curlew movement data
#
#
##############################

# Code to create visualisations of GPS tracking data


# Animated maps


# Uses 0_hadstart_curlew_gps_maps_data_prep.R to prepare dataset for mapping


# =======================    Animated maps   =================


# =======================    Control values   =================

set_fix_rate <- "4 hours" # fix rate in minutes e.g. 120 = 1 point every 2 hours

# set which vis to output
# animate_multiple <- FALSE
# animate_individuals <- TRUE

# map aesthetic output controls
map_service <- "esri"   # choose which map service, use esri to match static maps
map_style <- "world_imagery" # choose map style, world_imagery matches static maps
set_fps <- 25 # frame rate for animation


# =======================    Plot data - ANIMATED   =================

# convert move2 data to move class
test_move_dt <- to_move(all_tags_filtered %>% filter(flag_id %in% "9J"))

# align move_data to a uniform time scale
m <- align_move(test_move_dt, res = 720, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("blue"),
                         map_service = "esri", map_type = "world_imagery", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
# getwd()
animate_frames(frames, fps = 10, out_file = file.path(outputwd, "moveVis.gif"), overwrite = TRUE)




# List of birds to create maps for  -----------------

bird_flag_list <- all_tags_filtered %>% 
  pull(flag_id) %>% unique
bird_flag_list


if (!animate_individuals) {
  
  
  # 1. Set up dataset
  # transform to epsg 3857 to be able to use units of metres for the basemap
  anim_dt <- all_tags_filtered %>% st_transform(crs = 3857)
  
  # choose a date range to animate
  date_range <- c(min(mt_time(anim_dt)), max(mt_time(anim_dt)))
  date_range <- as.POSIXct(c("2025-04-15", "2025-06-05"))
  
  
  # add rounded timestamps column
  anim_dt <- anim_dt %>% 
    mutate(round_datetime = lubridate::round_date(timestamp, "30 mins") %>% 
             format("%Y-%m-%d %H:%M") %>% as.POSIXct(.))
  
  # # subset data by time windows
  # anim_dt <- mt_filter_per_interval(anim_dt, unit = set_fix_rate)
  
  # set the column to define time as the rounded time column
  mt_time(anim_dt) <- "round_datetime"
  ts <- mt_time(anim_dt) # retrieve timestamps of locations
  
  # extract vector of times for each location
  times <- sort(unique((c(date_range, ts[ts < max(date_range) & ts > min(date_range)]))))
  
  # create interpolated data to the date range specified
  data_interpolated <-
    mt_interpolate(
      anim_dt[!sf::st_is_empty(anim_dt), ],
      times,
      omit = TRUE
    )
  
  # # create labels (if using geom_text to display timestamp labels for the animation)
  # label_df <- data.frame(
  #   round_datetime = date_range,
  #   display_time = lubridate::with_tz(date_range, "UTC"))
  
  # 2. Create basemap for the animation using custom make_basemap function
  
  map_buffer_km <- 10
  basemap_alpha <- 0.9
  
  basemap_main <- make_basemap(sf_data = data_interpolated,
                               buff_dist = map_buffer_km*1000,
                               map_type = "main",
                               map_provider = "Esri.WorldImagery",
                               alpha_level = basemap_alpha)
  
  # set the x and y limits of the plot
  xlims <- c(st_bbox(basemap_main$geom)[1], st_bbox(basemap_main$geom)[3])
  ylims <- c(st_bbox(basemap_main$geom)[2], st_bbox(basemap_main$geom)[4])
  
  animation <- 
    basemap_main$map +
    geom_sf(data = mt_track_lines(data_interpolated), color = "lightyellow") +
    theme_linedraw() +
    geom_sf(
      data = data_interpolated, size = 5,
      aes(color = individual_local_identifier)
    ) +
    scale_color_viridis_d(option = "viridis") +
    guides(color = "none") +
    xlab(NULL) +
    ylab(NULL) +
    
    # geom_text(
    #   data = label_df,
    #   aes(label = paste0(format(display_time, "%Y-%m-%d %H:%M:%S")), x = xlims[1]+(map_buffer_km*1000), y = ylims[1]+((map_buffer_km*1000))),
    #   color = "lightyellow", size = 3, hjust = 0, vjust = 0
    # ) +
    # 
    
    # coord_sf(xlim = xlims, ylim = ylims) +
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 18, hjust = 0, vjust = 1),
          plot.subtitle = element_text(size = 16, hjust = 0, vjust = 1)
    ) +
    
    gganimate::transition_time(round_datetime) +
    
    labs(title = "Headstart curlew movements\n\n",
         subtitle = "{format(frame_time, '%Y-%m-%d %H:%M:%S')}\n\n") +
    
    shadow_wake(0.3, exclude_layer = 6)
  
  
  system.time(
    birds_animate_short <- animate(animation,
                                   # nframes = 10 * 24 * 2 + 1,
                                   nframes = 20,
                                   # fps = 25,
                                   # duration = 30, # length of animation in sec
                                   # detail = 5,
                                   height = 800,
                                   width = 800,
                                   units = "px"
                                   # res = 150
    ))
  
  anim_save("live_tags_short.gif",
            birds_animate_short, 
            path = outputwd)
  
  
  system.time(
    birds_animate_long <- animate(animation,
                                  # nframes = 60 * 24,
                                  # nframes = 100,
                                  fps = 25,
                                  duration = 45, # length of animation in sec
                                  # detail = 5,
                                  height = 800,
                                  width = 800,
                                  units = "px"
                                  # res = 150
    ))
  
  saveRDS(birds_animate_long, file.path(workspacewd, "live_tags_60_day_animation.rds"))
  
  anim_save("live_tags_long.gif", 
            birds_animate_long, 
            path = outputwd)
  
  
  
  ############################
  
  
  ##################################
  
  
}



# Animate migration of migrants only  ----------------

if (animate_migration) {
  
  for (b in migrant_list) {
    
    fix_rate <- set_fix_rate
    
    # filter movement data individual
    bird_df <- all_tags %>% 
      filter(flag_id %in% b)
    
    # set the migration date (first mig only)
    mig_date <- bird_df %>% 
      pull(migration_date) %>% 
      head(1) %>% 
      dmy(.) %>% 
      paste(., "01:00:00") %>% 
      strptime(., format = "%Y-%m-%d %H:%M:%S", tz="UTC")
    
    # filter data to 3 day window around migration
    bird_df <- bird_df %>% 
      filter(new_datetime >= mig_date - 3600*72 & new_datetime < mig_date + 3600*72)
    
    # # filter by date
    # if (filter_by_date) {
    #   bird_df <- bird_df %>% 
    #     filter(new_datetime >= first_date & new_datetime < last_date)
    # }
    
    num_days_vis <- floor(max(bird_df$new_datetime) - min(bird_df$new_datetime))
    
    message("Creating visualisation for ", b, " using a fix rate of ", fix_rate, " minutes. Includes ", length(unique(bird_df$local_identifier)), " individuals:\n\n",
            paste(unique(bird_df$local_identifier), collapse = "\n"),
            "\n\nVisualisation runs from ", min(bird_df$new_datetime), " to ", max(bird_df$new_datetime), " over a time period of ", num_days_vis, " days.........\n\n\n")
    
    # subset to only relevant columns needed to plot movements
    # filter to high satellite counts only (4+)
    # convert to move object
    bird_df_move <- bird_df %>% 
      filter(gps_satellite_count >= 3) %>% 
      dplyr::select(plot_label, new_datetime, location_lat, location_long) %>% 
      df2move(proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
              x = "location_long",
              y = "location_lat",
              time = "new_datetime",
              track_id = "plot_label"
      )
    
    # align move_data to a uniform time scale
    m <- align_move(bird_df_move, res = fix_rate, unit = "mins")
    
    # set map aesthetics
    path_colours <- "orangered"
    tail_trace_col <- "orange"
    
    # set map extent, conditional on if bird has migrated away from Wash
    centroid_wash <- data.frame(long = 0.33104897, lat = 52.921168)
    
    if (b %in% "9L") {
      set_extent <- extent(centroid_wash$long - 10, 
                           centroid_wash$long + 3,
                           centroid_wash$lat - 7, 
                           centroid_wash$lat + 3)
    } else {
      set_extent <- extent(centroid_wash$long - 0.5,
                           centroid_wash$long + 0.6,
                           centroid_wash$lat - 0.25, 
                           centroid_wash$lat + 0.25)
    }
    
    if (b %in% "6Y") {
      set_extent <- extent(centroid_wash$long - 12,
                           centroid_wash$long + 2,
                           centroid_wash$lat - 5, 
                           centroid_wash$lat + 3)
    } else {
      set_extent <- extent(centroid_wash$long - 0.5,
                           centroid_wash$long + 0.6,
                           centroid_wash$lat - 0.25, 
                           centroid_wash$lat + 0.25)
    }
    
    
    # trace_colours <- viridis(length(bird_df %>%
    #                                   distinct(plot_label) %>%
    #                                   pull),
    #                          alpha = 0.2
    # )
    
    # create spatial frames with a OpenStreetMap terrain map
    frames <- frames_spatial(m, path_colours = path_colours,
                             map_service = map_service, 
                             map_type = map_style,
                             map_token = ifelse(map_service == "mapbox", "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", ""),
                             alpha = 0.8,
                             equidistant = FALSE,
                             path_legend = FALSE,
                             tail_size = 1.2,
                             trace_show = TRUE,
                             trace_colour = tail_trace_col,
                             ext = set_extent
    ) %>%
      
      # add some customizations, such as axis labels
      # add_labels(x = "Longitude", y = "Latitude") %>% 
      add_northarrow() %>%
      add_scalebar(distance = ifelse(b %in% migrant_list, 100, 10)) %>%
      add_timestamps(m, type = "label") %>%
      add_progress() %>% 
      add_gg(gg = expr(list(
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank()
        ))))
    
    # bto_logo <- png::readPNG(file.path(datawd, "C1-BTO-master-logo-portrait-(no-strap).png"))
    # bto_rast <- grid::rasterGrob(bto_logo, interpolate=TRUE)
    # 
    # frames <- frames %>% 
    #   add_gg(frames,
    #          gg = expr(list(
    #            annotation_custom(bto_rast,
    #                              xmin=max(m$x)+0.002, xmax= max(m$x) + 0.009,
    #                              ymin= max(m$y)-0.005, ymax= max(m$y) + 0.008)
    #          )))
    
    frames <- frames %>% 
      add_gg(gg = expr(list(
        labs(
          caption="\u00A9 British Trust for Ornithology" ,
          title = paste(unique(bird_df$flag_id))
        )
      )))
    
    
    # preview one of the frames, e.g. the 100th frame
    frames[[200]]
    
    if (file_format %in% "gif") {
      animate_frames(frames,
                     out_file = file.path(outputwd, paste0("mig_", b, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                     height = 800,
                     width = 800
      )
      
    }
    
    if (file_format %in% "mp4") {
      
      # animate frames
      animate_frames(frames,
                     out_file = file.path(outputwd, paste0("mig_", b, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                     overwrite = TRUE,
                     height = 800,
                     width = 800,
                     fps = set_fps
      )
    }
    
  }
}



# Animate multiple birds (either by site or all sites) ----------------

if (animate_multiple) {
  
  # Separate sites ----------------
  
  if (separate_sites) {
    
    site_list <- c("Sandringham", "Ken Hill")
    # site_list <- c("Ken Hill")
    
    for (s in site_list) {
      
      # fix rate controls downsampling of data, in minutes
      # Could set to 15 if you have high res data, but trade off in terms of long rendering time
      site <- s
      fix_rate <- set_fix_rate
      
      # filter movement data to site, cohort, post-release date times
      bird_df <- all_tags %>% 
        filter(!flag_id %in% migrant_list) %>% 
        filter(release_location == site) %>% 
        # filter(cohort_num %in% set_cohort_num) %>% 
        filter(new_datetime >= strptime(paste(dmy(release_date), "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
      # filter(new_datetime >= dmy("08-08-2022"))
      
      num_days_vis <- floor(max(bird_df$new_datetime) - min(bird_df$new_datetime))
      
      message("Creating visualisation for ", site, " using a fix rate of ", fix_rate, " minutes. Includes ", length(unique(bird_df$local_identifier)), " individuals:\n\n",
              paste(unique(bird_df$local_identifier), collapse = "\n"),
              "\n\nVisualisation runs from ", min(bird_df$new_datetime), " to ", max(bird_df$new_datetime), " over a time period of ", num_days_vis, " days.........\n\n\n")
      
      # subset to only relevant columns needed to plot movements
      # filter to high satellite counts only (4+)
      # convert to move object
      bird_df_move <- bird_df %>% 
        filter(gps_satellite_count >= 3) %>% 
        dplyr::select(plot_label, new_datetime, location_lat, location_long) %>% 
        df2move(proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                x = "location_long",
                y = "location_lat",
                time = "new_datetime",
                track_id = "plot_label"
        )
      
      # align move_data to a uniform time scale
      m <- align_move(bird_df_move, res = fix_rate, unit = "mins")
      
      # set path colours
      path_colours <- viridis(length(bird_df %>%
                                       distinct(plot_label) %>%
                                       pull)
      )
      # trace_colours <- viridis(length(bird_df %>%
      #                                   distinct(plot_label) %>%
      #                                   pull),
      #                          alpha = 0.2
      # )
      
      # create spatial frames with a OpenStreetMap terrain map
      frames <- frames_spatial(m, path_colours = path_colours,
                               map_service = map_service, 
                               map_type = map_style,
                               map_token = ifelse(map_service == "mapbox", "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", ""),
                               alpha = 0.5,
                               # equidistant = ifelse(site == "Ken Hill", TRUE, FALSE),
                               equidistant = FALSE,
                               
                               path_legend = TRUE,
                               path_legend_title = paste(site, "birds"),
                               
                               # tail_size = 1.2,
                               tail_length = 20 #,
                               # trace_show = TRUE,
                               # trace_colour = trace_colours
                               
      ) %>%
        
        # add some customizations, such as axis labels
        # add_labels(x = "Longitude", y = "Latitude") %>% 
        add_northarrow() %>%
        add_scalebar() %>%
        add_timestamps(m, type = "label") %>%
        add_progress() %>% 
        add_gg(gg = expr(list(
          theme(axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank()
          ))))
      
      # bto_logo <- png::readPNG(file.path(datawd, "C1-BTO-master-logo-portrait-(no-strap).png"))
      # bto_rast <- grid::rasterGrob(bto_logo, interpolate=TRUE)
      # 
      # frames <- frames %>% 
      #   add_gg(frames,
      #          gg = expr(list(
      #            annotation_custom(bto_rast,
      #                              xmin=max(m$x)+0.002, xmax= max(m$x) + 0.009,
      #                              ymin= max(m$y)-0.005, ymax= max(m$y) + 0.008)
      #          )))
      
      frames <- frames %>% 
        add_gg(gg = expr(list(
          labs(caption="\u00A9 British Trust for Ornithology",
               title = paste("Norfolk headstarted curlew", current_year))
        )))
      
      
      # preview one of the frames, e.g. the 100th frame
      frames[[800]]
      
      if (file_format %in% "gif") {
        animate_frames(frames, out_file = file.path(outputwd, paste0(site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                       height = 800,
                       width = ifelse(site %in% "Sandringham", 1200, 800)
        )
        
      }
      
      if (file_format %in% "mp4") {
        
        # animate frames
        animate_frames(frames,
                       out_file = file.path(outputwd, paste0(site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                       overwrite = TRUE,
                       height = 800,
                       width = ifelse(site %in% "Sandringham", 1200, 800),
                       fps = set_fps
        )
      }
      
      
      # # create separate pngs to then stitch together with gifski
      # move_animation_dir <- file.path(outputwd, site)
      # if (!dir.exists((move_animation_dir))) dir.create(move_animation_dir)
      # 
      # names(frames_Ken_Hill) <- paste0("frame_", 1:length(frames_Ken_Hill))
      # 
      # lapply(names(frames_Ken_Hill), function(f) {
      #   
      #   ggsave(
      #     frames_Ken_Hill[[f]],
      #     filename = paste0(f,".png"),
      #     device="png",
      #     path = move_animation_dir,
      #     height = 8,
      #     width = 8,
      #     units = "in",
      #     dpi=150)
      #   
      # }) %>% invisible
      # 
      # # use the gifski package to render gif of png files
      # png_files <- dir(move_animation_dir, pattern = ".*png$", full.names = TRUE)
      # gifski(png_files, gif_file = file.path(outputwd, paste0(map_i, ".gif")), width = 960, height = 672, delay = 1, loop = TRUE, progress = TRUE)
      
    }
    
  }
  
  # Sites together -----------------
  
  if (!separate_sites) {
    
    # fix rate controls downsampling of data, in minutes
    # Could set to 15 if you have high res data, but trade off in terms of long rendering time
    site <- "all release sites"
    fix_rate <- set_fix_rate
    
    # filter movement data to site, cohort, post-release date times
    bird_df <- all_tags %>% 
      # filter(!flag_id %in% migrant_list) %>% 
      filter(new_datetime >= strptime(paste(dmy(release_date), "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
    # filter(new_datetime >= dmy("08-08-2022"))
    
    # bird_df <- all_tags %>% 
    #   filter(flag_id %in% migrant_list) %>% 
    #   filter(new_datetime <= strptime(paste(dmy(migration_date), "00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC")) %>% 
    #   bind_rows(bird_df) %>% 
    #   arrange(cohort_num, release_location, flag_id)
    
    num_days_vis <- floor(max(bird_df$new_datetime) - min(bird_df$new_datetime))
    
    message("Creating visualisation for ", site, " using a fix rate of ", fix_rate, " minutes. Includes ", length(unique(bird_df$local_identifier)), " individuals:\n\n",
            paste(unique(bird_df$local_identifier), collapse = "\n"),
            "\n\nVisualisation runs from ", min(bird_df$new_datetime), " to ", max(bird_df$new_datetime), " over a time period of ", num_days_vis, " days.........\n\n\n")
    
    # subset to only relevant columns needed to plot movements
    # filter to high satellite counts only (4+)
    # convert to move object
    bird_df_move <- bird_df %>% 
      filter(gps_satellite_count >= 3) %>% 
      dplyr::select(plot_label, new_datetime, location_lat, location_long) %>% 
      df2move(proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
              x = "location_long",
              y = "location_lat",
              time = "new_datetime",
              track_id = "plot_label"
      )
    
    # align move_data to a uniform time scale
    m <- align_move(bird_df_move, res = fix_rate, unit = "mins")
    
    # set path colours
    path_colours <- viridis(length(bird_df %>%
                                     distinct(plot_label) %>%
                                     pull)
    )
    
    # set map extent
    centroid_wash <- data.frame(long = 0.33104897, lat = 52.921168)
    
    set_extent <- extent(centroid_wash$long - 0.38,
                         centroid_wash$long + 0.58,
                         centroid_wash$lat - 0.19, 
                         centroid_wash$lat + 0.12
    )
    
    # create spatial frames with a OpenStreetMap terrain map
    frames <- frames_spatial(m, path_colours = path_colours,
                             map_service = map_service, 
                             map_type = map_style,
                             map_token = ifelse(map_service == "mapbox", "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", ""),
                             alpha = 0.5,
                             # equidistant = ifelse(site == "Ken Hill", TRUE, FALSE),
                             equidistant = FALSE,
                             
                             path_legend = TRUE,
                             path_legend_title = "All birds",
                             tail_length = 20,
                             ext = set_extent
                             
                             
    ) %>%
      
      # add some customizations, such as axis labels
      # add_labels(x = "Longitude", y = "Latitude") %>% 
      add_northarrow() %>%
      add_scalebar(distance = 10) %>%
      add_timestamps(m, type = "label") %>%
      add_progress() %>% 
      add_gg(gg = expr(list(
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank()
        ))))
    
    # bto_logo <- png::readPNG(file.path(datawd, "C1-BTO-master-logo-portrait-(no-strap).png"))
    # bto_rast <- grid::rasterGrob(bto_logo, interpolate=TRUE)
    # 
    # frames <- frames %>% 
    #   add_gg(frames,
    #          gg = expr(list(
    #            annotation_custom(bto_rast,
    #                              xmin=max(m$x)+0.002, xmax= max(m$x) + 0.009,
    #                              ymin= max(m$y)-0.005, ymax= max(m$y) + 0.008)
    #          )))
    
    frames <- frames %>% 
      add_gg(gg = expr(list(
        labs(caption="\u00A9 British Trust for Ornithology",
             title = paste("Norfolk headstarted curlew", current_year))
      )))
    
    
    # preview one of the frames, e.g. the 100th frame
    frames[[600]]
    
    if (file_format %in% "gif") {
      animate_frames(frames, out_file = file.path(outputwd, paste0(site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                     height = 800,
                     width = ifelse(site %in% c("Sandringham", "all release sites"), 1200, 800)
      )
      
    }
    
    if (file_format %in% "mp4") {
      
      # animate frames
      animate_frames(frames,
                     out_file = file.path(outputwd, paste0(site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                     overwrite = TRUE,
                     height = 800,
                     width = ifelse(site %in% c("Sandringham", "all release sites"), 1200, 800),
                     fps = set_fps
      )
    }
    
    
    # # create separate pngs to then stitch together with gifski
    # move_animation_dir <- file.path(outputwd, site)
    # if (!dir.exists((move_animation_dir))) dir.create(move_animation_dir)
    # 
    # names(frames_Ken_Hill) <- paste0("frame_", 1:length(frames_Ken_Hill))
    # 
    # lapply(names(frames_Ken_Hill), function(f) {
    #   
    #   ggsave(
    #     frames_Ken_Hill[[f]],
    #     filename = paste0(f,".png"),
    #     device="png",
    #     path = move_animation_dir,
    #     height = 8,
    #     width = 8,
    #     units = "in",
    #     dpi=150)
    #   
    # }) %>% invisible
    # 
    # # use the gifski package to render gif of png files
    # png_files <- dir(move_animation_dir, pattern = ".*png$", full.names = TRUE)
    # gifski(png_files, gif_file = file.path(outputwd, paste0(map_i, ".gif")), width = 960, height = 672, delay = 1, loop = TRUE, progress = TRUE)
    
  }
  
}

