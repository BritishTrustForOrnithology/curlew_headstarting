
# DEPRECATED FOR NOW - ANIMATED MAPS RELYING ON moveVis package

# these will no longer work due to basemap tiles rendering incorrectly
# This is almost certainly caused by the same issue logged at https://github.com/16EAGLE/basemaps/issues/22for the basemaps package, caused by major changes in the terra package


# =======================    Plot data - ANIMATED   =================

if (animated_vis) {
  
  
  # Animate birds individually  ----------------
  
  if (animate_individuals) {
    
    for (b in bird_flag_list) {
      
      fix_rate <- set_fix_rate
      
      # filter movement data to site, cohort
      bird_df <- all_tags %>% 
        filter(flag_id %in% b) %>% 
        filter(new_datetime >= ymd(first_date) & new_datetime < ymd(last_date))
      
      # skip to next bird if no data from the last month
      if (nrow(bird_df) < 1) next
      
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
      
      # if (b %in% "9L") {
      #   set_extent <- extent(centroid_wash$long - 10, 
      #                        centroid_wash$long + 3,
      #                        centroid_wash$lat - 7, 
      #                        centroid_wash$lat + 3)
      # } else {
      #   set_extent <- extent(centroid_wash$long - 0.5,
      #                        centroid_wash$long + 0.6,
      #                        centroid_wash$lat - 0.25, 
      #                        centroid_wash$lat + 0.25)
      # }
      # 
      # if (b %in% "6Y") {
      #   set_extent <- extent(centroid_wash$long - 12,
      #                        centroid_wash$long + 2,
      #                        centroid_wash$lat - 5, 
      #                        centroid_wash$lat + 3)
      # } else {
      #   set_extent <- extent(centroid_wash$long - 0.5,
      #                        centroid_wash$long + 0.6,
      #                        centroid_wash$lat - 0.25, 
      #                        centroid_wash$lat + 0.25)
      # }
      
      current_extent <- extent(m)
      
      set_extent <- extent(current_extent[1] - 0.05,
                           current_extent[2] + 0.05,
                           current_extent[3] - 0.02, 
                           current_extent[4] + 0.02)
      
      
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
                               tail_size = 0.8,
                               trace_show = TRUE,
                               ext = set_extent,
                               trace_colour = tail_trace_col
                               
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
      
      if (!no_labels) {
        frames <- frames %>% 
          add_gg(gg = expr(list(
            labs(
              caption="\u00A9 British Trust for Ornithology" ,
              title = paste(unique(bird_df$flag_id))
            )
          )))
      }
      
      
      # # preview one of the frames, e.g. the 100th frame
      # frames[[length(frames)]]
      
      if (file_format %in% "gif") {
        animate_frames(frames,
                       out_file = file.path(outputwd, paste0("anim_", b, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                       # height = 800,
                       width = 800
        )
        
      }
      
      if (file_format %in% "mp4") {
        
        # animate frames
        animate_frames(frames,
                       out_file = file.path(outputwd, paste0("anim_", b, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                       overwrite = TRUE,
                       # height = 800,
                       width = 800,
                       fps = set_fps
        )
      }
      
      rm(frames)
      gc()
      
    }
    
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
  
}
