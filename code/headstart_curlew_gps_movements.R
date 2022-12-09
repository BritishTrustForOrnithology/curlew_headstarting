##############################
#
#    NE103: Headstarted Curlew movement data
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew_headstarting", output_version_date="2022-11", workspace_version_date="2022-11")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate","basemaps", "RStoolbox", "cowplot")
seed_number <- 1



# =======================    Read header source code   =================

# header code source:
# 1. sets working directories
# 2. loads required packages
# 3. prints session info to file
# 4. sets seed for reproducibility

# should run on either PC or Mac if using .Rproj
source(file.path("code/source_setup_code_rproj.R"))

# project directories created:
# parentwd = Git
# projectwd = eurasian_african_bird_migration_atlas
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# topoutputwd = top level output directory
# topworkspacewd= top level workspace directory


# =======================    Control values   =================

current_year <- 2022
today_date <- Sys.Date()
set_cohort_num <- c(1,2,3,4)
separate_sites <- TRUE
set_fix_rate <- 120
animated_vis <- FALSE
static_vis <- FALSE
static_inset_vis <- TRUE
animate_migration <- FALSE
animate_individuals <- FALSE
bird_flag_list <- c(
  "6Y" ,
  "7K",
  # "8K", # dead 30/08/2022
  "7Y",
  "8L",
  "6X",
  "7E",
  "8E",
  "7U",
  "8X",
  "9L",
  "9J"
)
migrant_list <- c(
  "6Y",
  "9L"
)

# mapping controls
file_format <- "mp4"   # various formats available in MoveVis package, if you've got a long animation, gif file size is huge, mp4s are much smaller
map_service <- "mapbox"   # choose which map service, I've used osm and mapbox (satellite imagery)
map_style <- "satellite" # choose map style (terrain vs satellite)
set_fps <- 25
confidential <- TRUE   # strips lat/lon axis labels from map

filter_by_date <- TRUE
last_date <- ymd(Sys.Date())
first_date <- last_date - 30

# =======================    Load data   =================

# Load individual metadata (pulls live Google sheet data) from current year
source(file.path("code", "headstart_CU_database.R"))
dt_meta <- dt_meta %>% as_tibble()

# Filter metadata to only tags, current year
dt_meta_tags <- dt_meta %>% 
  filter(tag_gps_radio_none == "gps") #%>% 
  # filter(year == current_year)

# ----------------  Get movebank tag data -----------

# load movebank log details
source(file.path(codewd, "movebank_log.R"))

# get info out of movebank
mb_study_name <- searchMovebankStudies(x="Curlews - headstarted", login=loginStored)
mb_study_id <- getMovebankID(mb_study_name, login=loginStored)
mb_study_animals <- getMovebankAnimals(study = mb_study_id, login=loginStored)
mb_individual_id <- mb_study_animals[grep(paste(dt_meta_tags$flag_id, collapse = "|"), mb_study_animals$animalName), "local_identifier"]

# automatic Movebank download - doesn't work if tags are redployed
all_tags <- getMovebankData(
  study = mb_study_name,
  login = loginStored,
  sensorID = "GPS",
  animalName = mb_individual_id,
  removeDuplicatedTimestamps = TRUE
) %>% 
  as.data.frame()

# # read in Movebank data as downloaded csv
# all_tags <- read.csv(file.path(datawd, "BTO_NE_Pensthorpe_WWT - Eurasian Curlews - headstarted.csv"), header = TRUE, stringsAsFactors = FALSE) %>% 
#   as_tibble

# replace all '.' from Movebank column names with '_'
names(all_tags) <- names(all_tags) %>% 
  str_replace_all("[.]", "_")

# merge metadata with tag data
all_tags <- all_tags %>% 
  mutate(new_datetime = as.POSIXct(strptime(timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))) %>% 
  mutate(new_datetime_min = format(new_datetime,format='%Y-%m-%d %H:%M')) %>% 
  mutate(flag_id = substr(local_identifier, 4, 5)) %>% 
  left_join(dt_meta_tags, by="flag_id") %>% 
  mutate(plot_label = paste(release_location, "cohort", cohort_num, flag_id, sep="_")) %>% 
  mutate(plot_label = str_replace_all(plot_label, " ", "_"))

# manually filter out erroneous point for 8E using event_id
all_tags <- all_tags %>% 
  filter(event_id != 23414031453)


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
      
      frames <- frames %>% 
        add_gg(gg = expr(list(
          labs(
            caption="\u00A9 British Trust for Ornithology" ,
            title = paste(unique(bird_df$flag_id))
          )
        )))
      
      
      # # preview one of the frames, e.g. the 100th frame
      # frames[[length(frames)]]
      
      if (file_format %in% "gif") {
        animate_frames(frames,
                       out_file = file.path(outputwd, paste0("anim_", b, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                       height = 800,
                       width = 800
        )
        
      }
      
      if (file_format %in% "mp4") {
        
        # animate frames
        animate_frames(frames,
                       out_file = file.path(outputwd, paste0("anim_", b, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
                       overwrite = TRUE,
                       height = 800,
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
      
      # filter movement data to site, cohort
      bird_df <- all_tags %>% 
        filter(flag_id %in% b)
      # filter(cohort_num %in% set_cohort_num) %>% 
      # filter(new_datetime >= strptime(paste(dmy(migration_date), "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC") - 3600*72)
      # filter(new_datetime >= dmy("16-09-2022"))
      
      # filter by date
      if (filter_by_date) {
        bird_df <- bird_df %>% 
          filter(new_datetime >= ymd(first_date) & new_datetime < ymd(last_date))
      }
      
      
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
  
  
  
  # Separate sites  ----------------
  
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
                     width = ifelse(site %in% c("Sandringham", "all release sites"), 1200, 800),
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



# =======================    Plot data - STATIC   =================

# Static visualisation   -----------------

final_frame_list <- list()

if (static_vis) {
  
  for (b in bird_flag_list) {
    
    fix_rate <- set_fix_rate
    
    # filter movement data to site, cohort, post-release date times
    bird_df <- all_tags %>% 
      filter(flag_id %in% b) %>% 
      filter(new_datetime >= strptime(paste(dmy(release_date), "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
    
    num_days_vis <- floor(max(bird_df$new_datetime) - min(bird_df$new_datetime))
    
    message("Creating visualisation for ", b, " using a fix rate of ", fix_rate, " minutes. Includes ", length(unique(bird_df$local_identifier)), " individual(s):\n\n",
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
    path_col <- "orangered"
    tail_trace_col <- "orange"
    
    # set map extent, conditional on if bird has migrated away from Wash
    centroid_wash <- data.frame(long = 0.33104897, lat = 52.921168)
    
    if (b %in% migrant_list) {
      if (b %in% "9L") {
        set_extent <- extent(centroid_wash$long - 10, 
                             centroid_wash$long + 3,
                             centroid_wash$lat - 7, 
                             centroid_wash$lat + 3)
      }
      
      if (b %in% "6Y") {
        set_extent <- extent(centroid_wash$long - 12,
                             centroid_wash$long + 2,
                             centroid_wash$lat - 5, 
                             centroid_wash$lat + 3)
      }
      
    } else {
      set_extent <- extent(centroid_wash$long - 0.5,
                           centroid_wash$long + 0.6,
                           centroid_wash$lat - 0.25, 
                           centroid_wash$lat + 0.25)
    }
    
    # create spatial frames with a OpenStreetMap terrain map
    frames <- frames_spatial(m, path_colours = path_col,
                             map_service = map_service,
                             map_type = map_style,
                             # map_service = "osm",
                             # map_type = "watercolor",
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
      # add_northarrow() %>%
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
          # caption="\u00A9 British Trust for Ornithology" ,
          title = paste(unique(bird_df$flag_id))
        )
      )))
    
    
    # preview one of the frames, e.g. the 100th frame
    frames[[length(frames)]]
    final_frame_list[[b]] <- frames[[length(frames)]]
    
    # create separate pngs to then stitch together with gifski
    move_static_dir <- file.path(outputwd, "static_vis")
    if (!dir.exists((move_static_dir))) dir.create(move_static_dir)
    
    ggsave(
      frames[[length(frames)]],
      filename = paste0("last_frame_", b, ".png"),
      device="png",
      path = move_static_dir,
      height = 8,
      width = 8,
      units = "in",
      dpi=150
    )
    
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
    
    
    # animate_frames(frames, out_file = file.path(outputwd, paste0("Breckland_2022_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
    #                # overwrite = TRUE,
    #                # fps = 10
    # )
    
  }
  
  saveRDS(final_frame_list, file.path(workspacewd, "final_Frame_plots.rds"))
  
}

if (static_inset_vis) {
  
  for (b in bird_flag_list) {
    
    # filter movement data to site, cohort
    bird_df <- all_tags %>% 
      filter(flag_id %in% b) %>% 
      filter(new_datetime >= ymd(first_date) & new_datetime < ymd(last_date))
    
    # skip to next bird if no data from the last month
    if (nrow(bird_df) < 1) next
    
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
    
    
    # Plot RGB rasterbrick with points geom on top
    gg_main_map <- RStoolbox::ggRGB(basemap_raster, r=1, g=2, b=3) +
      geom_sf(data = st_transform(bird_df_sf$geometry, projection(basemap_raster)), fill = "orange", col = "orange", size = 0.5) +
      geom_sf(data = st_as_sfc(st_bbox(basemap_raster)), fill = NA, color = "black", size = 1) +
      coord_sf(crs = projection(basemap_raster)) +
      # theme(axis.text.x=element_blank(), #remove x axis labels
      #       axis.ticks.x=element_blank(), #remove x axis ticks
      #       axis.text.y=element_blank(),  #remove y axis labels
      #       axis.ticks.y=element_blank()  #remove y axis ticks
      # ) +
      theme_void()
    
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
    cowplot::ggdraw() +
      cowplot::draw_plot(gg_main_map) +
      cowplot::draw_plot(gg_inset_map, -0.08, -0.08, scale = 0.7, width = 0.5, height = 0.5)
    
    ggsave(
      filename = paste0(b, "_static_inset_map_", today_date, ".png"),
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
  
  
  
}



plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}

# plots <- dir(move_static_dir)
# plots <- plots[!(grepl(paste0(c(migrant_list, "non-migrants"), collapse="|"), plots))]
# 
# 
# 
# plot_list <- list()
# 
# for (p in 1:length(plots)) {
#  
#    plot_img <- magick::image_read(file.path(move_static_dir, plots[p]))
#   
# # grab_plot <- ggplot() +
# #   ggpubr::background_image(plot_img) + coord_fixed()
# 
# # grab_plot <- ggimage::ggbackground(grab_plot, plog_img)
# 
# # plot_grid_non_migrants[[p]] <- grab_plot
#    plot_list[[p]] <- cowplot::ggdraw() + cowplot::draw_image(plot_img)
# }
# 
# cowplot::plot_grid(plot_list[[1]], plot_list[[2]])
# cowplot::plot_grid(plot_list[[3]], plot_list[[4]])
# cowplot::plot_grid(plot_list[[5]], plot_list[[6]])
# cowplot::plot_grid(plot_list[[7]], plot_list[[8]])
# cowplot::plot_grid(plot_list[[9]], plot_list[[10]])
# 
# 
# cowplot::plot_grid


plot_grid_non_migrants <- final_frame_list[!names(final_frame_list) %in% migrant_list]

ggsave(
  plot_a_list(plot_grid_non_migrants, 3, 4),
  filename = paste0("last_frame_all_birds_non-migrants_", today_date, ".png"),
  device="png",
  path = move_static_dir,
  height = 10,
  width = 16,
  units = "in",
  dpi=150
)
