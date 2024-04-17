##############################
#
#    NE103: Headstarted Curlew movement data
#
#
##############################

# Code to create visualisations of GPS tracking data


# 17/11/2023: Major code updates to remove use of moveVis and basemaps due to issues with terra dependency and raster map tiles not plotting properly
# 21/11/2023: Major code revisions to use move2 package instead of move to interface with Movebank

# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew_headstarting", output_version_date="2024-04", workspace_version_date="2024-04")
package_details <- c("sf","tidyverse","move2","ggmap","RColorBrewer","viridisLite","rcartocolor","lubridate","suncalc","cowplot","sfheaders", "maptiles", "tidyterra","rnaturalearth","rnaturalearthdata")
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

select_year <- 2022
today_date <- Sys.Date()

# filtering criteria birds
filter_birds <- FALSE # filter birds for mapping to only show active tags
filter_year <- TRUE # filter birds for mapping to only show particular year cohorts

# mapping criteria
map_all_birds_together <- TRUE # individual maps or all together
wash_obs_only <- FALSE # show only Wash-area GPS data on map

# filtering criteria dates
filter_by_date <- FALSE # filter GPS data by date
filter_last_60_days <- FALSE # filter data to last 60 days
set_first_date <- "2022-09-01" # in format "yyyy-mm-dd"
set_last_date <- "2024-04-17"

# filtering criteria other
filter_height_speed <- FALSE # filter flight heights & speeds

# data management criteria
update_gdrive_data <- FALSE # download fresh data from google drive

# ====  Load functions  =================================

## Load any functions here

source(paste(codewd, "functions", "run_all_functions.R", sep="/"))


# =======================    Load data   =================

# Load individual metadata from current year
# Toggle logic value above if fresh download of google drive data is needed
if (update_gdrive_data) source(file.path("code", "source", "download_gdrive_data.R"))
source(file.path("code", "source", "load_gdrive_data.R"))

dt_meta <- dt_meta %>% as_tibble()


# =======================    Individuals to map   =================

# Filter metadata to still active / recently active tags (i.e. not dead / unknown fate birds)
if (filter_birds) {
  dt_meta_tags <- dt_meta %>% 
    filter(tag_gps_radio_none == "gps") %>% 
    filter(state %in% "")
} else {
  dt_meta_tags <- dt_meta %>% 
    filter(tag_gps_radio_none == "gps")
}

# Filter metadata to only include tags of a certain year release
if (filter_year) {
  dt_meta_tags <- dt_meta_tags %>% 
    filter(year == select_year)
}

# ---- Get movebank tag data -----------

# load movebank log details
source(file.path(codewd, "movebank_log.R"))

# get info out of movebank - 'live' or recently live tags only
# study ID for headstarting project = 1678739528
# mb_study_name <- movebank_download_study_info (x="Curlews - headstarted", login=loginStored)
mb_study_id <- movebank_get_study_id("BTO-NE-Pensthorpe - Eurasian Curlews - headstarted")
mb_study_animals <- movebank_download_deployment(mb_study_id)

# download all active tagged birds
mb_individual_id <- mb_study_animals[grep(paste(dt_meta_tags$flag_id, collapse = "|"), mb_study_animals$individual_local_identifier), "individual_local_identifier"] %>% pull %>% as.character

# automatic Movebank download - live or recently live tags only
# (doesn't work if tags are redeployed - not sure if this applies in move2 packages)
all_tags <- movebank_download_study(
  study_id = mb_study_id,
  sensor_type_id  = "gps",
  individual_local_identifier = mb_individual_id #,
  # as.POSIXct("2008-08-01 00:00:00"),
  # timestamp_end = as.POSIXct("2008-08-03 00:00:00"),
  # removeDuplicatedTimestamps = TRUE
)

# # save as rds file
# if (filter_birds) saveRDS(all_tags, file = file.path(workspacewd, "movebank_live_tags_download.rds"))
# if (!filter_birds) saveRDS(all_tags, file = file.path(workspacewd, "movebank_all_tags_download.rds"))

# # read in Movebank data as last downloaded RDS file (can be used if Movebank is down)
# # load rds file
# if (filter_birds) all_tags <- readRDS(file.path(workspacewd, "movebank_live_tags_download.rds"))
# if (!filter_birds) all_tags <- readRDS(file.path(workspacewd, "movebank_all_tags_download.rds"))

# summarise tag data per tag (first & last dates, total number of fixes)
all_tags_summary <- all_tags %>% 
  st_drop_geometry %>% 
  group_by(individual_local_identifier) %>% 
  summarise(min_date = min(timestamp), max_date = max(timestamp)) %>% 
  left_join(all_tags %>% st_drop_geometry %>% group_by(individual_local_identifier) %>% tally, by = "individual_local_identifier")

all_tags_summary


# ---- Merge with individual metadata -----------

# also add lon/lat values not as a geometry for use later
all_tags_meta <- all_tags %>% 
  mutate(flag_id = substr(individual_local_identifier, 4, 5)) %>%
  right_join(.,dt_meta_tags, by="flag_id") %>%
  mutate(plot_label = paste(release_location, flag_id, name, sep="_")) %>%
  mutate(plot_label = str_replace_all(plot_label, " ", "_")) # %>% 
# mutate(lon = st_coordinates(.)[,1],
#        lat = st_coordinates(.)[,2])


# # ---- Explore data to add filtering criteria for dodgy observations -----------
# 
# # filter out to only ground speeds > 1 m/s to look for turning points in speed data
# # nice turning point between stationary and active flight data at ca. 4.2 m/s
# # use this value to filter subsequent data to active flight only
# ggplot(all_tags_meta %>% filter(as.numeric(ground_speed) > 1), aes(as.numeric(ground_speed))) +
#   geom_density() +
#   scale_x_continuous(n.breaks = 20)
# 
# active_flight_threshold <- 4.2
# 
# ggplot(all_tags_meta %>% filter(as.numeric(height_above_msl) > -50 & as.numeric(height_above_msl) < 0), aes(as.numeric(height_above_msl))) +
#   geom_density() +
#   scale_x_continuous(n.breaks = 20)
# 
# ggplot(all_tags_meta %>% filter(as.numeric(height_above_msl) > 250), aes(as.numeric(height_above_msl))) +
#   geom_density() +
#   scale_x_continuous(n.breaks = 20)
# 
# ggplot(all_tags_meta %>% filter(as.numeric(height_above_msl) < 0), aes(as.numeric(height_above_msl))) +
#   geom_histogram()
# 
# ggplot(all_tags_meta %>% filter(as.numeric(height_above_msl) > -50 & as.numeric(height_above_msl) < 0), aes(as.numeric(height_above_msl))) +
#   geom_histogram()
# 
# # quite a few records have negative altitudes
# # still quite a few which are probably valid location data have heights less than -20
# all_tags_meta %>% filter(as.numeric(height_above_msl) <= -20) %>% as.data.frame %>% nrow


# ----  Clean tag data - generic  -----------------

all_tags_filtered <- all_tags_meta %>% 
  # manually filter out erroneous points using event_id (picked out from kml files)
  filter(event_id != 23414031453) %>% # 8E
  filter(event_id != 26788471078) %>% # 7K
  filter(event_id != 30038596753) %>% # XP
  filter(as.numeric(gps_satellite_count) >= 4)  %>% # filter out low sat counts
  filter(!st_is_empty(.)) # omit empty locations

# summarise tag data per tag (first & last dates, total number of fixes)
all_tags_summary <- all_tags_filtered %>% 
  st_drop_geometry %>% 
  group_by(individual_local_identifier) %>% 
  summarise(min_date = min(timestamp), max_date = max(timestamp)) %>% 
  left_join(all_tags_filtered %>% st_drop_geometry %>% group_by(individual_local_identifier) %>% tally, by = "individual_local_identifier")

all_tags_summary


# ----  Filter by date if required  -----------------

# Filter by date if required
# Otherwise, first and last dates are just the min and max timestamps per tag
if (filter_by_date) {
  
  # Filter to last 60 days of data (per tag) if required
  # Otherwise, use other custom date range
  if (filter_last_60_days) {
    all_tags_date_range <- all_tags_summary %>% 
      rename(last_date = max_date) %>% 
      mutate(first_date = last_date - 60*86400) %>% 
      dplyr::select(individual_local_identifier, first_date, last_date)
  } else {
    all_tags_date_range <- all_tags_summary %>% 
      rename(last_date = max_date) %>% 
      mutate(first_date = as.POSIXct(set_first_date, tz = "UTC")) %>% 
      mutate(last_date = as.POSIXct(set_last_date, tz = "UTC")) %>% 
      dplyr::select(individual_local_identifier, first_date, last_date)
  }

} else {
  
  all_tags_date_range <- all_tags_summary %>% 
    rename(last_date = max_date) %>% 
    rename(first_date = min_date) %>% 
    dplyr::select(individual_local_identifier, first_date, last_date)
  
}

all_tags_filtered <- all_tags_filtered %>% 
  left_join(all_tags_date_range, by = "individual_local_identifier") %>% 
  mutate(in_date = ifelse(timestamp >= first_date & timestamp <= last_date, TRUE, FALSE)) %>%     filter(in_date == TRUE)


# summarise tag data per tag (first & last dates, total number of fixes)
all_tags_summary <- all_tags_filtered %>% 
  st_drop_geometry %>% 
  group_by(individual_local_identifier) %>% 
  summarise(min_date = min(timestamp), max_date = max(timestamp)) %>% 
  left_join(all_tags_filtered %>% st_drop_geometry %>% group_by(individual_local_identifier) %>% tally, by = "individual_local_identifier")

all_tags_summary



# ----  Filter tag data - altitude/ground speed  -----------------

if (filter_height_speed) {
  
  # for relevant analyses, filter out unlikely flight height and ground speed values
  # for analyses only using gps locational data, these records are probably fine
  all_tags_filtered <- all_tags_filtered %>% 
    filter(as.numeric(height_above_msl) >= -20 & as.numeric(height_above_msl) <= 2000) %>% # filter out altitudes that are unlikely
    filter(as.numeric(ground_speed) >= 0) %>% # filter out negative ground_speeds
    mutate(in_flight = ifelse(ground_speed >= active_flight_threshold, TRUE, FALSE)) # add flying / stationary column flag
  
}

# ----  Add sunrise / sunset times ---------------------

# add sunrise / sunset times using suncalc package
# https://github.com/datastorm-open/suncalc

all_tags_filtered <- all_tags_filtered %>% 
  mutate(suncalc::getSunlightTimes(
    data = all_tags_filtered %>% 
      mutate(date = as.Date(timestamp),
             lon = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2]
      )
  )) %>% 
  mutate(day_night = ifelse(timestamp >= sunrise & timestamp <= sunset, "day", "night")) %>% 
  arrange(individual_local_identifier, timestamp)


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
  gis_wash_dir <- file.path("../../GIS/curlew/headstarting") # Sam's computer GIS filepath
  # gis_wash_dir <- file.path("../GIS/wwrg_wash_study_area_polygon") # Katharine's computer GIS path
  
  # Load WWRG Wash study shapefile -----------------
  wash_area <- st_read(file.path(gis_wash_dir, "wash_north_norfolk_study_area_polygon.shp"))
  # wash_area <- st_read(file.path(gis_wash_dir, "wwrg_wash_study_area_polygon.shp"))
  
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


