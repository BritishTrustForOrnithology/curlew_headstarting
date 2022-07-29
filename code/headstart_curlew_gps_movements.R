##############################
#
#    NE103: Headstarted Curlew movement data
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew_headstarting", output_version_date="2022_headstarting", workspace_version_date="2022_headstarting")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate")
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
cohort_num <- 1

# mapping controls
file_format <- "mp4"   # various formats available in MoveVis package, if you've got a long animation, gif file size is huge, mp4s are much smaller
map_service <- "mapbox"   # choose which map service, I've used osm and mapbox (satellite imagery)
map_style <- "satellite" # choose map style (terrain vs satellite)
confidential <- TRUE   # strips lat/lon axis labels from map

# =======================    Load data   =================

# Load individual metadata (pulls live Google sheet data) from current year
source(file.path("code", "headstart_CU_database.R"))
dt_meta <- dt_meta %>% as_tibble()

# Filter metadata to only tags, current year
dt_meta_tags <- dt_meta %>% 
  filter(tag_gps_radio_none == "gps") %>% 
  filter(year == current_year)

# ----------------  Get movebank tag data

# load movebank log details
source(file.path(codewd, "movebank_log.R"))

# get info out of movebank
mb_study_name <- searchMovebankStudies(x="Curlews - headstarted", login=loginStored)
mb_study_id <- getMovebankID(mb_study_name, login=loginStored)
mb_study_animals <- getMovebankAnimals(study = mb_study_id, login=loginStored)
mb_individual_id <- mb_study_animals[grep(paste(dt_meta_tags$flag_id, collapse = "|"), mb_study_animals$animalName), "local_identifier"]

# automatic Movebank download - doesn't work if tags are redployed
all_tags <- getMovebankLocationData(
  study = mb_study_name,
  login = loginStored,
  sensorID = "GPS",
  animalName = mb_individual_id,
  removeDuplicatedTimestamps = TRUE
) %>% as_tibble

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
  mutate(flag_id = substr(individual_local_identifier, 4, 5)) %>% 
  left_join(dt_meta_tags, by="flag_id") %>% 
  mutate(plot_label = paste("bird", flag_id, release_location, sep="_")) %>% 
  mutate(plot_label = str_replace_all(plot_label, " ", "_"))

# # add time since midnight
# clocks <-  function(t){hour(t)*3600+minute(t)*60+second(t)}
# all_tags$sec_since_midnight <- clocks(all_tags$new_datetime)
# 
# # add rough day / night variable (6am UTC to 6pm UTC = day)
# all_tags <- all_tags %>% 
#   mutate(day_night = ifelse(sec_since_midnight < 21600 | sec_since_midnight > 64800, "night", "day"))


# Choose dates -----------------
# 
# # First cohort release date
# if (cohort_num == 1) {
#   first_date <- all_tags %>% 
#     filter(cohort_num == 1) %>% 
#     distinct(release_date) %>% 
#     dmy
# }
#   
#   # first_date <- min(all_tags$new_datetime) %>% as.Date()
#   last_date <- dmy(today_date)
#   
#   all_tags_filtered <- all_tags %>% 
#     filter(new_datetime >= first_date_1 & new_datetime <= last_date)
#   

# =======================    Plot data   =================

# Ken Hill -----------------

# fix rate controls downsampling of data, in minutes
# Could set to 15 if you have high res data, but trade off in terms of long rendering time
site <- "Ken Hill"
fix_rate <- 30

# filter movement data to site, cohort, post-release date times
bird_df <- all_tags %>% 
  filter(release_location == site) %>% 
  filter(cohort_num == cohort_num) %>% 
  filter(new_datetime >= strptime(paste(dmy(release_date), "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))

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
frames_Ken_Hill <- frames_spatial(m, path_colours = path_colours,
                                  map_service = map_service, 
                                  map_type = map_style,
                                  map_token = ifelse(map_service == "mapbox", "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", ""),
                                  alpha = 0.5,
                                  equidistant = ifelse(site == "Ken Hill", TRUE, FALSE),
                                  
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

bto_logo <- png::readPNG(file.path(datawd, "C1-BTO-master-logo-portrait-(no-strap).png"))
bto_rast <- grid::rasterGrob(bto_logo, interpolate=TRUE)

frames_Ken_Hill <- add_gg(frames_Ken_Hill,
                             gg = expr(list(
                               annotation_custom(bto_rast,
                                                 xmin=max(m$x)+0.002, xmax= max(m$x) + 0.009,
                                                 ymin= max(m$y)-0.005, ymax= max(m$y) + 0.008)
                             ))) %>% 
  add_gg(gg = expr(list(
    labs(caption="\u00A9 British Trust for Ornithology",
         title = paste("Norfolk headstarted curlew", current_year))
  )))


# preview one of the frames, e.g. the 100th frame
frames_Ken_Hill[[100]]

# animate frames
animate_frames(frames_Ken_Hill,
               out_file = file.path(outputwd, paste0(site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
               overwrite = TRUE,
               fps = 10
)

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


# Sandringham -----------------

# fix rate controls downsampling of data, in minutes
# Could set to 15 if you have high res data, but trade off in terms of long rendering time
site <- "Sandringham"
fix_rate <- 30     

# filter movement data to site, cohort, post-release date times
bird_df <- all_tags %>% 
  filter(release_location == site) %>% 
  filter(cohort_num == cohort_num) %>% 
  filter(new_datetime >= strptime(paste(dmy(release_date), "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))

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
#                          alpha = 0.5
# )

# create spatial frames with a OpenStreetMap terrain map
frames_Sandringham <- frames_spatial(m, path_colours = path_colours,
                                     map_service = map_service, 
                                     map_type = map_style,
                                     map_token = ifelse(map_service == "mapbox", "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", ""),
                                     alpha = 0.5,
                                     equidistant = ifelse(site == "Ken Hill", TRUE, FALSE),
                                     
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


bto_logo <- png::readPNG(file.path(datawd, "C1-BTO-master-logo-portrait-(no-strap).png"))
bto_rast <- grid::rasterGrob(bto_logo, interpolate=TRUE)

frames_Sandringham <- add_gg(frames_Sandringham,
                 gg = expr(list(
                   annotation_custom(bto_rast,
                                     xmin=max(m$x) - 0.03, xmax= max(m$x) + 0.005,
                                     ymin= max(m$y) - 0.03, ymax= max(m$y) + 0.01)
                 ))) %>% 
  add_gg(gg = expr(list(
    labs(caption="\u00A9 British Trust for Ornithology",
         title = paste("Norfolk headstarted curlew", current_year))
  )))

# preview one of the frames
frames_Sandringham[[300]]

# animate frames
animate_frames(frames_Sandringham,
               out_file = file.path(outputwd, paste0(site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
               overwrite = TRUE,
               fps = 10
)

# # create separate pngs to then stitch together with gifski
# move_animation_dir <- file.path(outputwd, site)
# if (!dir.exists((move_animation_dir))) dir.create(move_animation_dir)
# 
# names(frames_Sandringham) <- paste0("frame_", 1:length(frames_Sandringham))
# 
# lapply(names(frames_Sandringham), function(f) {
#   
#   ggsave(
#     frames_Sandringham[[f]],
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
# if (site %in% "Ken Hill") {
#   
#   if (map_style %in% "satellite") {
#     frames <- m %>% 
#       frames_spatial(path_colours = c("orangered"),
#                      map_service = "mapbox",
#                      map_type = map_style,
#                      map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg",
#                      tail_colour = "orange",
#                      tail_size = 1.2,
#                      trace_show = TRUE,
#                      trace_colour = "orange",
#                      equidistant = FALSE
#       )  %>% 
#       add_gg(gg = expr(list(
#         theme(
#           legend.position = "none")
#       ))) %>%  
#       # add_labels(x = "Longitude", y = "Latitude") %>%
#       add_northarrow() %>%
#       add_scalebar() %>%
#       add_timestamps(m, type = "label") %>%
#       add_progress()
#   }
#   
#   if (map_style %in% "terrain") {
#     
#     if (confidential) {
#       
#       frames <- m %>% 
#         frames_spatial(path_colours = c("orangered"),
#                        map_service = "osm",
#                        map_type = map_style,
#                        tail_colour = "orange",
#                        tail_size = 1.2,
#                        trace_show = TRUE,
#                        trace_colour = "orange",
#                        equidistant = FALSE
#         ) %>% 
#         add_gg(gg = expr(list(
#           theme(axis.text = element_blank(),
#                 axis.title = element_blank(),
#                 axis.ticks = element_blank(),
#                 legend.position = "none")
#         ))) %>% 
#         # add_labels(x = "Longitude", y = "Latitude") %>%
#         add_northarrow() %>%
#         add_scalebar() %>%
#         add_timestamps(m, type = "label") %>%
#         add_progress()
#       
#     } else {
#       
#       frames <- m %>% 
#         frames_spatial(path_colours = c("orangered"),
#                        map_service = "osm",
#                        map_type = map_style,
#                        tail_colour = "orange",
#                        tail_size = 1.2,
#                        trace_show = TRUE,
#                        trace_colour = "orange",
#                        equidistant = FALSE
#         ) %>%
#         add_gg(gg = expr(list(
#           theme(
#             legend.position = "none")
#         ))) %>% 
#         add_labels(x = "Longitude", y = "Latitude") %>%
#         add_northarrow() %>%
#         add_scalebar() %>%
#         add_timestamps(m, type = "label") %>%
#         add_progress()
#     }
#     
#   }
#   
# }
# 
# if (site %in% "Sandringham") {
#   
#   if (confidential) {
#     
#     frames <- m %>% 
#       frames_spatial(path_colours = c("orangered"),
#                      map_service = "osm",
#                      map_type = map_style,
#                      tail_colour = "orange",
#                      tail_size = 1.2,
#                      trace_show = TRUE,
#                      trace_colour = "orange",
#                      equidistant = FALSE
#       ) %>% 
#       add_gg(gg = expr(list(
#         theme(axis.text = element_blank(),
#               axis.title = element_blank(),
#               axis.ticks = element_blank(),
#               legend.position = "none")
#       ))) %>% 
#       # add_labels(x = "Longitude", y = "Latitude") %>%
#       add_northarrow() %>%
#       add_scalebar() %>%
#       add_timestamps(m, type = "label") %>%
#       add_progress()
#     frames[[100]]
#     
#   } else {
#     
#     frames <- m %>%
#       frames_spatial(path_colours = c("orangered"),
#                      map_service = "mapbox",
#                      map_type = map_style,
#                      map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg",
#                      tail_colour = "orange",
#                      tail_size = 1.2,
#                      trace_show = TRUE,
#                      trace_colour = "orange",
#                      equidistant = FALSE
#       ) %>%
#       add_gg(gg = expr(list(
#         theme(legend.position = "none")
#       ))) %>% 
#       add_labels(x = "Longitude", y = "Latitude") %>%
#       add_northarrow() %>%
#       add_scalebar() %>%
#       add_timestamps(m, type = "label") %>%
#       add_progress()
#     
#   }
# }
# 
# 
# frames[[100]]
# 
# animate_frames(frames, 
#                out_file = file.path(outputwd, paste0(tag, "_", site, "_", map_style, "_with-longer-tail_", today_date, ".", file_format)), overwrite = TRUE
# )



