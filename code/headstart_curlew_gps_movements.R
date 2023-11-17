##############################
#
#    NE103: Headstarted Curlew movement data
#
#
##############################

# 17/11/2023: Major code updates to remove use of moveVis and basemaps due to issues with terra dependency and map tiles not plotting properly


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew_headstarting", output_version_date="2023-11", workspace_version_date="2023-07")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate","basemaps", "RStoolbox", "cowplot","sfheaders", "maptiles", "tidyterra")
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

current_year <- 2023
# current_year <- 2022
today_date <- Sys.Date()
set_cohort_num <- c(1,2,3,4)
set_cohort_num <- c(1,2)

# set_fix_rate <- 120

# set which vis to output
# animated_vis <- FALSE
static_vis_final_frame <- FALSE
static_inset_vis <- TRUE
# animate_multiple <- FALSE
# animate_migration <- FALSE
# animate_individuals <- TRUE
# separate_sites <- FALSE

# map aesthetic output controls
# file_format <- "mp4"   # various formats available in MoveVis package, if you've got a long animation, gif file size is huge, mp4s are much smaller
# map_service <- "mapbox"   # choose which map service, I've used osm and mapbox (satellite imagery)
# map_style <- "satellite" # choose map style (terrain vs satellite)
# set_fps <- 25
# confidential <- TRUE   # strips lat/lon axis labels from map
# no_labels <- TRUE

filter_by_date <- FALSE
# last_date <- ymd(Sys.Date())
# last_date <- ymd("2023-11-01")
# first_date <- last_date - 60


# ====  Load functions  =================================

## Load any functions here

source(paste(codewd, "functions", "run_all_functions.R", sep="/"))


# =======================    Individuals to map   =================

bird_flag_list_2022 <- c(
  # "6Y" ,
  "7K",
  # # "8K", # dead 30/08/2022
  # # "7Y",
  "8L",
  "6X",
  # # "7E",
  # # "8E",
  # # "7U",
  "8X",
  "9L",
  "9J"
)

migrant_list_2022 <- c(
  "6Y",
  "9L"
)

bird_flag_list_2023 <- 
  c("LJ",
    "LU",
    "LV",
    "XA",
    "XE",
    "XH",
    "XJ",
    "XL",
    "XP",
    "XX",
    "YH",
    "YJ",
    "YK")


# =======================    Load data   =================

# Load individual metadata (pulls live Google sheet data) from current year
source(file.path("code", "headstart_CU_database.R"))

dt_meta <- dt_meta %>% as_tibble()

# Filter metadata to only tags, current year
dt_meta_tags <- dt_meta %>% 
  filter(tag_gps_radio_none == "gps") %>% 
  filter(dead %in% "")
# filter(year == current_year)

# ----------------  Get movebank tag data -----------

# load movebank log details
source(file.path(codewd, "movebank_log.R"))

# get info out of movebank
mb_study_name <- searchMovebankStudies(x="Curlews - headstarted", login=loginStored)
mb_study_id <- getMovebankID(mb_study_name, login=loginStored)
mb_study_animals <- getMovebankAnimals(study = mb_study_id, login=loginStored)
mb_individual_id <- mb_study_animals[grep(paste(dt_meta_tags$flag_id, collapse = "|"), mb_study_animals$animalName), "local_identifier"]

# summarise data for 'live' tags
mb_study_animals %>% 
  filter(local_identifier %in% mb_individual_id) %>% 
  dplyr::select(local_identifier, timestamp_start, timestamp_end, number_of_events)

# automatic Movebank download - doesn't work if tags are redeployed
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
all_tags_meta <- all_tags %>% 
  mutate(new_datetime = as.POSIXct(strptime(timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))) %>% 
  mutate(new_datetime_min = format(new_datetime,format='%Y-%m-%d %H:%M')) %>% 
  mutate(flag_id = substr(local_identifier, 4, 5)) %>% 
  right_join(dt_meta_tags, by="flag_id") %>% 
  mutate(plot_label = paste(release_location, flag_id, name, sep="_")) %>% 
  mutate(plot_label = str_replace_all(plot_label, " ", "_"))

# manually filter out erroneous point for 8E and 7K using event_id
all_tags_meta <- all_tags_meta %>% 
  filter(event_id != 23414031453) %>% 
  filter(event_id != 26788471078) %>% 
  filter(event_id != 30038596753)





# =======================    Static maps   =================

bird_flag_list <- unique(all_tags_meta$flag_id)

# Static visualisation - inset maps   -----------------

if (static_inset_vis) {
  
  for (b in bird_flag_list) {
    
    # filter movement data to site, cohort
    individual_df <- all_tags_meta %>% 
      filter(flag_id %in% b)
    
    # skip to next bird if no data from the last month
    if (nrow(individual_df) < 1) next
    
    draw_movement_map(individual_df, map_type = "path", inset_map = TRUE, filter_date = filter_by_date)
    
    
  }
  
  
}
