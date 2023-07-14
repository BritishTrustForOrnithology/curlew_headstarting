##############################
#
#    NE103: Headstarted Curlew Database
#    19/10/2021
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2022-11", workspace_version_date="2022-11")
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



# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")


# ----- Download latest file from Google Drive  -----

# individual metadata
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/10mZu0PAjuIXtCZMSYFUFxa2j-2QxlDDlTQDfNTArf-8/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_individual_metadata.csv")
)

# biometric data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1FqUZHwe2aLJlw-SiwJtdORtuISnk7tIHlUIT_ZR7pJg/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_biometric_data.csv")
)

# gps tag deployment data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1V3QMfRHw1Y_yxKeV83CgV-dojuqG0CfIiIy2-D4lK4k/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_tag_deployment_data_gps.csv")
)

# radio tag deployment data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1_3v4_vnuXvshp3kjDC-U_hzY9-uMuGH4qpJnmmugbNQ/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_tag_deployment_data_radio.csv")
)

# resighting data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1v-U_m87Nza0P3fcQiEkwOaELwlbwTdaiepFyXUJQhpU/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_resighting_data.csv")
)

# egg collection data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/18JzojuDbjbuqsboZ5clSNgtX5iOXu1ikR_J8FQptzUA/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_egg_collection_data.csv")
)

# rearing data
googledrive::drive_download(
  file = "https://docs.google.com/spreadsheets/d/1cpXhZwueeV5TqP4wANnWIxW6U8KSdmCvClKf4E-To2Y/edit?usp=sharing",
  type = "csv",
  overwrite = TRUE,
  path = file.path(datawd, "headstart_curlew_rearing_data.csv")
)

# ----- Load downloaded files  -----

# Load individual metadata
dt_meta <- read.csv(file.path(datawd, "headstart_curlew_individual_metadata.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load biometric data
dt_biometric <- read.csv(file.path(datawd, "headstart_curlew_biometric_data.csv"), header = TRUE, stringsAsFactors = FALSE) %>% dplyr::select(1:7, 9:10, 12, 15:16, 18:22, 36:42)

# Load gps tag data
dt_deploy_gps <- read.csv(file.path(datawd, "headstart_curlew_tag_deployment_data_gps.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load radio tag data
dt_deploy_radio <- read.csv(file.path(datawd, "headstart_curlew_tag_deployment_data_radio.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load resighting data
dt_resighting <- read.csv(file.path(datawd, "headstart_curlew_resighting_data.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load egg collection data
dt_eggs <- read.csv(file.path(datawd, "headstart_curlew_egg_collection_data.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load rearing data
dt_rearing <- read.csv(file.path(datawd, "headstart_curlew_rearing_data.csv"), header = TRUE, stringsAsFactors = FALSE)

# ----- Create output for DemOn data entry  -----

# Rename biometric fields
biometric_field_names <- c(
  "entered_demon",
  "urn",
  "day",
  "month",
  "year",
  "site_code",
  "county_code",
  "ring",
  "type",
  "age",
  "wing_initials",
  "wing",
  "tarsus_toe",
  "bill",
  "total_head",
  "weight",
  "weight_time",
  "days_age",
  "flag_id",
  "LA",
  "LB",
  "RA",
  "RB",
  "comments"
)

names(dt_biometric) <- biometric_field_names

# add date to biometric data
dt_biometric <- dt_biometric %>% 
  mutate(across(.cols = c("day", "month", "year"), .fns = as.character)) %>% 
  mutate(date = ifelse(is.na(year), NA, paste(day, month, year, sep="/"))) %>% 
  mutate(new_date = as.Date(date, tz = "BST", format = c("%d/%m/%Y")))

# # ----- Create output for DemOn data entry  -----
# 
# dt_biometric_ringing <- dt_biometric %>% filter(type %in% "H")
# 
# # Merge metadata with biometric ringing event (type = H, as per DemOn code H)
# # dt_all <- merge(dt_biometric_ringing, dt_meta, by = c("flag_id", "year", "ring"))
# dt_all <- dt_biometric %>% 
#   right_join(dt_meta, by = c("flag_id", "year", "ring"))
# 
# 
# # Output easily readable data for DemOn data entry
# dt_easy_demon <- dt_all %>% 
#   filter(year %in% 2022) %>% 
#   filter(type %in% "H") %>% 
#   filter(release_location != "not released") %>%
#   filter(entered_demon != "Y") %>% 
#   arrange(ring) %>% 
#   mutate(scheme = "GBT",
#          species = "Curlew",
#          sexing_method = "DNA",
#          capture_time = "12:00",
#          condition = "H",
#          capture_method = "H",
#          metal_mark_info = "N",
#          colour_mark_info = "U"
#          ) %>% 
#   dplyr::select(entered_demon, type, ring, scheme, species, age, sex, sexing_method, release_date, capture_time, release_location, condition, capture_method, metal_mark_info, wing_initials, colour_mark_info, LB, RB, LA, RA, tag_gps_radio_none)
# 
# dt_easy_demon <- dt_easy_demon %>% 
#   rename(vist_date = release_date,
#          location = release_location,
#          extra_text = tag_gps_radio_none) %>% 
#   mutate(location = ifelse(location %in% "Ken Hill", "KH-pen", "SH-pen-02")) %>% 
#   mutate(extra_text = ifelse(extra_text %in% "gps", "gps tag deployed", ifelse(extra_text %in% "radio", "radio tag deployed", "")))
# # write.csv(dt_easy_demon, file.path(outputwd, "easy_demon_data_entry_2022.csv"), row.names = FALSE)


# ----- Merge tag deployment data with dt_biometric  -----

# choose latest biometrics date for each individual
dt_biometric %>% 
  filter(type %in% "R") %>% 
  group_by(ring) %>% 
  summarise(max_date = max(new_date), weight, wing, tarsus_toe) %>% 
  right_join(dt_deploy_gps, by = c("ring")) %>% 
  filter(year %in% "2022") %>% 
  dplyr::select(ring, flag_id,tagged_date, release_location, weight.x, wing.x, tarsus_toe) %>% 
  as.data.frame() %>% 
  arrange(flag_id) %>% 
  write.csv(file.path(outputwd, "easy_gps_sm_reporting.csv"), row.names = FALSE)


# ----- playing around  -----


# dt_all <- dt_biometric %>%
#   right_join(dt_meta, by = c("flag_id",  "ring"))
# 
# dt_all %>% 
#   filter(tag_gps_radio_none %in% "gps") %>% 
#   filter(type %in% "R") %>% 
#   filter(year.x %in% "2022") %>% 
#   dplyr::select(flag_id, tagged_date, days_age, weight, wing) %>% 
#   group_by(tagged_date) %>% 
#   summarise(mean_age = mean(days_age), sd_age = sd(days_age), min_age = min(days_age), max_age = max(days_age), mean_weight = mean(weight), mean_wing = mean(wing)) %>% 
#   arrange(tagged_date)


# ----- Merge individual metadata with egg & rearing data (released birds only)  ------

dt_meta_rear <- dt_meta %>% 
  right_join(dt_rearing, by = c("flag_id", "year"))

dt_meta_rear_egg <- dt_meta_rear %>% 
  right_join(dt_eggs, by = c("year", "clutch_num"))

# tally of released birds only by airfield
dt_meta_rear_egg %>% 
  group_by(year, airfield_name) %>% 
  tally()
