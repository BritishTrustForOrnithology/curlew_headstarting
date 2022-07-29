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

project_details <- list(project_name="curlew", output_version_date="2022_headstarting", workspace_version_date="2022_headstarting")
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

# ----- Load downloaded files  -----

# Load individual metadata
dt_meta <- read.csv(file.path(datawd, "headstart_curlew_individual_metadata.csv"), header = TRUE, stringsAsFactors = FALSE)

# Load biometric data
dt_biometric <- read.csv(file.path(datawd, "headstart_curlew_biometric_data.csv"), header = TRUE, stringsAsFactors = FALSE) %>% dplyr::select(1:7, 9:11, 13:15, 17:21, 35:41)

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
  "plumage",
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

dt_biometric_ringing <- dt_biometric %>% filter(type %in% "H")

# Merge metadata with biometric ringing event (type = H, as per DemOn code H)
dt_all <- merge(dt_biometric_ringing, dt_meta, by = c("flag_id", "year", "ring"))

# Output easily readable data for DemOn data entry
dt_easy_demon <- dt_all %>% 
  dplyr::select(entered_demon, type, ring, age, sex, release_date, release_location, LB, RB, LA, RA)
# write.csv(dt_easy_demon %>% filter(release_location != "not released") %>% filter(entered_demon != "Y"), file.path(outputwd, "easy_demon_data_entry.csv"), row.names = FALSE)
