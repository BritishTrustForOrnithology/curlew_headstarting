##############################
#
#    NE103: Headstarted Curlew - All Encounter Histories
#    13/07/2023, 16/11/2023
#
##############################


# Code for outputting csv resighting histories & maps for individual birds


# Done 04/01/2023
# removed duplicate WWRG records that have also been entered in NE103 google form sheet - inspected by eye and removed duplicate records from the same day / time / location / observer



# =======================    Control values   =================

# TRUE = fresh download of google drive data
update_gdrive_data <- FALSE



# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="resighting_histories", workspace_version_date="2023-07")
package_details <- c("tidyverse","patchwork","RColorBrewer","viridisLite","rcartocolor","lubridate", "data.table")
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


# =======================    Load functions   =================

source(paste(codewd, "functions", "run_all_functions.R", sep="/"))


# =======================    Load data   =================

today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
# Toggle logic value above if fresh download of google drive data is needed
# will need to provide authentication for R to access Google Drive
if (update_gdrive_data) source(file.path("code", "source", "download_gdrive_data.R"))
source(file.path("code", "source", "load_gdrive_data.R"))


# =======================    Prepare resighting data   =================

# FIRST (before running any code!)  ---------------------

# Tidy up the Google sheet form responses
# Check the flag / colour combo column to verify it is indeed a Norfolk headstart
# If yes, then enter the flag code in the flag_id column
# WWRG birds and headstarts from other projects sometimes get submitted to the Google Form
# NE103 birds are sometimes submitted to the WWRG Shiny app - these sightings have all been extracted and transferred to the NE103 Google sheet up to 02/01/2024
# Check the 'location' column particularly: convert any 'descriptive' locations or locations which include text to a OSGB grid ref or rough lat lon location
# E.g. if observer enters "Snettisham Pits mudflats opposite Shore Hide" for location, use Gooogle Maps or similar to determine a rough position and replace the observer descriptive location with a spatial one
# Similarly, if observer includes text in the field together with a georeferenced location, remove the text leaving only the spatial location; e.g. Change "52.4976, 0.62823 Heacham Beach" to just "52.4976, 0.62823"
# Make a note in the column QA-QC comments on the Google sheet if you make any manual changes, retaining the information of what the observer originally entered in the form

resighting_field_names <- c(
  "qa_qc_comments",
  "flag_id",
  "timestamp",
  "observer",
  "date",
  "time",
  "location",
  "description",
  "colour_combo",
  "observer_comments",
  "photos",
  "email",
  "privacy_agreement"
)

names(dt_resighting) <- resighting_field_names

# get rid of any rows which don't have a filled flag_id value
dt_resighting_filtered <- dt_resighting %>% 
  filter(flag_id != "")


# =======================    Encounter histories for resighted / GPS tagged birds  =================

dt_encounter_long <- dt_resighting_filtered %>% 
  left_join(dt_meta, by = "flag_id") %>% 
  mutate(date = strptime(date, format = "%d/%m/%Y")) %>% 
  mutate(resight_year = year(date),
         resight_month = month(date),
         seen = 1) %>% 
  arrange(flag_id, date) %>% 
  dplyr::select(flag_id, year, tag_gps_radio_none, state, resight_year, resight_month, seen) %>% 
  distinct %>% 
  mutate(resight_month = ifelse(resight_month %in% c(1:9), paste0("0", resight_month), resight_month))# add leading zeros to months 1:9

dt_encounter_wide <- dt_encounter_long %>% 
  pivot_wider(
    names_from = c("resight_year", "resight_month"), 
    values_from = seen
  ) %>% 
  as.data.table
dt_encounter_wide

# create all year_month combinations and add any missing combination cols to dt_encounter_wide
all_years_months <- expand.grid(resight_year = c(2021, 2022, 2023), resight_month = c(1:12)) %>% 
  arrange(resight_year, resight_month) %>% 
  mutate(resight_month = ifelse(resight_month %in% c(1:9), paste0("0", resight_month), resight_month)) %>% # add leading zeros to months 1:9
  mutate(resight_year_month = paste(resight_year, resight_month, sep="_")) %>% 
  filter(!(resight_year_month %in% c("2021_01", "2021_02", "2021_03", "2021_04", "2021_05", "2021_06")))
all_years_months

all_years_months$resight_year_month[!which(names(dt_encounter_wide)[5:length(dt_encounter_wide)] %in% all_years_months$resight_year_month)]

# arrange encounter history cols in date order
setcolorder(dt_encounter_wide, c(names(dt_encounter_wide)[1:4], (names(dt_encounter_wide)[5:length(dt_encounter_wide)] %>% sort)))
dt_encounter_wide %>% head

# manually adjust GPS birds to known current state
# substitute 1 for last month with transmitted data / bird known to have been alive
dt_encounter_wide %>% 
  filter(tag_gps_radio_none %in% "gps")

dt_encounter_wide[flag_id == "0E", "2023_06"] <- 1
dt_encounter_wide[flag_id == "6X", "2023_12"] <- 1
dt_encounter_wide[flag_id == "6Y", "2023_07"] <- 1
dt_encounter_wide[flag_id == "7K", "2023_12"] <- 1
dt_encounter_wide[flag_id == "7Y", "2022_12"] <- 1
dt_encounter_wide[flag_id == "9L", "2023_12"] <- 1
dt_encounter_wide[flag_id == "LV", "2023_12"] <- 1
dt_encounter_wide[flag_id == "XL", "2023_12"] <- 1
dt_encounter_wide[flag_id == "XP", "2023_12"] <- 1
dt_encounter_wide[flag_id == "XU", "2023_09"] <- 1
dt_encounter_wide[flag_id == "XX", "2023_12"] <- 1
dt_encounter_wide[flag_id == "YJ", "2023_12"] <- 1
dt_encounter_wide[flag_id == "YK", "2023_12"] <- 1
dt_encounter_wide[flag_id == "YN", "2023_08"] <- 1

# pivot back to longer
dt_encounter_long_2 <- dt_encounter_wide %>% 
  pivot_longer(
    cols = !(c(flag_id, year, tag_gps_radio_none, state)), 
    names_to = "release_year_month", 
    values_to = "seen",
    values_drop_na = TRUE
  ) 

dt_encounter_long_2 %>% 
  print(n = nrow(dt_encounter_long_2))



# =======================    Most recent 'sightings' for resighted / GPS tagged birds  =================

# customise filter as required to the release year and month you want to see if birds have been seen up until
# in this case, want to know who is known to be alive up to 1 year post-release (so has been seen since July of year following release)

# arrange sightings per bird by year and month and take the last date seen per bird (i.e. most recent sighting)
last_sighting <- dt_encounter_long_2 %>% 
  group_by(flag_id) %>% 
  arrange(release_year_month) %>% 
  slice(n())

# 2021 birds with most recent sighting at least 1 year post-release (from July of year following release)
last_sighting %>% 
  filter(year == 2021) %>% 
  mutate(release_year = substr(release_year_month, 1,4) %>% as.numeric,
         release_month = substr(release_year_month, 6,7) %>% as.numeric) %>% 
  filter(release_year >= 2022 & release_month >=7)

# 2022 birds with most recent sighting at least 1 year post-release (from July of year following release)
last_sighting %>% 
  filter(year == 2022) %>% 
  mutate(release_year = substr(release_year_month, 1,4) %>% as.numeric,
         release_month = substr(release_year_month, 6,7) %>% as.numeric) %>% 
  filter(release_year >= 2023 & release_month >=7)

